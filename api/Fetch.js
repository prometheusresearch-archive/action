/**
 * This module construct a data fetch endpoint based on a type universe.
 *
 * @flow
 */

const invariant = require('invariant');
const pg = require('pg');
const DataLoader = require('dataloader');
const makeDebug = require('debug');

import * as t from './types.js';
import * as gql from 'graphql';
import * as gqlType from 'graphql/type';
import * as Db from './Db.js';

const debug = makeDebug('RexAction:Fetch');

type Params = {
  univ: t.Universe,
  db: Db.Db,
};

const entityMetaField = {
  type: new gqlType.GraphQLObjectType({
    name: '_EntityMeta',
    fields: {
      title: {type: new gqlType.GraphQLNonNull(gqlType.GraphQLString)},
    },
  }),
  resolve(data) {
    const meta = {};

    const FIELDS_AS_TITLE = ['title', 'name', 'code'];
    for (const name of FIELDS_AS_TITLE) {
      if (data[name] != null && typeof data[name] != 'object') {
        meta.title = String(data[name]);
        break;
      }
    }

    if (meta.title == null) {
      meta.title = String(data.id);
    }

    return meta;
  },
};

export function create(params: Params) {
  function makeLoaderBySingleColumn(table, column) {
    return new DataLoader(async vals => {
      const inQuery = vals.map((_, idx) => `$${idx + 1}`).join(', ');
      const query = `
        select *
        from "${table.name}"
        where
          "${table.name}"."${column.name}" IN (${inQuery})
      `;
      const res = await params.db.query(query, vals);
      return res.rows;
    });
  }

  function makeLoaderByMultipleColumns(table, columns) {
    return new DataLoader(async vals => {
      const columnsQuery = columns.map(c => `"${table.name}"."${c.name}"`).join(', ');
      const inQuery = vals.map((_, idx) => `$${idx + 1}`).join(', ');
      const query = `
        select *
        from "${table.name}"
        where
          (${columnsQuery}) IN (${inQuery})
      `;
      const res = await params.db.query(query, vals);
      return res.rows;
    });
  }

  function makeLoaderForConstraint(loaders, table, constraint) {
    const columns = Array.from(constraint.columns.values());
    if (columns.length === 1) {
      const column = columns[0];
      loaders.set(column.name, makeLoaderBySingleColumn(table, column));
    } else {
      loaders.set(
        columns.map(c => c.name).join('--'),
        makeLoaderByMultipleColumns(table, columns),
      );
    }
  }

  // pregenerate loaders to fetch db entities by pks and fks
  // TODO: maybe we should generate a loader by constraint
  const loaders: Map<string, Map<string, DataLoader<*, *>>> = new Map();
  for (const name in params.univ.entity) {
    const entity = params.univ.entity[name];
    const table = entity.table;
    const loadersByColumn = new Map();
    loaders.set(name, loadersByColumn);

    // constraints
    for (const constraint of table.constraints.values()) {
      switch (constraint.type) {
        case 'UNIQUE':
          makeLoaderForConstraint(loadersByColumn, table, constraint);
        case 'PRIMARY KEY':
          makeLoaderForConstraint(loadersByColumn, table, constraint);
        case 'FOREIGN KEY':
          makeLoaderForConstraint(loadersByColumn, table, constraint);
      }
    }
  }

  function getLoaderByTableColumn(tableName: string, columnName: string) {
    const loadersByColumn = loaders.get(tableName);
    invariant(
      loadersByColumn != null,
      'Cannot resolve loader for table: "%s"',
      tableName,
    );
    const loader = loadersByColumn.get(columnName);
    invariant(
      loader != null,
      'Cannot resolve loader for column: "%s.%s"',
      tableName,
      columnName,
    );
    return loader;
  }

  function loadEntityByConstraint(table, constraint, value) {
    const columns = Array.from(constraint.referencedColumnsBy.values());
    const loader = getLoaderByTableColumn(
      table.name,
      columns.map(c => c.name).join('--'),
    );
    if (value.length === 1) {
      value = value[0];
    }
    return loader.load(value);
  }

  function createEntityType(params: Params, entity: t.EntityType) {
    const fields = () => {
      const fields = {};
      for (const fieldName in entity.fields) {
        const field = entity.fields[fieldName];
        const type = createEntityFieldType(params, entity, field);
        const resolve = createEntityFieldResolver(params, entity, field);
        fields[fieldName] = {
          type,
          resolve,
        };
        // TODO: add fieldName__list field for sequences
      }

      fields._meta = entityMetaField;

      return fields;
    };

    return new gql.GraphQLObjectType({name: entity.name, fields});
  }

  function createEntityFieldType(params: Params, entity: t.EntityType, field) {
    const {kind} = field;
    switch (field.kind) {
      case 'Relation': {
        const {kind, type} = field;
        if (type.type === 'EntityType') {
          const gType = fields[type.table.name].type;
          return gType;
        } else if (type.type === 'ListType') {
          const gType = fields[type.child.table.name].type;
          return new gqlType.GraphQLList(gType);
        } else {
          invariant(false, 'Invalid Relation');
        }
      }
      default:
        // TODO: implement a proper mapping
        return gqlType.GraphQLString;
    }
  }

  const {univ} = params;
  const fields = {};

  for (const name in univ.entity) {
    const entity = univ.entity[name];
    const type = createEntityType(params, entity);
    const resolve = createEntityResolver(params, entity);
    const resolveList = createEntityListResolver(params, entity);
    if (entity.fields.id != null) {
      fields[name] = {
        type,
        resolve,
        args: {
          id: {
            type: new gqlType.GraphQLNonNull(typeToGraphQLType(entity.fields.id.type)),
          },
        },
      };
    }
    fields[name + '__list'] = {
      type: new gqlType.GraphQLList(type),
      resolve: resolveList,
      args: {
        offset: {type: gqlType.GraphQLInt, defaultValue: 0},
        limit: {type: gqlType.GraphQLInt, defaultValue: 50},
      },
    };
  }

  function createEntityResolver(params: Params, entity: t.EntityType) {
    async function resolve(_root, args) {
      const loader = getLoaderByTableColumn(entity.table.name, 'id');
      return loader.load(args.id);
    }
    return resolve;
  }

  function createEntityListResolver(params: Params, entity: t.EntityType) {
    async function resolve(root, args) {
      const query = `select * from "${entity.table.name}" limit $1 offset $2`;
      const result = await params.db.query(query, [args.limit, args.offset]);
      return result.rows;
    }
    return resolve;
  }

  function createEntityFieldResolver(params, entityType: t.EntityType, field: t.Field) {
    async function resolve(entity) {
      const {kind} = field;
      switch (field.kind) {
        case 'Relation': {
          const {relation} = field;
          switch (relation.type) {
            case 'MANY TO ONE': {
              const {constraint, targetTable} = relation;
              const values = [];
              for (const columnName of constraint.referencedColumnsBy.keys()) {
                values.push(entity[columnName]);
              }
              return loadEntityByConstraint(targetTable, constraint, values);
            }
            case 'ONE TO MANY': {
              const {constraint, targetTable, sourceTable} = relation;
              const where = [];
              const values = [];
              for (const [
                columnName,
                referencedColumn,
              ] of constraint.referencedColumnsBy.entries()) {
                const target = `"${targetTable.name}"."${columnName}"`;
                where.push(`${target} = $${values.length + 1}`);
                values.push(entity[referencedColumn.name]);
              }
              const whereQuery = where.join(' AND ');
              const query = `select * from "${targetTable.name}" where ${whereQuery}`;
              const result = await params.db.query(query, values);
              return result.rows;
            }
            default:
              invariant(false, 'Not supported relation type: %s', field.relation.type);
          }
        }
        default:
          return entity[field.name];
      }
    }
    return resolve;
  }

  const query = new gql.GraphQLObjectType({name: 'query', fields});
  const schema = new gql.GraphQLSchema({query});
  return schema;
}

function typeToGraphQLType(type: t.Type) {
  switch (type.type) {
    case 'StringType':
      return gqlType.GraphQLString;
    case 'IntegerType':
      return gqlType.GraphQLInt;
    case 'BooleanType':
      return gqlType.GraphQLInt;
    default:
      // TODO: handle more types
      return gqlType.GraphQLString;
  }
}
