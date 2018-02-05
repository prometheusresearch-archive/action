/**
 * This module construct a data fetch endpoint based on a type universe.
 *
 * @flow
 */

const invariant = require('invariant');
const pg = require('pg');
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

export function create(params: Params) {
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
    fields[name] = {
      type,
      resolve,
      args: {
        id: {type: new gqlType.GraphQLNonNull(gqlType.GraphQLString)},
      },
    };
    fields[name + '__list'] = {
      type: new gqlType.GraphQLList(type),
      resolve: resolveList,
      args: {
        offset: {type: gqlType.GraphQLInt, defaultValue: 0},
        limit: {type: gqlType.GraphQLInt, defaultValue: 50},
      },
    };
  }

  const query = new gql.GraphQLObjectType({name: 'query', fields});
  const schema = new gql.GraphQLSchema({query});
  return schema;
}

function createEntityResolver(params: Params, entity: t.EntityType) {
  async function resolve(_root, args) {
    const query = `select * from "${entity.table.name}" where id = $1`;
    const result = await params.db.query(query, [args.id]);
    return result.rows[0];
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
            const {constraint, targetTable, sourceTable} = relation;
            const where = [];
            const values = [];
            for (const [
              columnName,
              referencedColumn,
            ] of constraint.referencedColumnsBy.entries()) {
              const target = `"${targetTable.name}"."${referencedColumn.name}"`;
              where.push(`${target} = $${values.length + 1}`);
              values.push(entity[columnName]);
            }
            const whereQuery = where.join(' AND ');
            const query = `select * from "${targetTable.name}" where ${whereQuery}`;
            const result = await params.db.query(query, values);
            return result.rows[0];
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
