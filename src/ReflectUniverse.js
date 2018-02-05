/**
 * @flow
 */

const invariant = require('invariant');
const PgStructure = require('pg-structure');

import * as gql from 'graphql';
import * as t from './types.js';

/**
 * Reflect entities from database into the universe.
 */
export async function reflect(
  univ: t.Universe,
  connection: PgStructure.Connection,
): Promise<void> {
  function reflectTableToEntity(table: PgStructure.Table): t.EntityType {
    const fields = {};
    for (const column of table.columns.values()) {
      fields[column.name] = reflectColumnToField(column);
    }
    for (const rel of table.o2mRelations.values()) {
      const name = rel.generateName();
      fields[name] = reflectO2MRelationToField(rel);
    }
    for (const rel of table.m2oRelations.values()) {
      const name = rel.generateName();
      fields[name] = reflectM2ORelationToField(rel);
    }
    return {
      type: 'EntityType',
      name: table.name,
      fields,
      table,
    };
  }

  function reflectO2MRelationToField(rel: PgStructure.O2MRelation): t.Field {
    // entity might not be reflected yet, store type as a thunk we force later
    const type: any = () => {
      const entityType = entity[rel.targetTable.name];
      return {type: 'ListType', child: entityType};
    };
    const name = rel.generateName();
    return {kind: 'Relation', type, name, relation: rel};
  }

  function reflectM2ORelationToField(rel: PgStructure.M2ORelation): t.Field {
    // entity might not be reflected yet, store type as a thunk we force later
    const type: any = () => {
      const entityType = entity[rel.targetTable.name];
      return entityType;
    };
    const name = rel.generateName();
    return {kind: 'Relation', type, name, relation: rel};
  }

  function reflectColumnToField(column: PgStructure.Column): t.Field {
    // TODO: handle other types
    const type = {type: 'StringType'};
    return {kind: 'Attribute', type, name: column.name};
  }

  function resolveEntityType(name) {
    return () => entity[name];
  }

  const schemaName = 'public';

  const db = await PgStructure(connection, [schemaName], {});
  const schema = db.schemas.get(schemaName);
  invariant(schema != null, 'Unable to reflect database schema');

  const entity: {[name: string]: t.EntityType} = {};
  for (const table of schema.tables.values()) {
    entity[table.name] = reflectTableToEntity(table);
  }

  // force all fields thunks
  for (const name in entity) {
    const e = entity[name];
    for (const name in e.fields) {
      const f = e.fields[name];
      if (typeof f.type === 'function') {
        f.type = f.type();
      }
    }
  }

  univ.entity = {...univ.entity, ...entity};
}
