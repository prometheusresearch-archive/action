/**
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
import {traverseAction} from './ConfigWorkflow.js';

type Params = {
  workflow: t.Workflow,
  db: Db.Db,
};

export function create({db, workflow}: Params) {
  function createFieldForGuard(action) {
    const args = {};
    for (const req of action.require) {
      args[req.name] = {
        type: new gqlType.GraphQLNonNull(gqlType.GraphQLString),
      };
    }
    const field = {
      type: gqlType.GraphQLBoolean,
      resolve: async (_root, args) => {
        const {query, values} = compileQuery(action.query, args);
        const result = await db.query(query, values);
        const row = result.rows[0];
        if (row == null) {
          return false;
        } else {
          return row.result;
        }
      },
      args,
    };
    return {[action.id]: field};
  }

  function createFieldForQuery(action) {
    const args = {};
    for (const req of action.require) {
      args[req.name] = {
        type: new gqlType.GraphQLNonNull(gqlType.GraphQLString),
      };
    }
    const fields = {};

    for (const {type, query} of action.query) {
      fields[action.id + '__' + type.name] = {
        type: gqlType.GraphQLString,
        resolve: async (_root, args) => {
          const compiled = compileQuery(query, args);
          const result = await db.query(compiled.query, compiled.values);
          const row = result.rows[0];
          if (row == null) {
            return null;
          } else {
            return row.result;
          }
        },
        args,
      };
    }
    return fields;
  }

  let fields = {};
  traverseAction(workflow, action => {
    if (action.type === 'GuardAction') {
      fields = {...fields, ...createFieldForGuard(action)};
    } else if (action.type === 'QueryAction') {
      fields = {...fields, ...createFieldForQuery(action)};
    }
  });

  const emptyObjectFunction = () => ({});

  // FIXME: shouldn't be polymorphic
  const values =
    Object.keys(fields).length > 0
      ? new gqlType.GraphQLObjectType({
          name: '_workflowValues',
          fields,
        })
      : RawType;

  return {
    _workflow: {
      resolve: emptyObjectFunction,
      type: new gql.GraphQLObjectType({
        name: '_workflow',
        resolve: emptyObjectFunction,
        fields: {
          values: {
            resolve: emptyObjectFunction,
            type: values,
          },
          workflow: {
            resolve: () => workflow,
            type: RawType,
          },
        },
      }),
    },
  };
}

function compileQuery(query, args) {
  let idx = 0;
  const values = [];
  const compiledQuery = query.replace(/\$([a-zA-Z_]+)/g, (_, name) => {
    idx = idx + 1;
    invariant(args[name] != null, 'Invalid query');
    values.push(args[name]);
    return '$' + String(idx);
  });
  return {query: compiledQuery, values};
}

const RawType = new gqlType.GraphQLScalarType({
  name: 'Raw',
  serialize(value) {
    return value;
  },
});
