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
import {type Workflow, traverseAction} from './ConfigWorkflow.js';

type Params = {
  workflow: Workflow,
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

    for (const name in action.query) {
      fields[action.id + '__' + name] = {
        type: gqlType.GraphQLString,
        resolve: async (_root, args) => {
          const {query, values} = compileQuery(action.query[name], args);
          const result = await db.query(query, values);
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
    if (action.type === 'guard') {
      fields = {...fields, ...createFieldForGuard(action)};
    } else if (action.type === 'query') {
      fields = {...fields, ...createFieldForQuery(action)};
    }
  });

  return {
    _workflow: {
      resolve: () => ({}),
      type: new gql.GraphQLObjectType({
        name: '_workflow',
        fields,
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
