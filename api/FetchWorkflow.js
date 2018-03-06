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

export function create(params: Params) {
  const {db, workflow} = params;
  let fields = {};
  traverseAction(workflow, action => {
    if (action.type === 'GuardAction') {
      fields[action.id] = createFieldForGuard(params, action);
    } else if (action.type === 'QueryAction') {
      fields[action.id] = createFieldForQuery(params, action);
    }
  });

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

function createFieldForGuard({db}: Params, action: t.GuardAction) {
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
  return field;
}

function createFieldForQuery({db}: Params, action: t.QueryAction) {
  const args = {};
  for (const req of action.require) {
    args[req.name] = {
      type: new gqlType.GraphQLNonNull(gqlType.GraphQLString),
    };
  }

  const fields = {};
  for (const {type, query} of action.query) {
    fields[type.name] = {
      type: gqlType.GraphQLString,
      resolve: async root => {
        const compiled = compileQuery(query, root.args);
        const result = await db.query(compiled.query, compiled.values);
        const row = result.rows[0];
        if (row == null) {
          return null;
        } else {
          return row.result;
        }
      },
    };
  }

  return {
    resolve: (_, args) => ({args}),
    args,
    type: new gqlType.GraphQLObjectType({
      name: action.id,
      fields,
    }),
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

const emptyObjectFunction = () => ({});
