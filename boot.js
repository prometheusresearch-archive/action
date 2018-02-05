/**
 * @flow
 */

const loudRejection = require('loud-rejection');
const invariant = require('invariant');

import * as pg from 'pg';
import * as gql from 'graphql';

import * as Universe from './src/Universe.js';
import * as ReflectDB from './src/ReflectUniverse.js';
import * as Config from './src/Config.js';
import * as Fetch from './src/Fetch.js';

loudRejection();

const settings = {
  workflowConfig: './boot.yaml',
  db: {database: 'study_demo'},
};

async function query(settings, query) {
  const db = new pg.Client(settings.db);
  await db.connect();

  const univ = Universe.create();
  await ReflectDB.reflect(univ, db);
  const workflow = await Config.configureOf(univ, settings.workflowConfig);
  const schema = Fetch.create({univ: workflow.univ, db});

  const response = await gql.graphql(schema, query, null);
  console.log(JSON.stringify(response, null, 2));
  await db.end();
}

query(settings, process.argv[2]);
