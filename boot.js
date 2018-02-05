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
import * as Db from './src/Db.js';

loudRejection();

const settings = {
  workflowConfig: './boot.yaml',
  db: {database: 'study_demo'},
};

async function query(settings, query) {
  const db: Db.Db = await Db.connect(settings.db);

  try {
    const univ = Universe.create();
    await ReflectDB.reflect(univ, db.client);
    const workflow = await Config.configureOf(univ, settings.workflowConfig);
    const schema = Fetch.create({univ: workflow.univ, db});

    const response = await gql.graphql(schema, query, null);
    console.log(JSON.stringify(response, null, 2));
  } finally {
    await db.disconnect();
  }
}

query(settings, process.argv[2]);
