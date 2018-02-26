/**
 * @flow
 */

import * as express from 'express';
const graphqlHTTP = require('express-graphql');
const loudRejection = require('loud-rejection');
const invariant = require('invariant');

import * as pg from 'pg';
import * as gql from 'graphql';

import * as Universe from './Universe.js';
import * as ReflectDB from './ReflectUniverse.js';
import * as Config from './Config.js';
import * as Fetch from './Fetch.js';
import * as FetchWorkflow from './FetchWorkflow.js';
import * as Db from './Db.js';

loudRejection();

type Settings = {
  workflowConfig: string,
  db: pg.Config,
};

const settings = {
  workflowConfig: './boot.yaml',
  db: {database: 'study_demo'},
};

async function loadWorkflow(db: Db.Db, settings: Settings) {
  const univ = Universe.create();
  await ReflectDB.reflect(univ, db.client);
  const workflow = await Config.configureOf(univ, settings.workflowConfig);
  const dataFields = Fetch.create({univ: workflow.univ, db});
  const workflowFields = FetchWorkflow.create({workflow: workflow.workflow, db});
  const query = new gql.GraphQLObjectType({
    name: 'query',
    fields: {
      ...dataFields,
      ...workflowFields,
    },
  });
  const graphqlSchema = new gql.GraphQLSchema({query});
  return {graphqlSchema, workflow, univ};
}

async function query(settings, query) {
  const db: Db.Db = await Db.connect(settings.db);
  try {
    const {graphqlSchema} = await loadWorkflow(db, settings);
    const response = await gql.graphql(graphqlSchema, query, null);
    console.log(JSON.stringify(response, null, 2));
  } finally {
    await db.disconnect();
  }
}

async function serveWorkflow(settings) {
  const db: Db.Db = await Db.connect(settings.db);
  const {graphqlSchema} = await loadWorkflow(db, settings);

  const router = new express.Router();

  router.get('/', async (req: express.$Request, res, next) => {
    const yep = await 'hello';
    res.send(yep);
  });

  router.use(
    '/graphql',
    graphqlHTTP({
      schema: graphqlSchema,
      graphiql: true,
    }),
  );

  router.get('/graphql', async (req: express.$Request, res, next) => {
    const yep = await 'hello';
    res.send(yep);
  });

  const app = express.default();
  app.use(catchAllErrors);
  app.use(allowCrossOrigin);
  app.use(router);

  const port = 3001;
  app.listen(port, () => console.log(`Server running on localhost:${port}`));
}

const catchAllErrors: express.Middleware = (
  req: express.$Request,
  res: express.$Response,
  next: express.NextFunction,
) => {
  Promise.reject().catch(next);
};

const allowCrossOrigin: express.Middleware = (
  req: express.$Request,
  res: express.$Response,
  next: express.NextFunction,
) => {
  res.header('Access-Control-Allow-Origin', '*');
  res.header(
    'Access-Control-Allow-Headers',
    'Origin, X-Requested-With, Content-Type, Accept',
  );
  next();
};

serveWorkflow(settings).catch(err => {
  console.log(err.stack);
  process.exit(1);
});
//query(settings, process.argv[2]);
