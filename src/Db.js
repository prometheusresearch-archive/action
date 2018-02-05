/**
 * @flow
 */

const pg = require('pg');
const mkDebug = require('debug');

export type Db = {
  client: pg.Client,
  query(query: string, values: Array<mixed>): Promise<pg.Result>,
  disconnect(): Promise<void>,
};

const debug = mkDebug('RexAction:Db');

export async function connect(config: pg.Config): Promise<Db> {
  const client = new pg.Client(config);
  await client.connect();

  function query(query, values) {
    debug(query);
    return client.query(query, values);
  }
  function disconnect() {
    return client.end();
  }
  return {
    client,
    query,
    disconnect,
  };
}
