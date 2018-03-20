/**
 * @flow
 */

declare module 'workflow' {
  declare export type Result<T> =
    | {+type: 'Error', +error: string}
    | {+type: 'Ok', +value: T};

  declare export opaque type UI;
  declare export opaque type State;
  declare export opaque type Query;
  declare export opaque type UntypedQuery;
  declare export opaque type QueryResult;

  declare export var start: Result<State>;
  declare export function render(state: State): Result<{+ui: UI, +state: State}>;
  declare export function bind(
    query: UntypedQuery,
    state: State,
  ): Result<{+ui: UI, +state: State}>;
  declare export function next(state: State): Array<State>;

  declare export function getQuery(ui: UI, state: State): Query;
  declare export function runQuery(query: Query): Result<QueryResult>;
  declare export function pickValue(id: mixed): UntypedQuery;
}
