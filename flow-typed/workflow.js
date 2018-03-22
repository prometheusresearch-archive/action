/**
 * @flow
 */

declare module 'workflow' {
  declare export type Result<T> =
    | {+type: 'Error', +error: string}
    | {+type: 'Ok', +value: T};

  declare export opaque type UI;
  declare export opaque type State;
  declare export opaque type QueryResult;

  declare export var start: Result<{+ui: UI, +state: State}>;
  declare export function renderState(state: State): Result<{+ui: UI, +state: State}>;
  declare export function next(state: State): Array<State>;

  declare export function getData(state: State): QueryResult;
  declare export function getTitle(state: State): QueryResult;

  declare export function pickValue(
    id: mixed,
    state: State,
  ): Result<{+ui: UI, +state: State}>;

  declare export function uiName(ui: UI): string;
  declare export function breadcrumbs(State): Array<State>;
}
