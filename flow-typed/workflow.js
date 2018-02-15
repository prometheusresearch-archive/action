/**
 * @flow
 */

declare module 'workflow' {
  /***************************************************************
   * Core types to model UI state and application data.
   ***************************************************************/

  /**
   * ContextShape describes the context value at some point in time.
   *
   * It can be either used to describe what is required from the context or to
   * validate what's being inserted into the context.
   */
  declare export type ContextShape = {[name: string]: ContextType};
  declare export opaque type ContextType;

  /**
   * Utility functions to produce context type specifications.
   */
  declare export var numberType: ContextType;
  declare export var stringType: ContextType;
  declare export function entityType(string): ContextType;

  /**
   * Context represents a type bag of k-v pairs.
   *
   * Actions are being composed by specifying their expectation for the context
   * values.
   */
  declare export type Context = {[name: string]: ContextValue};
  declare export opaque type ContextValue;

  /**
   * Utility functions to produce context values annotated with types.
   */
  declare export function number(number): ContextValue;
  declare export function string(string): ContextValue;
  declare export function entity(string, mixed): ContextValue;

  /**
   * GraphQL query.
   *
   * TODO: For now it is represented as a string but in the future we might want
   * to accept some kind of AST for that.
   */
  declare export type Query = string;

  /**
   * DataSet fetched by the workflow engine.
   */
  declare export type DataSet = {[name: string]: any};

  /***************************************************************
   * Action configuration
   ***************************************************************/

  /**
   * Primitive actions are presented by this type.
   *
   * There are several kinds of primitives actions which can be constructed by
   * functions below.
   */
  declare export opaque type Action<+UI>;

  /**
   * Interactions are UI screens which expect some UI input.
   */
  declare type InteractionConfig<UI> = {
    /**
     * What's required from context. If those requirements are not satisifed
     * then the action won't be run.
     */
    requires: ContextShape,

    provides: ContextShape,

    query: Context => Query,
    queryTitle: Context => ?Query,
    ui: UI,
  };
  declare export function interaction<UI>(InteractionConfig<UI>): Action<UI>;

  /**
   * Guards allow to cut branches of the workflow by checking for some specific
   * condition.
   */
  declare type GuardConfig = {
    requires: ContextShape,
    query: Context => Query,
    check: (Context, DataSet) => boolean,
  };
  declare export function guard(GuardConfig): Action<*>;

  /**
   * Queries allow to update context with some data fetched from the database.
   */
  declare type QueryConfig = {
    requires: ContextShape,
    provides: ContextShape,
    query: Context => Query,
    update: (Context, DataSet) => Context,
  };
  declare export function query(QueryConfig): Action<*>;

  /***************************************************************
   * Workflow configuration
   ***************************************************************/

  /**
   * Workflow is a composition of primives actions which represent some control
   * flow.
   */
  declare export opaque type Workflow<+UI>;

  /**
   * Make workflow out of a primitive action.
   */
  declare export function action<UI>(Action<UI>): Workflow<UI>;

  /**
   * Sequential composition of actions.
   *
   * Actions are being executed one by one.
   */
  declare export function sequence<UI>(Array<Workflow<UI>>): Workflow<UI>;

  /**
   * Parallel composition of actions.
   *
   * User can proceed with any action of the provided.
   */
  declare export function choice<UI>(Array<Workflow<UI>>): Workflow<UI>;

  /***************************************************************
   * Execution API
   *
   * This API should be used by implementors of workflow UIs.
   ***************************************************************/

  declare export opaque type Frame<UI>;

  declare export type Config = {
    waitForData: Query => Promise<DataSet>,
  };

  declare export type LimitedInfo<UI> = {
    context: Context,
    ui: UI,
    frame: Frame<UI>,
    dataTitle: ?DataSet,
  };

  declare export type Info<UI> = {
    context: Context,
    data: DataSet,
    dataTitle: DataSet,
    ui: UI,
    prev: Array<LimitedInfo<UI>>,
    next: Array<LimitedInfo<UI>>,
  };

  declare export function init<UI>(Workflow<UI>): Frame<UI>;

  declare export function runToInteraction<UI>(
    config: Config,
    frame: Frame<UI>,
  ): Promise<{info: ?Info<UI>, frame: Frame<UI>}>;

  declare export function nextToInteraction<UI>(
    config: Config,
    context: Context,
    frame: Frame<UI>,
  ): Promise<{info: ?Info<UI>, frame: Frame<UI>}>;
}
