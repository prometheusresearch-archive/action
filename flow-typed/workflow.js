/**
 * @flow
 */

declare module 'workflow' {
  declare export opaque type Action<UI>;
  declare export opaque type Workflow<UI>;

  declare export opaque type ContextType;
  declare export type ContextShape = {[name: string]: ContextType};

  declare export type ContextValue = {value: any, type: ContextType};
  declare export type Context = {[name: string]: ContextValue};

  declare export type Query = string;
  declare export type DataSet = {[name: string]: any};

  declare export function number(number): ContextValue;
  declare export function string(string): ContextValue;
  declare export function entity(string, mixed): ContextValue;

  declare export var numberType: ContextType;
  declare export var stringType: ContextType;
  declare export function entityType(string): ContextType;

  declare type InteractionConfig<UI> = {
    requires: ContextShape,
    provides: ContextShape,
    query: Context => Query,
    ui: UI,
  };
  declare export function interaction<UI>(InteractionConfig<UI>): Action<UI>;

  declare type GuardConfig = {
    requires: ContextShape,
    query: Context => Query,
    check: (Context, DataSet) => boolean,
  };
  declare export function guard(GuardConfig): Action<*>;

  declare export function action<UI>(Action<UI>): Workflow<UI>;
  declare export function sequence(Array<Workflow<UI>>): Workflow<UI>;
  declare export function choice(Array<Workflow<UI>>): Workflow<UI>;

  declare export opaque type Frame<UI>;

  declare export type Config = {
    waitForData: Query => Promise<DataSet>,
  };

  declare export type Info<UI> = {
    context: Context,
    data: Data,
    ui: UI,
    prev: Array<{ui: UI, frame: Frame<UI>}>,
    next: Array<{ui: UI, frame: Frame<UI>}>,
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
