/**
 * @flow
 */

import * as PgStructure from 'pg-structure';
import * as React from 'react';

/**
 * Universe is a collection of entities present in an application.
 */
export type Universe = {
  entity: {
    [name: string]: EntityType,
  },
};

/**
 * Types.
 */

export type StringType = {
  type: 'StringType',
};

export type IntegerType = {
  type: 'IntegerType',
};

export type BooleanType = {
  type: 'BooleanType',
};

export type DateType = {
  type: 'DateType',
};

export type DateTimeType = {
  type: 'DateTimeType',
};

export type ListType = {
  type: 'ListType',
  child: Type,
};

export type Attribute = {
  kind: 'Attribute',
  type: Type,
  name: string,
};

export type Relation = {
  kind: 'Relation',
  type: EntityType,
  name: string,

  // TODO: need db-specific type instead
  relation: PgStructure.Relation,
};

export type Field = Attribute | Relation;

export type RecordType = {
  type: 'RecordType',
  fields: {
    [name: string]: Field,
  },
};

export type EntityType = {
  type: 'EntityType',
  name: string,
  fields: {
    [name: string]: Field,
  },

  // TODO: need db-specific type instead
  table: PgStructure.Table,
};

export type Type =
  | StringType
  | IntegerType
  | BooleanType
  | DateType
  | DateTimeType
  | EntityType
  | ListType
  | RecordType;

export type TypeImage =
  | StringType
  | IntegerType
  | BooleanType
  | DateType
  | DateTimeType
  | EntityTypeImage
  | ListTypeImage;

export type EntityTypeImage = {
  type: 'EntityType',
  name: string,
};

export type ListTypeImage = {
  type: 'ListType',
  child: TypeImage,
};

/**
 * Typed value.
 */
export type Value = SimpleValue | EntityValue;

export type SimpleValue = {
  type: Type,
  value: mixed,
};

export opaque type EntityID = string;

export type EntityValue = {
  type: EntityType,
  id: EntityID,
  fields: {[name: string]: Value},
};

/**
 * A stack of sets of values.
 */
type Context = {
  prev: ?Context,
  bindings: {[name: string]: Value},
};

type ActionProps = {
  context: Context,
};

/**
 * Action is something which performs state transitions.
 */
type Action = UserAction | ProcessAction;

/**
 * User actions perform state transitions through user input.
 */
type UserAction = {
  requires: Array<[string, Type]>,
  render(props: ActionProps): React.Element<*>,
};

/**
 * Process actions perform state transition through automated processes.
 */
type ProcessAction = {
  requires: Array<[string, Type]>,
  perform(props: ActionProps): Promise<StateTransition>,
};

/**
 * State of the workflow is the current action is the stack of current action
 * and current context.
 */
type State = {
  prev: ?State,
  context: Context,
  action: Action,
};

type StateTransition = RunAction | SetContextValue | UpdateEntity;

type RunAction = {
  type: 'RunAction',
  action: Action,
};

type SetContextValue = {
  type: 'SetContextValue',
  key: string,
  value: Value,
};

type UpdateEntity = {
  type: 'UpdateEntity',
  entity: EntityValue,
};

/**
 * This is the workflow — a collection of UI actions over the universe.
 */
export type Workflow = {
  /*
   * Workflow's own universe.
   */
  univ: Universe,
  workflow: {
    [name: string]: ActionConfig,
  },
};

export type ActionConfig = TerminalActionConfig | NonTerminalActionConfig;
type NonTerminalActionConfig = Array<ActionConfig>;
type TerminalActionConfig = {type: string};

/**
 * Perform action and wait for a state transition.
 */
declare function runAction(state: State): Promise<StateTransition>;

/**
 * Perform a state transition.
 */
declare function runTransition(state: State, transition: StateTransition): Promise<State>;

/**
 * Render state into UI.
 */
declare function render(state: State): React.Element<*>;
