/**
 * @flow
 */

import type {Relation as PgRelation, Table as PgTable} from 'pg-structure';

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
  relation: PgRelation,
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
  table: PgTable,
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

export type Action = QueryAction | GuardAction | PickAction | ViewAction | MakeAction;

export type ContextShape = Array<{name: string, type: string}>;

export type GuardAction = {
  type: 'GuardAction',
  id: string,
  require: ContextShape,
  query: string,
};

export type MakeAction = {
  type: 'MakeAction',
  id: string,
  entity: string,
};

export type ViewAction = {
  type: 'ViewAction',
  id: string,
  entity: string,
  title: string,
  fields: Array<string>,
};

export type EditAction = {
  type: 'EditAction',
  id: string,
  entity: string,
};

export type PickAction = {
  type: 'PickAction',
  id: string,
  entity: string,
  title: string,
  fields: Array<string>,
};

export type QueryAction = {
  type: 'QueryAction',
  id: string,
  require: ContextShape,
  query: Array<{
    type: {type: string, name: string},
    query: string,
  }>,
};

export type WorkflowSequence = {
  type: 'WorkflowSequence',
  sequence: Array<WorkflowNode>,
};

export type WorkflowChoice = {
  type: 'WorkflowChoice',
  choice: Array<WorkflowNode>,
};

export type WorkflowAction = {
  type: 'WorkflowAction',
  action: Action,
};

export type WorkflowRef = {
  type: 'WorkflowRef',
  ref: string,
};

export type WorkflowNode =
  | WorkflowAction
  | WorkflowRef
  | WorkflowSequence
  | WorkflowChoice;

export type WorkflowNodeOrAction = WorkflowNode | WorkflowAction;

export type Workflow = {
  start: WorkflowNode,
  nodes: {[id: string]: WorkflowNode},
};
