/**
 * @flow
 */

import invariant from 'invariant';
import {
  record,
  string,
  mapping,
  mappingGeneral,
  sequence,
  lazy,
  constant,
  recursive,
  switchCase,
  any,
  hasFieldOfType,
  onSequence,
  onScalar,
  onMapping,
  ParseError,
  parseWith,
  parseStringWith,
  Node,
  ScalarNode,
  getMappingNodeValueNodeByKey,
  type Parser as ParserBase,
} from './Validate.js';

export type Action = GuardAction | PickAction | ViewAction | MakeAction;

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
};

export type PickAction = {
  type: 'PickAction',
  id: string,
  entity: string,
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

type ParserContext = {
  id: string,
  createActionReference(string): ?WorkflowRef,
};

type Parser<V> = ParserBase<V, ParserContext>;

const parseAction: Parser<WorkflowAction> = lazy().refine((pos, action) => {
  const typeNode = getMappingNodeValueNodeByKey('type', action.node);
  if (typeNode == null) {
    pos.error('missing "type" attribute');
    invariant(false, 'impossible');
  } else if (!(typeNode instanceof ScalarNode)) {
    pos.error('"type" attribute should be a scalar');
    invariant(false, 'impossible');
  } else {
    const type = typeNode.value;
    const parseAction = parseActionByType[type];
    if (parseAction == null) {
      pos.error(`unknown action found "${type}"`);
      invariant(false, 'impossible');
    }
    const parsedAction = action.parseWith({}, parseAction);
    return {
      type: 'WorkflowAction',
      action: {...parsedAction, id: sanitizeId(pos.context.id)},
    };
  }
});

const parseActionRef: Parser<WorkflowRef> = string().refine((pos, value, node) => {
  const action = pos.context.createActionReference(value);
  if (action == null) {
    throw new ParseError(`unable to resolve action ${value}`, pos);
  }
  return action;
});

const parseWorkflowNode = recursive(parseWorkflowNode => {
  const parseChoice: Parser<WorkflowNode> = sequence(parseWorkflowNode).refine(
    (_ctx, choice) => {
      if (choice.length === 1) {
        return choice[0];
      } else {
        return {type: 'WorkflowChoice', choice};
      }
    },
  );
  const parseSequence = mappingGeneral({
    keyParser: parseActionRef,
    valueParser: parseWorkflowNode,
    initial: [],
    build: (sequence, k, v) => sequence.concat(k).concat(v),
  }).refine((_ctx, items) => {
    if (items.length === 1) {
      return items[0];
    } else {
      const sequence = [];
      for (const item of items) {
        if (item.type === 'WorkflowSequence') {
          sequence.push(...item.sequence);
        } else {
          sequence.push(item);
        }
      }
      return {type: 'WorkflowSequence', sequence};
    }
  });
  return switchCase(
    [onSequence, parseChoice],
    [onMapping, parseSequence],
    [onScalar, parseActionRef],
  );
});

const parseWorkflowNodeOrAction = switchCase(
  [hasFieldOfType('type', string()), parseAction],
  [true, parseWorkflowNode],
);

const parseWorkflow: ParserBase<Workflow> = mapping(lazy()).refine((ctx, rules, node) => {
  function createActionReference(ref: string) {
    const rule = rules[ref];
    if (rule == null) {
      return null;
    } else {
      return {type: 'WorkflowRef', ref};
    }
  }
  function createContext(id) {
    const context: ParserContext = {
      id,
      createActionReference,
    };
    return context;
  }
  const nodes = {};
  for (const id in rules) {
    nodes[id] = rules[id].parseWith(createContext(id), parseWorkflowNodeOrAction);
  }
  const start = createActionReference('start');
  if (start == null) {
    ctx.error('missing "start" workflow');
    invariant(false, 'impossible');
  }
  return {
    start,
    nodes,
  };
});

const parseWorkflowFromConfig = record(
  {workflow: parseWorkflow},
  {ignoreUnknownFields: true},
);

export function parse(node: Node) {
  return parseWith({}, parseWorkflowFromConfig, node);
}

export function parseString(v: string) {
  return parseStringWith({}, parseWorkflowFromConfig, v);
}

export function traverseAction(workflow: Workflow, f: Action => void) {
  const {start, nodes} = workflow;
  const queue = [start];
  while (queue.length > 0) {
    const w = queue.shift();
    switch (w.type) {
      case 'WorkflowRef':
        queue.push(nodes[w.ref]);
        break;
      case 'WorkflowAction':
        f(w.action);
        break;
      case 'WorkflowSequence':
        queue.push(...w.sequence);
        break;
      case 'WorkflowChoice':
        queue.push(...w.choice);
        break;
    }
  }
}

const parseContextShape: Parser<ContextShape> = sequence(
  switchCase(
    [onScalar, string().refine((_pos, value) => ({name: value, type: value}))],
    [
      onMapping,
      mapping(string()).refine((_pos, items) => {
        const reqs = [];
        for (const name in items) {
          reqs.push({name, type: items[name]});
        }
        return reqs;
      }),
    ],
  ),
);

const parseGuard = record({
  type: constant('guard'),
  require: parseContextShape,
  query: string(),
});

const parseQuery = record({
  type: constant('query'),
  require: parseContextShape,
  query: mapping(string()),
});

const parsePick = record({
  type: constant('pick'),
  entity: string(),
});

const parseView = record({
  type: constant('view'),
  entity: string(),
});

const parseMake = record({
  type: constant('make'),
  entity: string(),
});

const parseEdit = record({
  type: constant('edit'),
  entity: string(),
});

const parseActionByType = {
  view: parseView,
  make: parseMake,
  pick: parsePick,
  guard: parseGuard,
  query: parseQuery,
};

function sanitizeId(id) {
  return id.replace(/_/g, '__').replace(/-/g, '_');
}
