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
import * as t from './types.js';

type ParserContext = {
  id: string,
  createActionReference(string): ?t.WorkflowRef,
};

type Parser<V> = ParserBase<V, ParserContext>;

const parseAction: Parser<t.WorkflowAction> = lazy().refine((pos, action) => {
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
    const id = sanitizeId(pos.context.id);
    const parsedAction = action.parseWith({id}, parseAction);
    return {
      type: 'WorkflowAction',
      action: parsedAction,
    };
  }
});

const parseActionRef: Parser<t.WorkflowRef> = string().refine((pos, value, node) => {
  const action = pos.context.createActionReference(value);
  if (action == null) {
    throw new ParseError(`unable to resolve action ${value}`, pos);
  }
  return action;
});

const parseWorkflowNode: Parser<t.WorkflowNode> = recursive(parseWorkflowNode => {
  const parseChoice: Parser<t.WorkflowNode> = sequence(parseWorkflowNode).refine(
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

const parseWorkflowNodeOrAction: Parser<t.WorkflowNodeOrAction> = switchCase(
  [hasFieldOfType('type', string()), parseAction],
  [true, parseWorkflowNode],
);

const parseWorkflow: ParserBase<t.Workflow> = mapping(lazy()).refine(
  (ctx, rules, node) => {
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
  },
);

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

export function traverseAction(workflow: t.Workflow, f: t.Action => void) {
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

const parseType = switchCase(
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
);

const parseContextShape: Parser<t.ContextShape> = sequence(parseType);

const parseGuard: Parser<t.GuardAction> = record({
  type: constant('guard'),
  require: parseContextShape,
  query: string(),
}).refine((pos, data) => {
  return {
    ...data,
    type: 'GuardAction',
    id: pos.context.id,
  };
});

const parseQuery: Parser<t.QueryAction> = record({
  type: constant('query'),
  require: parseContextShape,
  query: sequence(record({type: parseType, query: string()})),
}).refine((pos, data) => {
  return {
    ...data,
    type: 'QueryAction',
    id: pos.context.id,
  };
});

const parsePick: Parser<t.PickAction> = record({
  type: constant('pick'),
  entity: string(),
  fields: sequence(string()),
}).refine((pos, data) => {
  return {
    ...data,
    type: 'PickAction',
    id: pos.context.id,
  };
});

const parseView: Parser<t.ViewAction> = record({
  type: constant('view'),
  fields: sequence(string()),
  entity: string(),
}).refine((pos, data) => {
  return {
    ...data,
    type: 'ViewAction',
    id: pos.context.id,
  };
});

const parseMake: Parser<t.MakeAction> = record({
  type: constant('make'),
  entity: string(),
}).refine((pos, data) => {
  return {
    ...data,
    type: 'MakeAction',
    id: pos.context.id,
  };
});

const parseEdit: Parser<t.EditAction> = record({
  type: constant('edit'),
  entity: string(),
}).refine((pos, data) => {
  return {
    ...data,
    type: 'EditAction',
    id: pos.context.id,
  };
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
