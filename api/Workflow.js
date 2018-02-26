/**
 * @flow
 */

import {
  record,
  string,
  mapping,
  mappingGeneral,
  sequence,
  lazy,
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
  type Parser as ParserBase,
} from './Validate.js';

type Sequence = {
  type: 'Sequence',
  sequence: Array<Workflow>,
};

type Choice = {
  type: 'Choice',
  choice: Array<Workflow>,
};

type Action = {
  type: 'Action',
  action: mixed,
};

type ActionRef = {
  type: 'ActionRef',
  ref: string,
};

type Workflow = Action | Sequence | Choice;

type ParserContext = {
  createActionReference(string): ?ActionRef,
};

type Parser<V> = ParserBase<V, ParserContext>;

const parseAction: Parser<Action> = record(
  {type: string()},
  {ignoreUnknownFields: true},
).refine((_ctx, action) => {
  return {type: 'Action', action};
});

const parseActionRef: Parser<ActionRef> = string().refine((pos, value, node) => {
  const action = pos.context.createActionReference(value);
  if (action == null) {
    throw new ParseError(`unable to resolve action ${value}`, pos);
  }
  return action;
});

const parseWorkflow = recursive(parseWorkflow => {
  const parseChoice: Parser<Workflow> = sequence(parseWorkflow).refine((_ctx, choice) => {
    if (choice.length === 1) {
      return choice[0];
    } else {
      return {type: 'Choice', choice};
    }
  });
  const parseSequence = mappingGeneral({
    keyParser: parseActionRef,
    valueParser: parseWorkflow,
    initial: [],
    build: (sequence, k, v) => sequence.concat(k).concat(v),
  }).refine((_ctx, items) => {
    if (items.length === 1) {
      return items[0];
    } else {
      const sequence = [];
      for (const item of items) {
        if (item.type === 'Sequence') {
          sequence.push(...item.sequence);
        } else {
          sequence.push(item);
        }
      }
      return {type: 'Sequence', sequence};
    }
  });
  return switchCase(
    [hasFieldOfType('type', string()), parseAction],
    [onSequence, parseChoice],
    [onMapping, parseSequence],
    [onScalar, parseActionRef],
  );
});

const parseWorkflowConfig: ParserBase<{[name: string]: Workflow}> = mapping(
  lazy(),
).refine((_ctx, rules, node) => {
  const ctx: ParserContext = {
    createActionReference(ref: string) {
      const rule = rules[ref];
      if (rule == null) {
        return null;
      } else {
        return {type: 'ActionRef', ref};
      }
    },
  };
  const result = {};
  for (const id in rules) {
    result[id] = rules[id].parseWith(ctx, parseWorkflow);
  }
  return result;
});

const parseWorkflowConfigByKey = record(
  {workflow: parseWorkflowConfig},
  {ignoreUnknownFields: true},
);

export function parse(node: Node) {
  return parseWith({}, parseWorkflowConfigByKey, node);
}

export function parseString(v: string) {
  return parseStringWith({}, parseWorkflowConfigByKey, v);
}
