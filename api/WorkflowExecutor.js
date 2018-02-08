/**
 * @flow
 */

import * as React from 'react';
const mkDebug = require('debug');
const invariant = require('invariant');

const debug = mkDebug('RexAction:api:WorkflowExecutor');

import type {Action, Context, ContextTypeShape, View, Query, Data} from './Workflow.js';

export type Config = {
  initialAction: Action,
  initialContext?: Context,
  waitForUserInput: (
    Context,
    Data,
    (Context) => void,
    (Context, Data, (Context) => void) => React.Element<*>,
  ) => *,
  waitForData: (Query<*>) => Promise<{get(mixed): mixed}>,
  onState?: State => void,
  onTransition?: StateTransition => void,
};

export type State = {
  action: Action,
  context: Context,

  bail: 'allow' | 'ignore',
};

export type StateTransition = Next | Bail | Noop;

type Next = {
  type: 'Next',
  state: State,
};

type Bail = {
  type: 'Bail',
};

type Noop = {
  type: 'Noop',
};

function bail(state: State): StateTransition {
  switch (state.bail) {
    case 'allow':
      return {type: 'Bail'};
    case 'ignore':
      return {type: 'Noop'};
    default:
      invariant(false, 'Impossible');
  }
}

function next(state: State): StateTransition {
  return {type: 'Next', state};
}

function nextWithContext(state: State, context: Context): StateTransition {
  return {type: 'Next', state: {...state, context}};
}

export async function run({
  initialAction,
  initialContext,
  waitForUserInput,
  waitForData,
  onState,
  onTransition,
}: Config) {
  async function fetchData(state: State) {
    function findQuery(state: State) {
      const {action, context} = state;
      switch (action.type) {
        case 'View': {
          return action.query(context);
        }
        case 'Process': {
          return action.query(context);
        }
        case 'Guard': {
          return action.query(context);
        }
        case 'Sequence': {
          return null;
        }
        case 'Choice': {
          return null;
        }
        default:
          invariant(false, 'Unknown action: %s', action.type);
      }
    }

    const query = findQuery(state);
    debug('query', query);

    if (query != null) {
      const data = await waitForData(query);
      return data;
    } else {
      return {};
    }
  }

  async function step(state: State): Promise<StateTransition> {
    debug('state', state);
    if (onState != null) {
      onState(state);
    }
    const transition = await stepImpl(state);
    debug('transition', transition);
    if (onTransition != null) {
      onTransition(transition);
    }
    return transition;
  }

  async function stepImpl(state: State): Promise<StateTransition> {
    const {context, action} = state;

    if (!contextConformsTo(context, getContextRequirements(action))) {
      return bail(state);
    }

    const data = await fetchData(state);

    switch (action.type) {
      case 'View': {
        return new Promise(resolve => {
          const onContext = context => {
            state = {...state, bail: 'ignore'};
            resolve(nextWithContext(state, context));
          };
          waitForUserInput(state.context, data, onContext, action.render);
        });
      }

      case 'Process': {
        const nextContext = await action.execute(state.context, data);
        return nextWithContext(state, nextContext);
      }

      case 'Guard': {
        const isAllowed: boolean = Boolean(data);
        if (isAllowed) {
          return next(state);
        } else {
          return bail(state);
        }
      }

      case 'Sequence': {
        let nextState = {...state};
        for (const childAction of action.sequence) {
          nextState = {...nextState, action: childAction};
          const next: StateTransition = await step(nextState);
          if (next.type === 'Next') {
            nextState = next.state;
            continue;
          } else {
            return next;
          }
        }
        return next(nextState);
      }

      case 'Choice': {
        for (const childAction of action.choice) {
          const scopedState = {...state, bail: 'allow', action: childAction};
          const next: StateTransition = await step(scopedState);
          if (next.type === 'Bail') {
            continue;
          } else {
            return next;
          }
        }
        return next(state);
      }

      default:
        invariant(false, 'Unknown action: %s', action.type);
    }
  }

  const state: State = {
    prev: null,
    action: initialAction,
    context: initialContext || {},
    bail: 'ignore',
  };

  return await step(state);
}

function contextConformsTo(context: Context, shape: ContextTypeShape) {
  for (const key in shape) {
    const value = context[key];
    if (value == null) {
      return false;
    }
    const type = shape[key];
    switch (type.type) {
      case 'NumberType':
        if (typeof value !== 'number') {
          return false;
        }
        break;
      case 'StringType':
        if (typeof value !== 'string') {
          return false;
        }
        break;
      case 'EntityType':
        if (!(typeof value === 'object' && value != null && value.__type === type.name)) {
          return false;
        }
        break;
      default:
        // TODO:
        invariant(false, 'Need to handle type: %s', type.type);
    }
  }
  return true;
}

function getContextRequirements(action: Action) {
  switch (action.type) {
    case 'View':
    case 'Process':
    case 'Guard':
      return action.requires;
    case 'Sequence':
    case 'Choice':
      return {};
    default:
      invariant(false, 'Unknown action: %s', action.type);
  }
}
