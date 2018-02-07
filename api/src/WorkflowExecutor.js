/**
 * @flow
 */

import * as React from 'react';
const invariant = require('invariant');

import type {Action, Context, View, Query, Data} from './Workflow.js';

type Config = {
  initialAction: Action,
  initialContext: Context,
  waitForUserInput: (() => React.Element<*>) => void,
  waitForData: (Query<*>) => Promise<Data>,
};

type State = {
  action: Action,
  context: Context,

  allowBail(): State,
  ignoreBail(): State,
  withAction(Action): State,

  bail(): StateTransition,
  next(): StateTransition,
  nextWithContext(context: Context): StateTransition,
};

type Next = {
  type: 'Next',
  state: State,
};

type Bail = {
  type: 'Bail',
};

type StateTransition = Next | Bail;

export async function run({
  initialAction,
  initialContext,
  waitForUserInput,
  waitForData,
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

    if (query != null) {
      const data = await waitForData(query);
      return data;
    } else {
      return {};
    }
  }

  async function step(state: State): Promise<?StateTransition> {
    const data = await fetchData(state);
    const {action} = state;

    switch (action.type) {
      case 'View': {
        // TODO: check if context conforms to view requirements
        return new Promise(resolve => {
          const onContext = context => {
            resolve(state.ignoreBail().nextWithContext(context));
          };
          const render = () => {
            return action.render(state.context, data.get(action), onContext);
          };
          waitForUserInput(render);
        });
      }

      case 'Process': {
        // TODO: check if context conforms to action requirements
        const nextContext = await action.execute(state.context, data[action]);
        return state.nextWithContext(nextContext);
      }

      case 'Guard': {
        // TODO: check if context conforms to action requirements
        const isAllowed = data.get(action);
        if (isAllowed) {
          return state.nextWithContext(state.context);
        } else {
          return state.bail();
        }
      }

      case 'Sequence': {
        let nextState = {...state};
        for (const action of action.actions) {
          nextState = nextState.withAction(action);
          const next: ?StateTransition = await step(nextState);
          if (next == null) {
            continue;
          } else if (next.type === 'Next') {
            nextState = next.state;
            continue;
          } else {
            return next;
          }
        }
        return state.nextWithContext(nextState.context);
      }

      case 'Choice': {
        for (const action of action.actions) {
          const scopedState = state.allowBail().withAction(action);
          const next: ?StateTransition = await step(scopedState);
          if (next == null) {
            continue;
          } else if (next.type === 'Bail') {
            continue;
          } else {
            return next;
          }
        }
        return state.next();
      }

      default:
        invariant(false, 'Unknown action: %s', action.type);
    }
  }

  const state: State = {
    prev: null,

    action: initialAction,

    context: initialContext,

    withAction(action) {
      return {
        ...this,
        action,
      };
    },

    allowBail() {
      return {
        ...this,
        bail() {
          return {type: 'Bail'};
        },
      };
    },

    ignoreBail() {
      return {
        ...this,
        bail() {
          return null;
        },
      };
    },

    bail() {
      invariant(false, 'Unable to bail before reaching an UI');
    },

    next() {
      return {type: 'Next', state: this};
    },

    nextWithContext(context) {
      return {type: 'Next', state: {...this, context}};
    },
  };

  return await step(state);
}
