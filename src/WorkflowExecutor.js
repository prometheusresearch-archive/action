/**
 * @flow
 */

const invariant = require('invariant');

import type {Action, Context, View} from './Workflow.js';

type State = {
  prev: ?State,
  action: Action,
  context: Context,
};

type WorkflowEvent = RenderView | EndOfWorkflow;

type RenderView = {
  type: 'RenderView',
  view: View<*>,
};

type EndOfWorkflow = {
  type: 'EndOfWorkflow',
};

export function run(
  initialAction: Action,
  initialContext: Context,
  handle: WorkflowEvent => void,
) {
  function aux(state) {
    const {action, context} = state;
    switch (action.type) {
      case 'View': {
      }
      case 'Process': {
      }
      case 'Guard': {
      }
      case 'Sequence': {
      }
      case 'Choice': {
      }
      default:
        invariant(false, 'Unknown action: %s', action.type);
    }
  }

  const state: State = {prev: null, action: initialAction, context: initialContext};
  aux(state);
}
