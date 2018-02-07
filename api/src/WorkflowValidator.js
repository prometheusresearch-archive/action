/**
 * This module defines an abstract interpreter to validate workflows.
 *
 * It operates on a type level of contexts and does speculative execution for
 * each of the paths in the Choice combinator.
 *
 * Essentially this is the type checking procedure.
 *
 * @flow
 */

const invariant = require('invariant');

import * as t from './types.js';
import type {Action, ContextType, ContextTypeShape} from './Workflow.js';

export function validate(initialAction: Action, initialContext: ContextType) {
  function aux(action, context, trace) {
    switch (action.type) {
      case 'View': {
        ensureContextTypeConformsTo(action, context, action.requires);
        context = updateContextType(context, action.provides);
        return context;
      }
      case 'Process': {
        ensureContextTypeConformsTo(action, context, action.requires);
        context = updateContextType(context, action.provides);
        return context;
      }
      case 'Guard': {
        return aux(action.action, context, trace.concat(action));
      }
      case 'Sequence': {
        return action.actions.reduce(
          ({context, trace}, action) => {
            return {
              context: aux(action, context, trace),
              trace: trace.concat(action),
            };
          },
          {context, trace: trace.concat(action)},
        ).context;
      }
      case 'Choice': {
        const contexts = action.actions.map(a => aux(a, context, trace.concat(action)));
        return reduceContextTypes(contexts);
      }
      default:
        invariant(false, 'Unknown action: %s', action.type);
    }
  }
}

function ensureContextTypeConformsTo(
  action: Action,
  context: ContextType,
  shape: ContextTypeShape,
) {
  return false;
}

function updateContextType(context: ContextType, shape: ContextTypeShape): ContextType {
  return context;
}

function reduceContextTypes(contexts: Array<ContextType>): ContextType {
  return contexts.reduce((a, b) => a, {});
}
