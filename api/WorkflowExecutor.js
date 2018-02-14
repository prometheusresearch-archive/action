/**
 * @noflow
 */

import * as React from 'react';
const mkDebug = require('debug');
const invariant = require('invariant');

const debug = mkDebug('RexAction:api:WorkflowExecutor');

import * as W from './Workflow.js';

export type Config = {
  initialAction: W.Action,
  initialContext?: W.Context,
  waitForUserInput: (
    Frame,
    Array<Frame>,
    W.Data,
    (W.Context) => void,
    (W.Context, W.Data, (W.Context) => void) => React.Element<*>,
  ) => *,
  waitForData: (W.Query<*>) => Promise<{get(mixed): mixed}>,
};

export type Frame = SequenceFrame | ChoiceFrame | SimpleFrame;

export type SequenceFrame = {
  type: 'SequenceFrame',
  prev: ?Frame,
  parent: ?Frame,
  action: W.Sequence,
  context: W.Context,
  index: number,
};

export type ChoiceFrame = {
  type: 'ChoiceFrame',
  prev: ?Frame,
  parent: ?Frame,
  action: W.Choice,
  context: W.Context,
  index: number,
};

export type SimpleFrame = {
  type: 'SimpleFrame',
  prev: ?Frame,
  parent: ?Frame,
  action: W.View<*> | W.Process<*> | W.Guard,
  context: W.Context,
};

export type StateTransition = Next | Bail | Execute;

type Next = {
  type: 'Next',
  context: W.Context,
};

type Bail = {
  type: 'Bail',
};

type Execute = {
  type: 'Execute',
  context: W.Context,
};

export type Executor = {
  frame: Frame,
  run(state: Frame): Promise<*>,
};

export function run({
  initialAction,
  initialContext,
  waitForUserInput,
  waitForData,
  onState,
  onTransition,
}: Config): Executor {
  async function fetchData(frame: Frame) {
    function findQuery(frame: Frame) {
      const {action, context} = frame;
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

    const query = findQuery(frame);

    if (query != null) {
      const data = await waitForData(query);
      return data;
    } else {
      return {};
    }
  }

  async function speculate(rootFrame: Frame, prevFrame: ?Frame): Promise<Array<Frame>> {
    async function speculateImpl(
      frame: Frame,
      prevFrame: ?Frame,
    ): Promise<?Array<Frame>> {
      function returnFrame(prev) {
        if (frame.parent != null) {
          return speculateImpl(frame.parent, prev);
        } else {
          return [];
        }
      }

      debug('speculateImpl', `action=${frame.action.id}`, {
        action: frame.action,
        context: frame.context,
        frame,
        prevFrame,
      });

      const {context, action} = frame;

      if (!contextConformsTo(context, getContextRequirements(action))) {
        return returnFrame(prevFrame);
      }

      switch (frame.type) {
        case 'SequenceFrame': {
          for (const nextAction of frame.action.sequence) {
            const nextFrame = call(
              frame.action.sequence[frame.index],
              frame,
              prevFrame,
              frame.context,
            );
            const result = await speculateImpl(nextFrame, prevFrame);
            if (result == null) {
              return null;
            } else if (result.length !== 0) {
              return result;
            }
          }
          return returnFrame(prevFrame);
        }
        case 'ChoiceFrame': {
          const results = [];
          for (const nextAction of frame.action.choice) {
            const nextFrame = call(
              frame.action.choice[frame.index],
              frame,
              prevFrame,
              frame.context,
            );
            const result = await speculateImpl(nextFrame, prevFrame);
            if (result != null) {
              results.push(...result);
            }
          }
          return results;
        }
        case 'SimpleFrame':
          switch (action.type) {
            case 'View': {
              return frame.action.id === rootFrame.action.id ? [] : [frame];
            }

            case 'Process': {
              return null;
            }

            case 'Guard': {
              const data = await fetchData(frame);
              if (await action.allowed(context, data)) {
                return returnFrame(prevFrame);
              } else {
                return null;
              }
            }
            default:
              invariant(false, 'Unknown action: %s', action.type);
          }
        default:
          invariant(false, 'Unknown frame: %s', frame.type);
      }
    }
    const res = await speculateImpl(rootFrame, prevFrame);
    return res || [];
  }

  async function execute(frame: Frame, transition: StateTransition, prevFrame: ?Frame) {
    function returnFrame(transition, prev) {
      if (frame.parent != null) {
        return execute(frame.parent, transition, prev);
      } else {
        return null;
      }
    }

    debug('execute', `action=${frame.action.id}`, `transition=${transition.type}`, {
      action: frame.action,
      context: frame.context,
      frame,
      prevFrame,
      transition,
    });

    const {context, action} = frame;

    if (!contextConformsTo(context, getContextRequirements(action))) {
      return returnFrame({type: 'Bail'}, prevFrame);
    }

    switch (frame.type) {
      case 'SequenceFrame': {
        if (transition.type === 'Execute') {
          const nextFrame = call(
            frame.action.sequence[frame.index],
            frame,
            prevFrame,
            transition.context,
          );
          return execute(nextFrame, {type: 'Execute', context: frame.context}, frame);
        } else if (transition.type === 'Next') {
          if (frame.index < frame.action.sequence.length - 1) {
            const nextFrame = {
              parent: frame,
              prev: prevFrame,
              type: 'SequenceFrame',
              action: frame.action,
              context: transition.context,
              index: frame.index + 1,
            };
            return execute(
              nextFrame,
              {type: 'Execute', context: nextFrame.context},
              prevFrame,
            );
          } else {
            return returnFrame(transition, prevFrame);
          }
        } else {
          return returnFrame(transition, prevFrame);
        }
      }
      case 'ChoiceFrame': {
        if (transition.type === 'Execute') {
          const nextFrame = call(
            frame.action.choice[frame.index],
            frame,
            prevFrame,
            transition.context,
          );
          return execute(nextFrame, {type: 'Execute', context: frame.context}, frame);
        } else if (transition.type === 'Bail') {
          if (frame.index < frame.action.choice.length - 1) {
            const nextFrame = {
              parent: frame,
              prev: prevFrame,
              type: 'ChoiceFrame',
              action: frame.action,
              context: frame.context,
              index: frame.index + 1,
            };
            return execute(
              nextFrame,
              {type: 'Execute', context: nextFrame.context},
              prevFrame,
            );
          } else {
            return returnFrame(transition, prevFrame);
          }
        } else {
          return returnFrame(transition, prevFrame);
        }
      }
      case 'SimpleFrame':
        switch (action.type) {
          case 'View': {
            if (transition.type === 'Execute') {
              const f = frame;
              const data = await fetchData(frame);
              const next = await speculate(frame, prevFrame);
              return new Promise(resolve => {
                const onContext = context => {
                  const nextContext = {...f.context, ...context};
                  const nextFrame = {...f, context: nextContext};
                  resolve(
                    execute(nextFrame, {type: 'Next', context: nextContext}, nextFrame),
                  );
                };
                waitForUserInput(f, next, data, onContext, action.render);
              });
            } else {
              return returnFrame(transition, prevFrame);
            }
          }

          case 'Process': {
            if (transition.type === 'Execute') {
              const data = await fetchData(frame);
              const context = await action.execute(frame.context, data);
              const nextContext = {...frame.context, ...context};
              return returnFrame({type: 'Next', context: nextContext}, prevFrame);
            } else {
              return returnFrame(transition, prevFrame);
            }
          }

          case 'Guard': {
            if (transition.type === 'Execute') {
              const data = await fetchData(frame);
              if (await action.allowed(context, data)) {
                return returnFrame({type: 'Next', context: frame.context}, prevFrame);
              } else {
                return returnFrame({type: 'Bail'}, prevFrame);
              }
            } else {
              return returnFrame(transition, prevFrame);
            }
          }
          default:
            invariant(false, 'Unknown action: %s', action.type);
        }
      default:
        invariant(false, 'Unknown frame: %s', frame.type);
    }
  }

  const frame: Frame = call(initialAction, null, null, initialContext || {});

  return {
    frame,
    run(frame: Frame) {
      return execute(frame, {type: 'Execute', context: frame.context}, null);
    },
  };
}

function contextConformsTo(context: W.Context, shape: W.ContextTypeShape) {
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

function call(action: W.Action, parent: ?Frame, prev: ?Frame, context: W.Context): Frame {
  switch (action.type) {
    case 'View':
    case 'Process':
    case 'Guard':
      return {type: 'SimpleFrame', action, parent, prev, context};
    case 'Sequence':
      return {type: 'SequenceFrame', action, parent, prev, context, index: 0};
    case 'Choice':
      return {type: 'ChoiceFrame', action, parent, prev, context, index: 0};
    default:
      invariant(false, 'Unknown action: %s', action.type);
  }
}

function getContextRequirements(action: W.Action) {
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
