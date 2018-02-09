/**
 * @flow
 */

import * as React from 'react';
import type {Action, Context} from '../Workflow.js';
import {type StateTransition, run} from '../WorkflowExecutor.js';

async function runAndCaptureTrace({
  initialAction,
  initialContext = {},
}: {
  initialAction: Action,
  initialContext?: Context,
}) {
  const trace = [];

  function onState(state) {
    trace.push({event: 'state', action: state.action.id, context: state.context});
  }

  function onTransition(transition: StateTransition) {
    trace.push({event: 'transition', type: transition.type});
  }

  async function waitForUserInput(state, data, onContext, render) {
    trace.push({event: 'render', context: state.context});
    render(state.context, data, onContext);
  }

  function waitForData(query) {
    trace.push({event: 'fetch', query});
    return Promise.resolve(new Map());
  }

  const next: ?StateTransition = await run({
    initialAction,
    initialContext,
    waitForUserInput,
    waitForData,
    onState,
    onTransition,
  });

  return {trace};
}

function view(params): Action {
  return {
    type: 'View',
    id: params.id,
    requires: params.requires || {},
    provides: {},
    query(context) {
      return '';
    },
    render(context, data, onContext) {
      if (params.onContext != null) {
        onContext(params.onContext(context));
      } else {
        onContext(context);
      }
      return <div>Hello</div>;
    },
  };
}

function set(key, value) {
  return context => ({...context, [key]: value});
}

test('one action workflow', async function() {
  const initialAction = view({id: 'hello', onContext: set('a', 43)});

  const {trace} = await runAndCaptureTrace({initialAction});
  expect(trace).toMatchSnapshot();
});

test('one action workflow bail', async function() {
  const initialAction = view({
    id: 'hello',
    requires: {
      a: {type: 'StringType'},
    },
    onContext: set('a', 43),
  });

  {
    const {trace} = await runAndCaptureTrace({initialAction});
    expect(trace).toMatchSnapshot();
  }

  {
    const {trace} = await runAndCaptureTrace({initialAction, initialContext: {a: 'ok'}});
    expect(trace).toMatchSnapshot();
  }

  {
    const {trace} = await runAndCaptureTrace({initialAction, initialContext: {a: 21}});
    expect(trace).toMatchSnapshot();
  }
});

test('two action sequence workflow', async function() {
  const initialAction = {
    type: 'Sequence',
    id: 'sequence',
    sequence: [
      view({id: 'hello', onContext: set('a', 43)}),
      view({id: 'world', onContext: set('b', 44)}),
    ],
  };

  const {trace} = await runAndCaptureTrace({initialAction});
  expect(trace).toMatchSnapshot();
});

test('two action sequence workflow - bail on second', async function() {
  const initialAction = {
    type: 'Sequence',
    id: 'sequence',
    sequence: [
      view({
        id: 'hello',
      }),
      view({
        id: 'world',
        requires: {
          req: {type: 'StringType'},
        },
      }),
    ],
  };

  const {trace} = await runAndCaptureTrace({initialAction});
  expect(trace).toMatchSnapshot();
});

test('two action choice workflow', async function() {
  const initialAction: Action = {
    type: 'Choice',
    id: 'choice',
    choice: [
      view({id: 'hello', onContext: set('a', 43)}),
      view({id: 'world', onContext: set('b', 44)}),
    ],
  };

  const {trace} = await runAndCaptureTrace({initialAction});
  expect(trace).toMatchSnapshot();
});
