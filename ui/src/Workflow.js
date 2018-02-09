/**
 * @flow
 */

import * as React from 'react';
import * as W from 'api/Workflow';
import * as WE from 'api/WorkflowExecutor';

type Props = {
  initialAction: W.Action,
  initialContext: W.Context,
  debugState?: boolean,
};

type State = {
  state: WE.State,
  data: W.Data,
  continueWithContext: W.Context => void,
  render: (W.Context, W.Data, (W.Context) => void) => React.Element<*>,
};

function getStateTrace(state: WE.State) {
  const trace = [];
  let c: WE.State = state;
  while (true) {
    trace.push(c);
    if (c.prev != null) {
      c = c.prev;
    } else {
      break;
    }
  }
  return trace;
}

function DebugState({state}: {state: WE.State}) {
  const trace = getStateTrace(state).map(c => (
    <div key={c.action.id}>
      {c.action.id}
      <pre>{JSON.stringify(c.context, null, 2)}</pre>
    </div>
  ));
  return <div>{trace}</div>;
}

function Breadcrumb({state, onState}: {state: WE.State, onState: WE.State => *}) {
  const items = [];
  for (const c of getStateTrace(state)) {
    if (c.action.type === 'View') {
      const onClick = () => onState(c);
      items.push(
        <div key={c.action.id} onClick={onClick}>
          {c.action.id}
        </div>,
      );
    }
  }
  return <div>{items}</div>;
}

export class Workflow extends React.Component<Props, State> {
  run: WE.State => *;

  static defaultProps = {
    initialContext: {},
  };

  constructor(props: Props) {
    super(props);
    const {run, state} = WE.run({
      initialAction: this.props.initialAction,
      initialContext: this.props.initialContext,
      waitForUserInput: this.waitForUserInput,
      waitForData: this.waitForData,
    });
    this.run = run;
    this.state = {
      state,
      data: {},
      continueWithContext: _context => {},
      render: (context, data, onContext) => <div>loading</div>,
    };
  }

  waitForUserInput = (
    state: WE.State,
    data: W.Data,
    continueWithContext: W.Context => void,
    render: (W.Context, W.Data, (W.Context) => void) => React.Element<*>,
  ) => {
    this.setState({state, data, continueWithContext, render});
  };

  waitForData = async (query: W.Query<*>) => {
    const resp = await fetch(`http://localhost:3001/graphql?query=query{${query}}`);
    const data = await resp.json();
    return data.data;
  };

  onState = (state: WE.State) => {
    this.run(state);
  };

  render() {
    const {debugState} = this.props;
    const {state, data, continueWithContext, render} = this.state;
    return (
      <div>
        {state == null && <div>Loading...</div>}
        {state != null && (
          <div>
            {debugState != null && debugState && <DebugState state={state} />}
            <Breadcrumb state={state} onState={this.onState} />
            <div key={state.action.id}>
              {render(state.context, data, continueWithContext)}
            </div>
          </div>
        )}
      </div>
    );
  }

  componentDidMount() {
    this.run(this.state.state);
  }
}
