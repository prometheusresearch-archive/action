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
  frame: WE.Frame,
  data: W.Data,
  continueWithContext: W.Context => void,
  render: (W.Context, W.Data, (W.Context) => void) => React.Element<*>,
};

function getFrameStack(state: WE.Frame) {
  const trace = [];
  let c: WE.Frame = state;
  while (true) {
    trace.push(c);
    if (c.parent != null) {
      c = c.parent;
    } else {
      break;
    }
  }
  return trace;
}

function getFrameTrace(state: WE.Frame) {
  const trace = [];
  let c: WE.Frame = state;
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

function DebugState({frame}: {frame: WE.Frame}) {
  const trace = getFrameStack(frame).map((f, idx) => (
    <div key={`${f.action.id}--${idx}`}>
      {f.action.id}
      <pre>{JSON.stringify(f.context, null, 2)}</pre>
    </div>
  ));
  return <div>{trace}</div>;
}

function Breadcrumb({frame, onState}: {frame: WE.Frame, onState: WE.Frame => *}) {
  const items = [];
  for (const c of getFrameTrace(frame)) {
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
  run: WE.Frame => *;

  static defaultProps = {
    initialContext: {},
  };

  constructor(props: Props) {
    super(props);
    const {run, frame} = WE.run({
      initialAction: this.props.initialAction,
      initialContext: this.props.initialContext,
      waitForUserInput: this.waitForUserInput,
      waitForData: this.waitForData,
    });
    this.run = run;
    this.state = {
      frame,
      data: {},
      continueWithContext: _context => {},
      render: (context, data, onContext) => <div>loading</div>,
    };
  }

  waitForUserInput = (
    frame: WE.Frame,
    data: W.Data,
    continueWithContext: W.Context => void,
    render: (W.Context, W.Data, (W.Context) => void) => React.Element<*>,
  ) => {
    this.setState({frame, data, continueWithContext, render});
  };

  waitForData = async (query: W.Query<*>) => {
    const resp = await fetch(`http://localhost:3001/graphql?query=query{${query}}`);
    const data = await resp.json();
    return data.data;
  };

  onState = (state: WE.Frame) => {
    this.run(state);
  };

  render() {
    const {debugState} = this.props;
    const {frame, data, continueWithContext, render} = this.state;
    return (
      <div>
        <div>
          {debugState != null && debugState && <DebugState frame={frame} />}
          <Breadcrumb frame={frame} onState={this.onState} />
          <div key={frame.action.id}>
            {render(frame.context, data, continueWithContext)}
          </div>
        </div>
      </div>
    );
  }

  componentDidMount() {
    this.run(this.state.frame);
  }
}
