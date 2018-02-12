/**
 * @flow
 */

import * as React from 'react';
import * as W from 'api/Workflow';
import * as WE from 'api/WorkflowExecutor';
import {View, Button, Text, TouchableOpacity} from 'react-native-web';

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

function Breadcrumb({frame, onFrame}: {frame: WE.Frame, onFrame: WE.Frame => *}) {
  const trace = getFrameTrace(frame);

  const items = [];
  for (const c of trace) {
    if (c.action.type === 'View') {
      const onPress = () => onFrame(c);
      const title = c.action.title || c.action.id;
      items.unshift({id: c.action.id, title, onPress});
    }
  }

  const buttons = items.map((item, idx) => {
    const isLast = idx === items.length - 1;
    const style = isLast
      ? {padding: 10, fontWeight: '600'}
      : {padding: 10, fontWeight: '200'};
    return (
      <TouchableOpacity key={item.id} onPress={item.onPress}>
        <Text style={style}>{item.title}</Text>
      </TouchableOpacity>
    );
  });

  return <View style={{flexDirection: 'row'}}>{buttons}</View>;
}

function Next({frames, onFrame}: {frames: Array<WE.Frame>, onFrame: WE.Frame => *}) {
  const items = [];
  for (const c of frames) {
    if (c.action.type === 'View') {
      const onClick = () => onFrame(c);
      items.push(
        <div key={c.action.id} onClick={onClick}>
          {c.action.id}
        </div>,
      );
    }
  }
  return <div>{items}</div>;
}

type Props = {
  initialAction: W.Action,
  initialContext: W.Context,
  debugState?: boolean,
};

type State = {
  frame: WE.Frame,
  nextFrames: Array<WE.Frame>,
  data: W.Data,
  continueWithContext: W.Context => void,
  render: (W.Context, W.Data, (W.Context) => void) => React.Element<*>,
};

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
      nextFrames: [],
      data: {},
      continueWithContext: _context => {},
      render: (context, data, onContext) => <div>loading</div>,
    };
  }

  waitForUserInput = (
    frame: WE.Frame,
    nextFrames: Array<WE.Frame>,
    data: W.Data,
    continueWithContext: W.Context => void,
    render: (W.Context, W.Data, (W.Context) => void) => React.Element<*>,
  ) => {
    this.setState({frame, nextFrames, data, continueWithContext, render});
  };

  waitForData = async (query: W.Query<*>) => {
    const resp = await fetch(`http://localhost:3001/graphql?query=query{${query}}`);
    const data = await resp.json();
    return data.data;
  };

  onFrame = (frame: WE.Frame) => {
    this.run(frame);
  };

  render() {
    const {debugState} = this.props;
    const {frame, nextFrames, data, continueWithContext, render} = this.state;
    const title = frame.action.title || frame.action.id;
    return (
      <View style={{flex: 1}}>
        {debugState != null && debugState && <DebugState frame={frame} />}
        <Breadcrumb frame={frame} onFrame={this.onFrame} />
        <Next frames={nextFrames} onFrame={this.onFrame} />
        <View key={frame.action.id} style={{padding: 10, flex: 1}}>
          <View style={{paddingBottom: 10}}>
            <Text style={{fontWeight: '600', fontSize: 18}}>{title}</Text>
          </View>
          <View style={{flex: 1}}>
            {render(frame.context, data, continueWithContext)}
          </View>
        </View>
      </View>
    );
  }

  componentDidMount() {
    this.run(this.state.frame);
  }
}
