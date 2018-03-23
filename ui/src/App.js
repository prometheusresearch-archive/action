/**
 * @flow
 */

import * as React from 'react';
import * as ReactNative from 'react-native-web';
import * as W from 'workflow';
import type {Result, State, UI} from 'workflow';
import {Pick} from './Pick.js';
import {View} from './View.js';

type P = {
  startState: Result<{+ui: UI, +state: State}>,
};

type S = {
  state: Result<{+ui: UI, +state: State}>,
};

export class App extends React.Component<P, S> {
  constructor(props: P) {
    super(props);
    this.state = {
      state: props.startState,
    };
  }

  onPick = (id: mixed) => {
    this.setState(state => {
      const w = state.state;
      if (w.type === 'Error') {
        return state;
      }
      const next = W.pickValue(id, w.value.state);
      return {
        ...state,
        state: next,
      };
    });
  };

  onState = (state: State) => {
    this.setState({state: W.render(state)});
  };

  render() {
    const {state} = this.state;
    if (state.type === 'Error') {
      return (
        <ReactNative.View>
          <ReactNative.Text>{state.error}</ReactNative.Text>
        </ReactNative.View>
      );
    } else if (state.type === 'Ok') {
      const breadcrumbs = W.breadcrumbs(state.value.state);
      const prev = breadcrumbs[1];
      const siblings = prev != null ? W.next(prev) : [];
      const next = W.next(state.value.state);
      const {ui, state: node} = state.value;
      const name = W.uiName(ui);
      let screen = null;
      const toolbar = <NavToolbar items={next} onState={this.onState} />;
      if (name === 'pick') {
        screen = <Pick toolbar={toolbar} state={node} onPick={this.onPick} />;
      } else if (name === 'view') {
        screen = <View toolbar={toolbar} state={node} />;
      } else {
        screen = (
          <ReactNative.View>
            <ReactNative.Text>UNKNOWN SCREEN</ReactNative.Text>
          </ReactNative.View>
        );
      }
      return (
        <ReactNative.View>
          <NavToolbar items={breadcrumbs.slice().reverse()} onState={this.onState} />
          {siblings.length > 1 && (
            <NavToolbar items={W.next(prev)} onState={this.onState} />
          )}
          <ReactNative.View style={{padding: 10}}>{screen}</ReactNative.View>
        </ReactNative.View>
      );
    }
  }
}

function NavToolbar({items, onState}) {
  const buttons = items.map((state, idx) => {
    const title = W.getTitle(state);
    const onPress = () => onState(state);
    return (
      <ReactNative.TouchableOpacity key={idx} onPress={onPress}>
        <ReactNative.View style={{padding: 10}}>
          <ReactNative.Text>{title}</ReactNative.Text>
        </ReactNative.View>
      </ReactNative.TouchableOpacity>
    );
  });
  return <ReactNative.View style={{flexDirection: 'row'}}>{buttons}</ReactNative.View>;
}
