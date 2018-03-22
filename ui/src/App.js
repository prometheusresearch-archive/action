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
    this.setState({state: W.renderState(state)});
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
      const {ui, state: node} = state.value;
      const name = W.uiName(ui);
      let screen = null;
      if (name === 'pick') {
        screen = <Pick state={node} onPick={this.onPick} />;
      } else if (name === 'view') {
        screen = <View state={node} />;
      } else {
        screen = (
          <ReactNative.View>
            <ReactNative.Text>UNKNOWN SCREEN</ReactNative.Text>
          </ReactNative.View>
        );
      }
      return (
        <ReactNative.View>
          <Breadcrumbs breadcrumbs={breadcrumbs} onState={this.onState} />
          {screen}
        </ReactNative.View>
      );
    }
  }
}

function Breadcrumbs({breadcrumbs, onState}) {
  breadcrumbs = breadcrumbs.slice();
  breadcrumbs.reverse();
  const buttons = breadcrumbs.map(state => {
    const title = W.getTitle(state);
    const onPress = () => onState(state);
    return (
      <ReactNative.TouchableOpacity onPress={onPress}>
        <ReactNative.View style={{padding: 10}}>
          <ReactNative.Text>{title}</ReactNative.Text>
        </ReactNative.View>
      </ReactNative.TouchableOpacity>
    );
  });
  return <ReactNative.View style={{flexDirection: 'row'}}>{buttons}</ReactNative.View>;
}
