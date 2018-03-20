/**
 * @flow
 */

import * as React from 'react';
import * as ReactNative from 'react-native-web';
import * as W from 'workflow';
import type {Result, State, UI, UntypedQuery} from 'workflow';
import {Pick} from './Pick.js';
import {View} from './View.js';

type P = {
  startState: Result<State>,
};

type S = {
  state: Result<{+ui: UI, +state: State}>,
};

export class App extends React.Component<P, S> {
  constructor(props: P) {
    super(props);
    this.state = {
      state: {type: 'Error', error: 'not started'},
    };
  }

  componentDidMount() {
    const {startState} = this.props;
    if (startState.type === 'Ok') {
      this.setState(state => {
        const nextState = W.render(startState.value);
        return {...state, state: nextState};
      });
    }
  }

  onQuery = (q: UntypedQuery) => {
    this.setState(state => {
      const w = state.state;
      if (w.type === 'Error') {
        return state;
      }
      const next = W.bind(q, w.value.state);
      return {
        ...state,
        state: next,
      };
    });
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
      const {ui, state: node} = state.value;
      if (ui.name === 'pick') {
        return <Pick query={W.getQuery(ui, node)} onQuery={this.onQuery} />;
      } else if (ui.name === 'view') {
        return <View query={W.getQuery(ui, node)} onQuery={this.onQuery} />;
      } else {
        return (
          <ReactNative.View>
            <ReactNative.Text>UNKNOWN SCREEN</ReactNative.Text>
          </ReactNative.View>
        );
      }
    }
  }
}
