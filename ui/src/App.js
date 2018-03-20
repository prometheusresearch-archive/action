/**
 * @flow
 */

import * as React from 'react';
import {View, Text} from 'react-native-web';
import * as W from 'workflow';
import type {Result, State, UI} from 'workflow';
import {Pick} from './Pick.js';

type P = {
  startState: Result<State>,
};

type S = {
  state: Result<{ui: UI, state: State}>,
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
      const nextState = W.render(startState.value);
      this.setState({state: nextState});
    }
  }

  render() {
    const {state} = this.state;
    if (state.type === 'Error') {
      return (
        <View>
          <Text>{state.error}</Text>
        </View>
      );
    } else if (state.type === 'Ok') {
      const {ui} = state.value;
      if (ui.name === 'pick') {
        return <Pick query={ui.query} />;
      } else if (ui.name === 'view') {
        return (
          <View>
            <Text>VIEW</Text>
          </View>
        );
      } else {
        return (
          <View>
            <Text>UNKNOWN SCREEN</Text>
          </View>
        );
      }
    }
  }
}
