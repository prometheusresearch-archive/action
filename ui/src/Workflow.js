/**
 * @flow
 */

import * as React from 'react';
import * as ReactNative from 'react-native-web';
import * as W from 'workflow';
import type {Result, RenderableState, State, UI} from 'workflow';
import {Pick} from './Pick.js';
import {View} from './View.js';

type P = {
  startState: Result<RenderableState>,
};

type S = {
  state: Result<RenderableState>,
};

export class Workflow extends React.Component<P, S> {
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
      const args = W.uiArgs(ui);
      let screen = null;
      const toolbar = <NavToolbar items={next} onState={this.onState} />;
      if (name === 'pick') {
        screen = <Pick toolbar={toolbar} state={node} args={args} onPick={this.onPick} />;
      } else if (name === 'view') {
        screen = <View toolbar={toolbar} state={node} args={args} />;
      } else {
        screen = (
          <ReactNative.View>
            <ReactNative.Text>UNKNOWN SCREEN</ReactNative.Text>
          </ReactNative.View>
        );
      }
      const breadcrumbsItems = breadcrumbs
        .slice()
        .slice(1)
        .reverse();
      return (
        <ReactNative.View>
          <NavToolbar
            Button={BreadcrumbButton}
            items={breadcrumbsItems}
            onState={this.onState}
          />
          {siblings.length > 1 && (
            <NavToolbar
              isItemActive={item => W.id(item) === W.id(node)}
              items={W.next(prev)}
              onState={this.onState}
            />
          )}
          <ReactNative.View style={{padding: 10}}>{screen}</ReactNative.View>
        </ReactNative.View>
      );
    }
  }
}

function BreadcrumbButton(props) {
  return (
    <ReactNative.TouchableOpacity onPress={props.onPress}>
      <ReactNative.View style={{padding: 10, flexDirection: 'row'}}>
        <ReactNative.Text style={{fontWeight: props.isActive ? '900' : '200'}}>
          {props.title}
        </ReactNative.Text>
        <ReactNative.Text style={{paddingLeft: 15}}>→</ReactNative.Text>
      </ReactNative.View>
    </ReactNative.TouchableOpacity>
  );
}

function Button(props) {
  return (
    <ReactNative.TouchableOpacity onPress={props.onPress}>
      <ReactNative.View style={{padding: 10, flexDirection: 'row'}}>
        <ReactNative.Text style={{fontWeight: props.isActive ? '900' : '200'}}>
          {props.title}
        </ReactNative.Text>
      </ReactNative.View>
    </ReactNative.TouchableOpacity>
  );
}

function NavToolbar({items, onState, isItemActive, Button}) {
  const buttons = items.map((state, idx) => {
    const title = W.getTitle(state);
    const onPress = () => onState(state);
    const isLast = idx === items.length - 1;
    const isActive = isItemActive(state, idx);
    return (
      <Button
        key={idx}
        isActive={isActive}
        isLast={isLast}
        onPress={onPress}
        title={title}
      />
    );
  });
  return <ReactNative.View style={{flexDirection: 'row'}}>{buttons}</ReactNative.View>;
}

NavToolbar.defaultProps = {
  Button: Button,
  isItemActive: (_state, _idx) => false,
};
