/**
 * @flow
 */

import * as React from 'react';
import * as ReactNative from 'react-native-web';
import * as W from 'workflow';
import type {Result, RenderableState, State, UI} from 'workflow';
import {Pick} from './Pick.js';
import {View} from './View.js';
import {type BreadcrumbItem, Breadcrumb} from './Header.js';

type P = {
  startState: Result<RenderableState>,
  renderHeader?: ({
    breadcrumb: Array<BreadcrumbItem>,
    toolbar?: ?React.Node,
  }) => React.Node,
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
      const breadcrumbItems = breadcrumbs
        .slice(1)
        .reverse()
        .map(item => ({
          title: String(W.getTitle(item)),
          onPress: this.onState.bind(null, item),
        }));

      const navToolbar =
        siblings.length > 1 ? (
          <NavToolbar
            isItemActive={item => W.id(item) === W.id(node)}
            items={W.next(prev)}
            onState={this.onState}
          />
        ) : null;
      return (
        <ReactNative.View>
          {this.props.renderHeader != null ? (
            this.props.renderHeader({breadcrumb: breadcrumbItems, toolbar: navToolbar})
          ) : (
            <Header breadcrumb={breadcrumbItems} toolbar={navToolbar} />
          )}
          <ReactNative.View style={{padding: 10}}>{screen}</ReactNative.View>
        </ReactNative.View>
      );
    }
  }
}

function Header(props) {
  const needBreadcrumb = props.breadcrumb.length > 0;
  const needNav = props.toolbar != null;
  if (!needNav && !needBreadcrumb) {
    return null;
  }
  return (
    <ReactNative.View style={{padding: 10}}>
      <ReactNative.View
        style={{
          boxShadow: '0px 1px 0px 0px #BBB',
          borderRadius: 2,
          flexDirection: 'column',
          border: '1px solid #BBB',
        }}>
        {needBreadcrumb && (
          <ReactNative.View style={{borderTop: '1px solid #bbb'}}>
            <Breadcrumb breadcrumb={props.breadcrumb} />
          </ReactNative.View>
        )}
        {needNav && (
          <ReactNative.View style={{borderTop: '1px solid #bbb'}}>
            {props.toolbar}
          </ReactNative.View>
        )}
      </ReactNative.View>
    </ReactNative.View>
  );
}

function Button(props) {
  return (
    <ReactNative.TouchableOpacity onPress={props.onPress}>
      <ReactNative.View
        style={{paddingHorizontal: 10, paddingVertical: 7, flexDirection: 'row'}}>
        <ReactNative.Text
          style={{fontSize: '9pt', fontWeight: props.isActive ? '600' : '200'}}>
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
