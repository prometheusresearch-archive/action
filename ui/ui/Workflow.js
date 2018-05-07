/**
 * @flow
 */

import * as React from 'react';
import * as ReactNative from 'react-native-web';
import MediaQuery from 'react-responsive';
import * as W from 'core';
import type {Result, RenderableState, State} from 'core';
import {Pick} from './Pick.js';
import {View} from './View.js';
import {Edit} from './Edit.js';
import {BarChart} from './BarChart.js';
import * as cfg from 'components/config';
import {SideNav, SideNavButton, divider} from 'components/SideNav';
import {Breadcrumb} from 'components/Breadcrumb';

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

  onNavigate = ({state}: {state: State}) => {
    this.onState(state);
  };

  onBreadcrumb = (ev: {state: State}) => {
    console.log('xx', ev);
    this.onState(ev.state);
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
      // $FlowFixMe: ...
      const name = W.uiName(ui);
      const args = {};
      let screen = null;
      if (name === 'pick') {
        screen = (
          <Pick state={node} args={args} onPick={this.onPick} onState={this.onState} />
        );
      } else if (name === 'view') {
        screen = <View state={node} args={args} onState={this.onState} />;
      } else if (name === 'edit') {
        screen = <Edit state={node} args={args} onState={this.onState} />;
      } else if (name === 'barChart') {
        screen = <BarChart state={node} args={args} onState={this.onState} />;
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
          id: W.id(item),
          state: item,
          title: String(W.query(item, 'title')),
        }));

      const navItems = [];

      next.forEach(item => {
        const titleVal = W.query(item, `title`);
        // $FlowFixMe: ...
        const title: string = titleVal;
        navItems.push({
          id: W.id(item),
          state: item,
          render: props => <SideNavButton {...props} label={title} />,
        });
      });

      navItems.push(divider);

      siblings.forEach(item => {
        const titleVal = W.query(item, `title`);
        // $FlowFixMe: ...
        const title: string = titleVal;
        navItems.push({
          id: W.id(item),
          state: item,
          render: props => <SideNavButton {...props} label={title} />,
        });
      });

      return (
        <ReactNative.View>
          <React.Fragment>
            <Breadcrumb items={breadcrumbItems} onSelect={this.onBreadcrumb} />
            <ReactNative.View style={{flexDirection: 'row'}}>
              <ReactNative.View style={{width: 300, paddingVertical: cfg.padding.size2}}>
                <SideNav
                  active={W.id(node)}
                  onActive={this.onNavigate}
                  items={navItems}
                />
              </ReactNative.View>
              <ReactNative.View style={{padding: 10, flex: 1}}>{screen}</ReactNative.View>
            </ReactNative.View>
          </React.Fragment>
        </ReactNative.View>
      );
    }
  }
}
