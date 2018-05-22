/**
 * @flow
 */

import invariant from 'invariant';
import * as React from 'react';
import * as ReactNative from 'react-native-web';
import MediaQuery from 'react-responsive';
import * as Core from 'core';
import type {DB, State, Workflow as WorkflowConfig} from 'core';
import {Pick} from './Pick.js';
import {View} from './View.js';
import {Form} from './Form.js';
import {BarChart} from './BarChart.js';
import * as cfg from 'components/config';
import {SideNav, SideNavButton, divider} from 'components/SideNav';
import {Breadcrumb} from 'components/Breadcrumb';

type P = {
  db: DB,
  workflowConfig: WorkflowConfig,
};

type S = {
  workflowState: ?State,
};

export class Workflow extends React.Component<P, S> {
  constructor(props: P) {
    super(props);
    this.state = {
      workflowState: null,
    };
  }

  componentDidMount() {
    const workflowState = Core.run(this.props.db, this.props.workflowConfig);
    this.setState({workflowState});
  }

  onPick = (id: mixed) => {
    this.setState(state => {
      invariant(state.workflowState != null, 'Invalid state');
      const workflowState = Core.replaceArgs({id}, state.workflowState);
      return {
        ...state,
        workflowState,
      };
    });
  };

  onState = (workflowState: State) => {
    this.setState({workflowState});
  };

  onNavigate = ({state}: {state: State}) => {
    this.onState(state);
  };

  onBreadcrumb = (ev: {state: State}) => {
    this.onState(ev.state);
  };

  render() {
    const {workflowState} = this.state;
    if (workflowState == null) {
      return (
        <ReactNative.View>
          <ReactNative.Text>Loading...</ReactNative.Text>
        </ReactNative.View>
      );
    } else {
      const ui = Core.ui(workflowState);
      let screen = null;
      if (ui.name === 'pick') {
        screen = <Pick state={workflowState} onState={this.onState} />;
      } else if (ui.name === 'view') {
        screen = <View state={workflowState} onState={this.onState} />;
      } else if (ui.name === 'form') {
        screen = <Form state={workflowState} onState={this.onState} />;
      } else if (ui.name === 'barChart') {
        screen = <BarChart state={workflowState} onState={this.onState} />;
      } else {
        screen = (
          <ReactNative.View>
            <ReactNative.Text>UNKNOWN SCREEN</ReactNative.Text>
          </ReactNative.View>
        );
      }

      // do not show the current one
      const breadcrumb = Core.breadcrumb(workflowState);
      breadcrumb.pop();
      const breadcrumbItems = breadcrumb.map(item => ({
        id: Core.id(item),
        state: item,
        title: String(Core.query('title', item)),
      }));

      return (
        <ReactNative.View>
          <Breadcrumb items={breadcrumbItems} onSelect={this.onBreadcrumb} />
          <ReactNative.View style={{flexDirection: 'row'}}>
            <ReactNative.View style={{width: 300, paddingVertical: cfg.padding.size2}}>
              <WorkflowSideNav state={workflowState} onState={this.onState} />
            </ReactNative.View>
            <ReactNative.View style={{padding: 10, flex: 1}}>{screen}</ReactNative.View>
          </ReactNative.View>
        </ReactNative.View>
      );
    }
  }
}

function WorkflowSideNav({state, onState}) {
  const navItems = [];
  const around = Core.around(state);
  const next = Core.next(state);

  const onNavigate = ({state}: {state: State}) => {
    onState(state);
  };

  navItems.push(divider);

  around.forEach(item => {
    const titleVal = Core.query('title', item);
    // $FlowFixMe: ...
    const title: string = titleVal;
    navItems.push({
      id: Core.id(item),
      state: item,
      render: props => <SideNavButton {...props} label={title} />,
    });
  });

  navItems.push(divider);

  next.forEach(item => {
    const titleVal = Core.query('title', item);
    // $FlowFixMe: ...
    const title: string = titleVal;
    navItems.push({
      id: Core.id(item),
      state: item,
      render: props => <SideNavButton {...props} label={title} />,
    });
  });
  return <SideNav active={Core.id(state)} onActive={onNavigate} items={navItems} />;
}
