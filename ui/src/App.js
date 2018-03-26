/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import * as W from 'workflow';
import {type ScreenConfig, type BreadcrumbItem, Header} from './Header.js';
import {Console} from './Console.js';
import {Workflow} from './Workflow';

type ScreenId = 'start' | 'workflow' | 'console';

type P = {};
type S = {activeScreen: ScreenId};

export class App extends React.Component<P, S> {
  state = {activeScreen: 'console'};
  onScreen = (activeScreen: ScreenId) => this.setState({activeScreen});
  render() {
    const {activeScreen} = this.state;
    const screen = screens[activeScreen];
    const renderHeader = props => (
      <Header
        breadcrumb={props.breadcrumb}
        screens={screens}
        activeScreen={activeScreen}
        onScreen={this.onScreen}
        toolbar={props.toolbar}
      />
    );
    return (
      <View>
        <screen.Component renderHeader={renderHeader} />
      </View>
    );
  }
}

function StartScreen(props) {
  return (
    <View>
      {props.renderHeader({breadcrumb: []})}
      <View style={{padding: 10}}>
        <Text>Hello</Text>
      </View>
    </View>
  );
}

function WorkflowScreen(props) {
  return <Workflow startState={W.start} renderHeader={props.renderHeader} />;
}

function ConsoleScreen(props) {
  return <Console renderHeader={props.renderHeader} />;
}

const screens: ScreenConfig<ScreenId> = {
  // start: {
  //   title: 'Start',
  //   Component: StartScreen,
  // },
  // workflow: {
  //   title: 'Predefined Workflow',
  //   Component: WorkflowScreen,
  // },
  console: {
    title: 'Query Console',
    Component: ConsoleScreen,
  },
};
