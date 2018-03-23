/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import {Workflow} from './Workflow';
import * as W from 'workflow';

type ScreenId = 'start' | 'workflow' | 'console';

type ScreenConfig = {
  [id: ScreenId]: {
    title: string,
    Component: React.ComponentType<{}>,
  },
};

type P = {};
type S = {activeScreen: ScreenId};

export class App extends React.Component<P, S> {
  state = {activeScreen: 'start'};
  onScreen = (activeScreen: ScreenId) => this.setState({activeScreen});
  render() {
    const {activeScreen} = this.state;
    const screen = screens[activeScreen];
    return (
      <View>
        <View style={{flexDirection: 'row', alignItems: 'center'}}>
          <View style={{padding: 10}}>
            <Text style={{fontWeight: '900', fontSize: '22pt'}}>action</Text>
          </View>
          <View style={{flexDirection: 'row'}}>
            {Object.keys(screens).map(id => {
              const screen = screens[id];
              return (
                <MenuButton
                  active={activeScreen === id}
                  onPress={this.onScreen.bind(null, id)}>
                  {screen.title}
                </MenuButton>
              );
            })}
          </View>
        </View>
        <View />
        <View>
          <screen.Component />
        </View>
      </View>
    );
  }
}

function StartScreen() {
  return (
    <View style={{padding: 10}}>
      <Text>Hello</Text>
    </View>
  );
}

function WorkflowScreen() {
  return <Workflow startState={W.start} />;
}

function ConsoleScreen() {
  return (
    <View style={{padding: 10}}>
      <Text>Hello</Text>
    </View>
  );
}

const screens: ScreenConfig = {
  start: {
    title: 'Start',
    Component: StartScreen,
  },
  workflow: {
    title: 'Predefined Workflow',
    Component: WorkflowScreen,
  },
  console: {
    title: 'Query Console',
    Component: WorkflowScreen,
  },
};

function MenuButton({children, active, onPress}) {
  return (
    <TouchableOpacity onPress={onPress}>
      <View style={{padding: 10}}>
        <Text style={{fontWeight: active ? '900' : '400'}}>{children}</Text>
      </View>
    </TouchableOpacity>
  );
}
