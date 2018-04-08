/**
 * @flow
 */

import * as React from 'react';
import {ScrollView, View, Text} from 'react-native-web';
import * as cfg from './config.js';

type _Fixture<P> = {
  name?: string,
  component: React.ComponentType<P>,
  props: P,
};

export type Fixture = _Fixture<*>;
export type FixtureList = Array<Fixture>;

function getComponentDisplayName(Component: React.ComponentType<*>) {
  // $FlowFixMe: awaiting official React Reflection API (or whatever)
  return Component.displayName || Component.name || 'Unknown';
}

type ShowcaseListProps = {
  rows: Array<{
    title: string,
    element: React.Node,
  }>,
};

export function createShowcaseList(
  Component: React.ComponentType<*>,
): React.ComponentType<ShowcaseListProps> {
  function ShowcaseList({rows}: ShowcaseListProps) {
    let rowsRendered = rows.map(row => (
      <View style={{padding: cfg.padding.size2}}>
        <View>
          <Text
            style={{
              color: cfg.color.black,
              fontWeight: cfg.fontWeight.semibold,
              fontFamily: cfg.fontFamily.sans,
            }}>
            {row.title}:
          </Text>
        </View>
        <View style={{paddingVertical: cfg.padding.size2}}>{row.element}</View>
      </View>
    ));
    return <ScrollView style={{padding: cfg.padding.size4}}>{rowsRendered}</ScrollView>;
  }
  ShowcaseList.displayName = getComponentDisplayName(Component);
  return ShowcaseList;
}

type ShowcaseMatrixProps = {
  rows: Array<{
    title: string,
    columns: Array<React.Node>,
  }>,
};

export function createShowcaseMatrix(Component: React.ComponentType<*>) {
  function ShowcaseMatrix({rows}: ShowcaseMatrixProps) {
    let casesRendered = rows.map(item => (
      <View>
        <View style={{padding: cfg.padding.size2}}>
          <Text>{item.title}</Text>
        </View>
        <View style={{alignItems: 'flex-start', flexDirection: 'row'}}>
          {item.columns.map(column => (
            <View style={{padding: cfg.padding.size2}}>{column}</View>
          ))}
        </View>
      </View>
    ));

    return <ScrollView style={{padding: cfg.padding.size4}}>{casesRendered}</ScrollView>;
  }
  ShowcaseMatrix.displayName = getComponentDisplayName(Component);
  return ShowcaseMatrix;
}

type StateContainerProps<S> = {
  initialState: S,
  children: ({state: S, onState: S => void}) => React.Node,
};

export class StateContainer<S> extends React.Component<StateContainerProps<S>, S> {
  constructor(props: StateContainerProps<S>) {
    super(props);
    this.state = props.initialState;
  }

  onState = (state: $Shape<S>) => this.setState(state);

  render() {
    return this.props.children({state: this.state, onState: this.onState});
  }
}
