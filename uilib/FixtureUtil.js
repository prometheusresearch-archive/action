/**
 * @flow
 */

import * as React from 'react';
import {View, Text} from 'react-native-web';
import * as cfg from './config.js';

type _Fixture<P> = {
  name?: string,
  component: React.ComponentType<P>,
  props: P,
};

export type Fixture = _Fixture<*>;
export type FixtureList = Array<Fixture>;

type ShowcaseListProps = {
  rows: Array<{
    title: string,
    element: React.Node,
  }>,
};

export function createShowcaseList(
  displayName: string,
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
    return <View style={{padding: cfg.padding.size4}}>{rowsRendered}</View>;
  }
  ShowcaseList.displayName = displayName;
  return ShowcaseList;
}

type ShowcaseMatrixProps = {
  rows: Array<{
    title: string,
    columns: Array<React.Node>,
  }>,
};

export function createShowcaseMatrix(displayName: string) {
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

    return <View style={{padding: cfg.padding.size4}}>{casesRendered}</View>;
  }
  ShowcaseMatrix.displayName = displayName;
  return ShowcaseMatrix;
}
