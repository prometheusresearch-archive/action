/**
 * @flow strict
 */

import * as React from 'react';
import {View, Text, TouchableHighlight} from 'react-native-web';
import * as W from 'workflow';
import type {State} from 'workflow';
import {ScreenTitle} from './ScreenTitle.js';

type P = {
  state: State,
  onPick: mixed => void,
  toolbar: React.Node,
};

export function Pick(props: P) {
  const data = W.getData(props.state);
  const title = W.getTitle(props.state);
  const onSelect = id => {
    props.onPick(id);
  };
  return (
    <View>
      <ScreenTitle>{title}</ScreenTitle>
      <View>{props.toolbar}</View>
      <View>
        <Table data={data} onSelect={onSelect} />
      </View>
    </View>
  );
}

function Table(props) {
  const {data, onSelect} = props;
  const rows = data.map(data => {
    const cells = [];
    for (const key in data) {
      cells.push(
        <View key={key} style={{padding: 5}}>
          <Text>{String(data[key])}</Text>
        </View>,
      );
    }
    return (
      <TouchableHighlight
        key={data.id}
        underlayColor="yellow"
        onPress={onSelect.bind(null, data.id)}>
        <View style={{flexDirection: 'row'}}>{cells}</View>
      </TouchableHighlight>
    );
  });
  return <View>{rows}</View>;
}
