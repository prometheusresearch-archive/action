/**
 * @flow strict
 */

import * as React from 'react';
import {View, Text, TouchableHighlight} from 'react-native-web';
import * as W from 'workflow';
import type {State, Args} from 'workflow';
import {ScreenTitle} from './ScreenTitle.js';

type P = {
  state: State,
  args: Args,
  toolbar: React.Node,
  onPick: mixed => void,
};

export function Pick(props: P) {
  const data = W.getData(props.state);
  const title = W.getTitle(props.state);
  const onSelect = id => {
    props.onPick(id);
  };
  const fields = props.args.fields || inferFields(data[0]);
  return (
    <View>
      <ScreenTitle>{title}</ScreenTitle>
      <View>{props.toolbar}</View>
      <View style={{padding: 5}}>
        <Table
          selectedId={props.args.id}
          data={data}
          onSelect={onSelect}
          fields={fields}
        />
      </View>
    </View>
  );
}

function inferFields(item) {
  const fields = [];
  for (const name in item) {
    const value = item[name];
    if (typeof value === 'object') {
      continue;
    }
    fields.push(name);
  }
  return fields;
}

function Table(props) {
  const {data, onSelect, fields, selectedId} = props;
  const rows = data.map(data => {
    const cells = [];
    for (const key of fields) {
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
        <View
          style={{
            flexDirection: 'row',
            backgroundColor:
              data.id != null && selectedId === data.id ? '#33ccff' : 'transparent',
          }}>
          {cells}
        </View>
      </TouchableHighlight>
    );
  });
  return <View>{rows}</View>;
}
