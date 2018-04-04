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
  const {title, data, type} = W.query(
    props.state,
    `
      {
        title: title,
        data: dataForUI,
        type: dataForUI:meta.type,
      }
    `,
  );
  const onSelect = id => {
    props.onPick(id);
  };
  const fields = fieldsFromType(type);
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

function fieldsFromType(type) {
  switch (type.type) {
    case 'entity': {
      const fields = [];
      for (const key in type.fields) {
        fields.push(key);
      }
      return fields;
    }
    case 'record': {
      const fields = [];
      for (const key in type.fields) {
        fields.push(key);
      }
      return fields;
    }
    default:
      return [];
  }
}

function Table(props) {
  const {data, onSelect, fields, selectedId} = props;
  const rows = data.map(data => {
    const cells = [];
    for (const field of fields) {
      cells.push(
        <View key={field} style={{padding: 5}}>
          <Text>{String(data[field])}</Text>
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
