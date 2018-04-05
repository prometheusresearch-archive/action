/**
 * @flow strict
 */

import * as React from 'react';
import {View, Text, TouchableHighlight} from 'react-native-web';
import * as W from 'workflow';
import type {State, Args, Meta} from 'workflow';
import {ScreenTitle} from './ScreenTitle.js';

type P = {
  state: State,
  args: Args,
  toolbar: React.Node,
  onPick: mixed => void,
};

export function Pick(props: P) {
  const {title, data, metadata: meta} = W.query(
    props.state,
    `
      {
        title: title,
        data: dataForUI,
        metadata: dataForUI:meta,
      }
    `,
  );
  const onSelect = id => {
    props.onPick(id);
  };
  const fields = fieldsFromMeta(meta);
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

function fieldsFromMeta(meta: Meta) {
  const {type: {type}, registry} = meta;
  switch (type.type) {
    case 'entity': {
      const fields = [];
      for (const name in registry[type.name].fields) {
        const fieldType = registry[type.name].fields[name].type;
        if (fieldType.type !== 'entity') {
          fields.push(name);
        }
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
