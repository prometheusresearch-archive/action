/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableHighlight} from 'react-native-web';
import * as W from 'workflow';
import type {Query} from 'workflow';
import {Error} from './Error.js';
import {ScreenTitle} from './ScreenTitle.js';

type P = {
  query: Query,
};

export function Pick(props: P) {
  const data = W.runQuery(props.query);
  const onSelect = id => {
    console.log(id);
  };
  if (data.type === 'Error') {
    return <Error error={data.error} />;
  } else {
    return (
      <View>
        <ScreenTitle>Pick</ScreenTitle>
        <View>
          <Table data={data.value} onSelect={onSelect} />
        </View>
      </View>
    );
  }
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
      <TouchableHighlight key={data.id} underlayColor="yellow" onPress={onSelect.bind(null, data.id)}>
        <View style={{flexDirection: 'row'}}>
          {cells}
        </View>
      </TouchableHighlight>
    );
  });
  return <View>{rows}</View>;
}
