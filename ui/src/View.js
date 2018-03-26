/**
 * @flow
 */

import * as React from 'react';
import * as ReactNative from 'react-native-web';
import * as W from 'workflow';
import type {State} from 'workflow';
import {ScreenTitle} from './ScreenTitle.js';

type P = {
  state: State,
  toolbar: React.Node,
};

export function View(props: P) {
  const data = W.getData(props.state);
  const title = W.getTitle(props.state);
  return (
    <ReactNative.View>
      <ScreenTitle>{title}</ScreenTitle>
      <ReactNative.View>{props.toolbar}</ReactNative.View>
      <ReactNative.View>
        <Data data={data} />
      </ReactNative.View>
    </ReactNative.View>
  );
}

function Data({data}) {
  if (typeof data === 'string') {
    return <ReactNative.Text>{String(data)}</ReactNative.Text>;
  } else if (typeof data === 'number') {
    return <ReactNative.Text>{String(data)}</ReactNative.Text>;
  } else if (typeof data === 'boolean') {
    return <ReactNative.Text>{String(data)}</ReactNative.Text>;
  } else if (data == null) {
    return <ReactNative.Text>-</ReactNative.Text>;
  } else if (Array.isArray(data)) {
    return data.map((data, idx) => <Data key={idx} data={data} />);
  } else {
    const fields = [];
    for (const name in data) {
      fields.push(
        <ReactNative.View
          key={name}
          style={{flexDirection: 'row', padding: 5, alignItems: 'center'}}>
          <ReactNative.View style={{flex: 1}}>
            <ReactNative.Text style={{fontWeight: '600'}}>{name}</ReactNative.Text>
          </ReactNative.View>
          <ReactNative.View style={{flex: 3}}>
            <Data data={data[name]} />
          </ReactNative.View>
        </ReactNative.View>,
      );
    }
    return (
      <ReactNative.View style={{flexDirection: 'column', padding: 5, maxWidth: 800}}>
        {fields}
      </ReactNative.View>
    );
  }
}
