/**
 * @flow
 */

import * as React from 'react';
import * as ReactNative from 'react-native-web';
import * as W from 'workflow';
import type {Query, UntypedQuery} from 'workflow';
import {Error} from './Error.js';
import {ScreenTitle} from './ScreenTitle.js';

type P = {
  query: Query,
  onQuery: UntypedQuery => void,
};

export function View(props: P) {
  const data = W.runQuery(props.query);
  if (data.type === 'Error') {
    return <Error error={data.error} />;
  } else {
    return (
      <ReactNative.View>
        <ScreenTitle>View</ScreenTitle>
        <ReactNative.View>
          <ReactNative.Text>{JSON.stringify(data.value, null, 2)}</ReactNative.Text>
        </ReactNative.View>
      </ReactNative.View>
    );
  }
}
