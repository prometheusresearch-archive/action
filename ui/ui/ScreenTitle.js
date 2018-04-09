/**
 * @flow strict
 */

import * as React from 'react';
import {Text} from 'react-native-web';

type P = {
  children: React.Node,
};

export function ScreenTitle(props: P) {
  return (
    <Text style={{padding: 10, fontSize: '18pt', fontWeight: '700'}}>
      {props.children}
    </Text>
  );
}