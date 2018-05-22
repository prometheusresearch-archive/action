/**
 * @flow strict
 */

import * as React from 'react';
import {Text} from 'react-native-web';
import * as cfg from 'components/config';

type P = {
  children: React.Node,
};

export function ScreenTitle(props: P) {
  return (
    <Text
      style={{
        paddingVertical: cfg.padding.size4,
        fontSize: cfg.fontSize.xxLarge,
        fontWeight: cfg.fontWeight.bold,
      }}>
      {props.children}
    </Text>
  );
}
