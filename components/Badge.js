/**
 * @flow
 */

import * as React from 'react';
import {View, Text} from 'react-native-web';
import * as cfg from './config.js';

type P = {
  children: React.Node,
  backgroundColor: string,
  textColor: string,
};

export function Badge({children, backgroundColor, textColor}: P) {
  return (
    <View
      style={{
        backgroundColor,
        padding: cfg.padding.size1,
        borderRadius: cfg.borderRadius.default,
      }}>
      <Text
        style={{
          color: textColor,
          fontSize: cfg.fontSize.xSmall,
          fontWeight: cfg.fontWeight.semibold,
        }}>
        {children}
      </Text>
    </View>
  );
}

Badge.defaultProps = {
  backgroundColor: cfg.color.black,
  textColor: cfg.color.white,
};
