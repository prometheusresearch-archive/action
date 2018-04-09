/**
 * @flow
 */

import * as React from 'react';
import {
  View,
  Text,
  TextInput as TextInputBase,
  StyleSheet,
  processColor,
} from 'react-native-web';
import * as cfg from './config.js';

let style = StyleSheet.create({
  base: {
    padding: cfg.padding.size2,
    borderWidth: 2,
    borderRadius: cfg.borderRadius.small,
    outlineWidth: 6,
    fontWeight: cfg.fontWeight.medium,
    fontFamily: cfg.fontFamily.sans,
  },
  monospace: {
    fontFamily: cfg.fontFamily.mono,
  },
});

// $FlowFixMe: get the props type from react-native-web
type P = any;

const focusRingColor = cfg.color.blue;

export function TextInput(props: P) {
  const colorStyle = {
    borderColor: props.color,
    outlineColor: focusRingColor,
  };
  return (
    <TextInputBase
      {...props}
      style={[style.base, props.monospace && style.monospace, colorStyle]}
    />
  );
}

TextInput.defaultProps = {
  color: cfg.color.black,
};
