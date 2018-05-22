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
    backgroundColor: cfg.color.transparent,
    padding: cfg.padding.size2,
    borderWidth: cfg.borderWidth.default,
    borderRadius: cfg.borderRadius.small,
    fontWeight: cfg.fontWeight.normal,
    fontFamily: cfg.fontFamily.sans,
    fontSize: cfg.fontSize.small,
  },
  error: {
    borderColor: cfg.color.red,
  },
  monospace: {
    fontFamily: cfg.fontFamily.mono,
  },
});

// $FlowFixMe: get the props type from react-native-web
type P = any;

const focusRingColor = cfg.color.blue;

export function TextInput(
  {monospace, error, outlineColor, ...props}: P,
  ref: React.Ref<*>,
) {
  const colorStyle = {
    borderColor: outlineColor,
    outlineColor: focusRingColor,
  };
  return (
    <TextInputBase
      {...props}
      ref={ref}
      style={[style.base, monospace && style.monospace, colorStyle, error && style.error]}
    />
  );
}

TextInput.defaultProps = {
  outlineColor: cfg.color.black,
};

// $FlowFixMe: update to new Flow which has it
TextInput = React.forwardRef(TextInput);

TextInput.displayName = 'TextInput';
