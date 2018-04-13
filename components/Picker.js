/**
 * @flow
 */

import * as React from 'react';
import {
  View,
  Text,
  Picker as PickerBase,
  StyleSheet,
  processColor,
} from 'react-native-web';
import * as cfg from './config.js';

let style = StyleSheet.create({
  base: {
    appearance: 'none',
    backgroundColor: cfg.color.transparent,
    padding: cfg.padding.size2,
    borderWidth: cfg.borderWidth.default,
    borderRadius: cfg.borderRadius.small,
    fontSize: cfg.fontSize.small,
    fontWeight: cfg.fontWeight.medium,
    fontFamily: cfg.fontFamily.sans,
  },
  error: {
    borderColor: cfg.color.red,
  },
  icon: {
    width: 20,
    position: 'absolute',
    top: 0,
    bottom: 0,
    right: cfg.padding.size2,
    justifyContent: 'center',
  },
});

// $FlowFixMe: get the props type from react-native-web
type P = any;

const focusRingColor = cfg.color.blue;

export function Picker(props: P) {
  let colorStyle = {
    borderColor: props.outlineColor,
    outlineColor: focusRingColor,
  };
  let options = props.options.map(option => (
    <PickerBase.Item key={option.value} label={option.label} value={option.value} />
  ));
  return (
    <View>
      <PickerBase {...props} style={[style.base, colorStyle, props.error && style.error]}>
        {options}
      </PickerBase>
      <View style={style.icon}>
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20">
          <path d="M9.293 12.95l.707.707L15.657 8l-1.414-1.414L10 10.828 5.757 6.586 4.343 8z" />
        </svg>
      </View>
    </View>
  );
}

Picker.defaultProps = {
  outlineColor: cfg.color.black,
};

export const PickerItem = Picker.Item;
