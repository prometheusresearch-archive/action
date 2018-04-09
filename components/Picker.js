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
    backgroundColor: cfg.color.white,
    padding: cfg.padding.size2,
    borderWidth: 2,
    borderRadius: cfg.borderRadius.small,
    outlineWidth: 6,
    fontSize: cfg.fontSize.small,
    fontWeight: cfg.fontWeight.medium,
    fontFamily: cfg.fontFamily.sans,
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
    borderColor: props.color,
    outlineColor: focusRingColor,
  };
  let options = props.options.map(option => (
    <PickerBase.Item label={option.label} value={option.value} />
  ));
  return (
    <View>
      <PickerBase {...props} style={[style.base, colorStyle]}>
        {options}
      </PickerBase>
      <View style={style.icon}>
        <svg
          class="fill-current h-4 w-4"
          xmlns="http://www.w3.org/2000/svg"
          viewBox="0 0 20 20">
          <path d="M9.293 12.95l.707.707L15.657 8l-1.414-1.414L10 10.828 5.757 6.586 4.343 8z" />
        </svg>
      </View>
    </View>
  );
}

Picker.defaultProps = {
  color: cfg.color.black,
};

export const PickerItem = Picker.Item;