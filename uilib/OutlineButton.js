/**
 * @flow
 */

import {View, Text, TouchableOpacity, StyleSheet} from 'react-native-web';
import * as React from 'react';
import * as cfg from './config.js';

type P = {
  label: string,
  size: 'small' | 'medium' | 'large',
  strokeColor?: string,
  fillColor?: string,
};

const style = StyleSheet.create({
  view: {
    backgroundColor: 'transparent',
    borderWidth: cfg.borderWidth.size2,
    borderRadius: cfg.borderRadius.default,
    borderStyle: 'solid',
    flexShrink: 1,
  },
  viewSizeSmall: {
    paddingHorizontal: cfg.padding.size2,
    paddingVertical: cfg.padding.size1,
    borderWidth: cfg.borderWidth.size2,
  },
  viewSizeMedium: {
    paddingHorizontal: cfg.padding.size3,
    paddingVertical: cfg.padding.size1,
    borderWidth: cfg.borderWidth.size2,
  },
  viewSizeLarge: {
    paddingHorizontal: cfg.padding.size4,
    paddingVertical: cfg.padding.size2,
    borderWidth: cfg.borderWidth.size2,
  },

  text: {
    color: cfg.color.black,
    fontFamily: cfg.fontFamily.sans,
    textAlign: 'center',
  },
  textSizeSmall: {
    fontWeight: cfg.fontWeight.semibold,
    fontSize: cfg.fontSize.xSmall,
  },
  textSizeMedium: {
    fontWeight: cfg.fontWeight.semibold,
  },
  textSizeLarge: {
    fontSize: cfg.fontSize.large,
    fontWeight: cfg.fontWeight.semibold,
  },
});

export function OutlineButton({label, size, strokeColor, fillColor}: P) {
  return (
    <TouchableOpacity
      activeOpacity={0.4}
      style={[
        style.view,
        size === 'small' && style.viewSizeSmall,
        size === 'medium' && style.viewSizeMedium,
        size === 'large' && style.viewSizeLarge,
        strokeColor != null && {borderColor: strokeColor},
        fillColor != null && {backgroundColor: fillColor},
      ]}>
      <Text
        style={[
          style.text,
          size === 'small' && style.textSizeSmall,
          size === 'medium' && style.textSizeMedium,
          size === 'large' && style.textSizeLarge,
          strokeColor != null && {color: strokeColor},
        ]}>
        {label}
      </Text>
    </TouchableOpacity>
  );
}

OutlineButton.defaultProps = {size: 'medium'};
