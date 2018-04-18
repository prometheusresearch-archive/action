/**
 * @flow
 */

import {View, Text, TouchableOpacity, StyleSheet, type Style} from 'react-native-web';
import * as React from 'react';
import * as cfg from './config.js';

type P = {
  label: string,
  size: 'small' | 'medium' | 'large',
  outlineColor?: string,
  fillColor?: string,
  style?: ?Style,
  onPress?: () => void,
};

const style = StyleSheet.create({
  view: {
    backgroundColor: 'transparent',
    borderWidth: cfg.borderWidth.default,
    borderRadius: cfg.borderRadius.small,
    borderStyle: 'solid',
    justifyContent: 'center',
    flexShrink: 1,
  },
  viewSizeSmall: {
    paddingHorizontal: cfg.padding.size2,
    paddingVertical: cfg.padding.size1,
    borderWidth: cfg.borderWidth.default,
  },
  viewSizeMedium: {
    paddingHorizontal: cfg.padding.size3,
    paddingVertical: cfg.padding.size2,
    borderWidth: cfg.borderWidth.default,
  },
  viewSizeLarge: {
    paddingHorizontal: cfg.padding.size6,
    paddingVertical: cfg.padding.size3,
    borderWidth: cfg.borderWidth.default,
  },

  text: {
    color: cfg.color.black,
    fontFamily: cfg.fontFamily.sans,
    textAlign: 'center',
  },
  textSizeSmall: {
    fontWeight: cfg.fontWeight.medium,
    fontSize: cfg.fontSize.xSmall,
  },
  textSizeMedium: {
    fontWeight: cfg.fontWeight.medium,
  },
  textSizeLarge: {
    fontSize: cfg.fontSize.large,
    fontWeight: cfg.fontWeight.medium,
  },
});

export function OutlineButton({
  label,
  size,
  outlineColor,
  fillColor,
  style: extraStyle,
  onPress,
}: P) {
  return (
    <TouchableOpacity
      onPress={onPress}
      activeOpacity={0.4}
      style={[
        style.view,
        size === 'small' && style.viewSizeSmall,
        size === 'medium' && style.viewSizeMedium,
        size === 'large' && style.viewSizeLarge,
        outlineColor != null && {borderColor: outlineColor},
        fillColor != null && {backgroundColor: fillColor},
        extraStyle,
      ]}>
      <Text
        style={[
          style.text,
          size === 'small' && style.textSizeSmall,
          size === 'medium' && style.textSizeMedium,
          size === 'large' && style.textSizeLarge,
          outlineColor != null && {color: outlineColor},
        ]}>
        {label}
      </Text>
    </TouchableOpacity>
  );
}

OutlineButton.defaultProps = {size: 'medium'};
