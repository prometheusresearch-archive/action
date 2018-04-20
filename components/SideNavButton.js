/**
 * @flow
 */

import * as React from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  TouchableHighlight,
  processColor,
  StyleSheet,
} from 'react-native-web';
import * as cfg from './config.js';

type P = {
  label: string,
  badge?: React.Node,
  icon?: React.Node,
  outlineColor: string,
  onPress: () => void,
  active: boolean,
};

let style = StyleSheet.create({
  viewStyle: {
    height: 34,
    flexDirection: 'row',
    alignItems: 'center',
    borderRadius: cfg.borderRadius.small,
    paddingVertical: cfg.padding.size2,
    paddingHorizontal: cfg.padding.size4,
  },
  iconWrapperStyle: {
    width: 30,
  },
});

export function SideNavButton({active, label, badge, icon, outlineColor, onPress}: P) {
  const textStyle = {
    color: outlineColor,
    // fontWeight: cfg.fontWeight.semibold,
    fontSize: active ? cfg.fontSize.base : cfg.fontSize.small,
    fontWeight: active ? cfg.fontWeight.black : cfg.fontWeight.normal,
  };
  return (
    <View>
      <TouchableHighlight
        disabled={Boolean(active)}
        onPress={onPress}
        underlayColor={processColor(outlineColor, 0.1)}
        delayPressOut={0}
        style={{
          borderRadius: cfg.borderRadius.small,
        }}>
        <View style={style.viewStyle}>
          <View style={style.iconWrapperStyle}>{icon}</View>
          <View style={{flexGrow: 1}}>
            <Text style={textStyle}>{label}</Text>
          </View>
          {badge != null && <View>{badge}</View>}
        </View>
      </TouchableHighlight>
    </View>
  );
}

SideNavButton.defaultProps = {
  outlineColor: cfg.color.black,
};
