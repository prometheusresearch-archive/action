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
} from 'react-native-web';
import * as cfg from './config.js';

const emptyFunction = () => {};

type NavItem = {
  id: string,
  render: (props: {
    key: string,
    outlineColor: string,
    active: boolean,
    onPress: () => void,
  }) => React.Node,
};

type ReactNodeItem = {type: 'ReactNode', node: React.Node};

type P<Item: NavItem> = {
  items: Array<Item | ReactNodeItem>,
  outlineColor: string,
  active?: string,
  onActive?: Item => void,
};

export function SideNav<Item: NavItem>({items, active, onActive, outlineColor}: P<Item>) {
  return (
    <View>
      {items.map(item => {
        if (item.type === 'ReactNode') {
          // $FlowFixMe: ...
          return item.node;
        } else {
          return item.render({
            key: item.id,
            outlineColor,
            active: active === item.id,
            onPress: onActive != null ? onActive.bind(null, item) : emptyFunction,
          });
        }
      })}
    </View>
  );
}

SideNav.defaultProps = {
  outlineColor: cfg.color.black,
};

type SideNavButtonProps = {
  label: string,
  badge?: React.Node,
  icon?: React.Node,
  outlineColor: string,
  onPress: () => void,
  active: boolean,
};

export function SideNavButton({
  active,
  label,
  badge,
  icon,
  outlineColor,
  onPress,
}: SideNavButtonProps) {
  const textStyle = {
    color: outlineColor,
    // fontWeight: cfg.fontWeight.semibold,
    fontSize: active ? cfg.fontSize.base : cfg.fontSize.small,
    fontWeight: active ? cfg.fontWeight.black : cfg.fontWeight.normal,
  };
  const viewStyle = {
    height: 34,
    flexDirection: 'row',
    alignItems: 'center',
    borderRadius: cfg.borderRadius.default,
    paddingVertical: cfg.padding.size2,
    paddingHorizontal: cfg.padding.size4,
  };
  const iconWrapperStyle = {
    width: 30,
  };
  return (
    <View>
      <TouchableHighlight
        disabled={Boolean(active)}
        onPress={onPress}
        underlayColor={processColor(outlineColor, 0.1)}
        delayPressOut={0}
        style={{
          borderRadius: cfg.borderRadius.default,
          // backgroundColor: active
          //   ? processColor(outlineColor, 0.25)
          //   : cfg.color.transparent,
        }}>
        <View style={viewStyle}>
          <View style={iconWrapperStyle}>{icon}</View>
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

export function SideNavDivider() {
  let style = {
    height: cfg.height.size6,
  };
  return <View style={style} />;
}

export const divider = {
  type: 'ReactNode',
  node: <SideNavDivider />,
};
