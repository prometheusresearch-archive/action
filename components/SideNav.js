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
export {SideNavButton} from './SideNavButton.js';

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
