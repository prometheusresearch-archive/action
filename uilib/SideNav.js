/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import * as cfg from './config.js';

type NavItem = {
  id: string,
  element: React.Node,
};

type P = {
  renderNav: (props: {outlineColor: string}) => Array<NavItem>,
  outlineColor: string,
  active?: string,
};

export function SideNav({renderNav, active, outlineColor}: P) {
  const nav = renderNav({outlineColor});
  return (
    <View>
      {nav.map(item => (
        <SideNavElement outlineColor={outlineColor} active={active === item.id}>
          {item.element}
        </SideNavElement>
      ))}
    </View>
  );
}

SideNav.defaultProps = {
  outlineColor: cfg.color.black,
};

function SideNavElement({children, active, outlineColor = cfg.color.black}) {
  const style = {
    paddingHorizontal: cfg.padding.size4,
    borderLeftColor: active ? outlineColor : cfg.color.transparent,
    borderLeftWidth: cfg.borderWidth.size2,
  };
  return <View style={style}>{children}</View>;
}

type SideNavProps = {
  label: string,
  badge?: React.Node,
  outlineColor: string,
};

export function SideNavButton({label, badge, outlineColor}: SideNavProps) {
  const textStyle = {
    color: outlineColor,
    fontWeight: cfg.fontWeight.semibold,
  };
  return (
    <View>
      <TouchableOpacity style={{flexDirection: 'row', alignItems: 'center'}}>
        <View style={{padding: cfg.padding.size2, flexGrow: 1}}>
          <Text style={textStyle}>{label}</Text>
        </View>
        {badge != null && <View>{badge}</View>}
      </TouchableOpacity>
    </View>
  );
}

SideNavButton.defaultProps = {
  outlineColor: cfg.color.black,
};
