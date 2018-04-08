/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import * as cfg from './config.js';

import {createShowcaseList} from './FixtureUtil.js';

function SideNav({renderNav, active}) {
  const nav = renderNav();
  return (
    <View>
      {nav.map(item => (
        <SideNavElement active={active === item.id}>{item.element}</SideNavElement>
      ))}
    </View>
  );
}

function SideNavElement({children, active}) {
  const style = {
    paddingVertical: cfg.padding.size2,
    paddingHorizontal: cfg.padding.size4,
    borderLeftColor: active ? cfg.color.black : cfg.color.transparent,
    borderLeftWidth: cfg.borderWidth.size2,
  };
  return <View style={style}>{children}</View>;
}

function SideNavButton({label}) {
  const textStyle = {
    color: cfg.color.black,
    fontWeight: cfg.fontWeight.semibold,
  };
  return (
    <View>
      <TouchableOpacity>
        <Text style={textStyle}>{label}</Text>
      </TouchableOpacity>
    </View>
  );
}

const renderNavOne = () => [{element: <SideNavButton label="Home" />, id: 'home'}];

const renderNavMultiple = () => [
  {element: <SideNavButton label="Home" />, id: 'home'},
  {element: <SideNavButton label="Tutorial" />, id: 'tutorial'},
  {element: <SideNavButton label="Documentation" />, id: 'documentation'},
  {element: <SideNavButton label="About" />, id: 'about'},
];

const oneElement = {
  title: 'One Element',
  element: <SideNav renderNav={renderNavOne} />,
};

const multipleElements = {
  title: 'Multiple Elements',
  element: <SideNav renderNav={renderNavMultiple} />,
};

const withActive = {
  title: 'Multiple Elements',
  element: <SideNav renderNav={renderNavMultiple} active="tutorial" />,
};

const displayName = SideNav.displayName || SideNav.name;
const ShowcaseList = createShowcaseList(displayName);

export default {
  component: ShowcaseList,
  props: {
    rows: [oneElement, multipleElements, withActive],
  },
};
