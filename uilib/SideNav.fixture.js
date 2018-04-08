/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import * as cfg from './config.js';
import {Badge} from './Badge.js';
import {SideNav, SideNavButton} from './SideNav.js';

import {createShowcaseList} from './FixtureUtil.js';

const renderNavOne = () => [{element: <SideNavButton label="Home" />, id: 'home'}];

const renderNavMultiple = () => [
  {element: <SideNavButton label="Home" />, id: 'home'},
  {element: <SideNavButton label="Tutorial" />, id: 'tutorial'},
  {element: <SideNavButton label="Documentation" />, id: 'documentation'},
  {element: <SideNavButton label="About" />, id: 'about'},
];

const oneElement = {
  title: 'One Element',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav renderNav={renderNavOne} />
    </View>
  ),
};

const multipleElements = {
  title: 'Multiple Elements',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav renderNav={renderNavMultiple} />
    </View>
  ),
};

const withActive = {
  title: 'Multiple Elements',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav renderNav={renderNavMultiple} active="tutorial" />
    </View>
  ),
};

const renderNavWithBadges = ({outlineColor}) => [
  {element: <SideNavButton outlineColor={outlineColor} label="Home" />, id: 'home'},
  {
    element: (
      <SideNavButton
        outlineColor={outlineColor}
        label="Individuals"
        badge={
          <Badge backgroundColor={cfg.color.redLight} textColor={cfg.color.white}>
            42
          </Badge>
        }
      />
    ),
    id: 'individuals',
  },
  {
    element: <SideNavButton outlineColor={outlineColor} label="Tutorial" />,
    id: 'tutorial',
  },
  {
    element: (
      <SideNavButton
        outlineColor={outlineColor}
        label="Documentation"
        badge={
          <Badge backgroundColor={outlineColor} textColor={cfg.color.white}>
            NEW
          </Badge>
        }
      />
    ),
    id: 'documentation',
  },
  {
    element: <SideNavButton outlineColor={outlineColor} label="About" />,
    id: 'about',
  },
];

const withBadges = {
  title: 'With Badges',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav renderNav={renderNavWithBadges} active="individuals" />
    </View>
  ),
};

const withCustomColor = {
  title: 'With Custom Color',
  element: (
    <View
      style={{
        width: 300,
        padding: cfg.padding.size6,
      }}>
      <SideNav
        outlineColor={cfg.color.indigo}
        renderNav={renderNavWithBadges}
        active="individuals"
      />
    </View>
  ),
};

const withCustomColorBg = {
  title: 'With Custom Color (on background)',
  element: (
    <View
      style={{
        width: 300,
        padding: cfg.padding.size6,
        backgroundColor: cfg.color.pinkLightest,
      }}>
      <SideNav
        outlineColor={cfg.color.indigo}
        renderNav={renderNavWithBadges}
        active="individuals"
      />
    </View>
  ),
};

const ShowcaseList = createShowcaseList(SideNav);

export default {
  component: ShowcaseList,
  props: {
    rows: [
      oneElement,
      multipleElements,
      withActive,
      withBadges,
      withCustomColor,
      withCustomColorBg,
    ],
  },
};
