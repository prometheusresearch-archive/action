/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import * as cfg from './config.js';

import {createShowcaseList} from './FixtureUtil.js';

function SideNav({renderNav, active, outlineColor}) {
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
};

function SideNavButton({label, badge, outlineColor}: SideNavProps) {
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

function Badge({children, backgroundColor, textColor}) {
  return (
    <View
      style={{
        backgroundColor,
        padding: cfg.padding.size1,
        borderRadius: cfg.borderRadius.default,
      }}>
      <Text
        style={{
          color: textColor,
          fontSize: cfg.fontSize.xSmall,
          fontWeight: cfg.fontWeight.semibold,
        }}>
        {children}
      </Text>
    </View>
  );
}
Badge.defaultProps = {
  backgroundColor: cfg.color.black,
  textColor: cfg.color.white,
};

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
  element: (
    <View style={{width: 300}}>
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
  {element: <SideNavButton outlineColor={outlineColor} label="About" />, id: 'about'},
];

const withBadges = {
  title: 'With Badges',
  element: (
    <View style={{width: 300}}>
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

const displayName = SideNav.displayName || SideNav.name;
const ShowcaseList = createShowcaseList(displayName);

export default {
  component: ShowcaseList,
  props: {
    rows: [oneElement, multipleElements, withActive, withBadges, withCustomColor],
  },
};
