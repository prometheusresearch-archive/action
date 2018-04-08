/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import * as cfg from './config.js';
import {Badge} from './Badge.js';
import {SideNav, SideNavButton, SideNavDivider} from './SideNav.js';
import {MdHome, MdPeople} from 'react-icons/lib/md';
import {createShowcaseList, StateContainer} from './FixtureUtil.js';

const itemsSingle = [
  {render: props => <SideNavButton {...props} label="Home" />, id: 'home'},
];

const itemsMultiple = [
  {
    render: props => <SideNavButton {...props} label="Home" />,
    id: 'home',
  },
  {
    render: props => <SideNavButton {...props} label="Tutorial" />,
    id: 'tutorial',
  },
  {
    render: props => <SideNavButton {...props} label="Documentation" />,
    id: 'documentation',
  },
  {
    render: props => <SideNavButton {...props} label="About" />,
    id: 'about',
  },
];

const oneElement = {
  title: 'One Element',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav items={itemsSingle} />
    </View>
  ),
};

const multipleElements = {
  title: 'Multiple Elements',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav items={itemsMultiple} />
    </View>
  ),
};

const withActive = {
  title: 'With Active',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav items={itemsMultiple} active="tutorial" />
    </View>
  ),
};

const itemsWithIcon = [
  {
    render: props => <SideNavButton {...props} icon={<MdHome />} label="Home" />,
    id: 'home',
  },
  {
    render: props => <SideNavButton {...props} label="Tutorial" />,
    id: 'tutorial',
  },
  {
    render: props => <SideNavButton {...props} label="Documentation" />,
    id: 'documentation',
  },
  {
    render: props => <SideNavButton {...props} label="About" />,
    id: 'about',
  },
];

const withIcon = {
  title: 'With Icon',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav items={itemsWithIcon} active="tutorial" />
    </View>
  ),
};

const itemsWithBadge = [
  {
    render: props => (
      <SideNavButton
        {...props}
        icon={<MdHome color={props.outlineColor} />}
        label="Home"
      />
    ),
    id: 'home',
  },
  {
    render: props => (
      <SideNavButton
        {...props}
        label="Individuals"
        icon={<MdPeople color={props.outlineColor} />}
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
    render: props => <SideNavButton {...props} label="Tutorial" />,
    id: 'tutorial',
  },
  {
    render: props => (
      <SideNavButton
        {...props}
        label="Documentation"
        badge={
          <Badge backgroundColor={props.outlineColor} textColor={cfg.color.white}>
            NEW
          </Badge>
        }
      />
    ),
    id: 'documentation',
  },
  {
    render: props => <SideNavButton {...props} label="About" />,
    id: 'about',
  },
];

const withBadges = {
  title: 'With Badges',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav items={itemsWithBadge} active="individuals" />
    </View>
  ),
};

const itemsWithDivider = [
  {
    render: props => (
      <SideNavButton
        {...props}
        icon={<MdHome color={props.outlineColor} />}
        label="Home"
      />
    ),
    id: 'home',
  },
  {
    render: props => (
      <SideNavButton
        {...props}
        label="Individuals"
        icon={<MdPeople color={props.outlineColor} />}
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
    render: props => <SideNavButton {...props} label="Tutorial" />,
    id: 'tutorial',
  },
  {node: <SideNavDivider key="divider" />},
  {
    render: props => (
      <SideNavButton
        {...props}
        label="Documentation"
        badge={
          <Badge backgroundColor={props.outlineColor} textColor={cfg.color.white}>
            NEW
          </Badge>
        }
      />
    ),
    id: 'documentation',
  },
  {
    render: props => <SideNavButton {...props} label="About" />,
    id: 'about',
  },
];

const withDivider = {
  title: 'With Divider',
  element: (
    <View style={{width: 300, padding: cfg.padding.size6}}>
      <SideNav items={itemsWithDivider} active="individuals" />
    </View>
  ),
};

const onWhiteBg = {
  title: 'On white background',
  element: (
    <View
      style={{
        width: 300,
        padding: cfg.padding.size6,
        backgroundColor: cfg.color.white,
      }}>
      <SideNav items={itemsWithBadge} active="individuals" />
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
        items={itemsWithBadge}
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
        items={itemsWithBadge}
        active="individuals"
      />
    </View>
  ),
};

const stateful = {
  title: 'Stateful',
  element: (
    <View
      style={{
        width: 300,
        padding: cfg.padding.size6,
        backgroundColor: cfg.color.pinkLightest,
      }}>
      <StateContainer initialState={{active: 'home'}}>
        {({state, onState}) => (
          <SideNav
            outlineColor={cfg.color.indigo}
            items={itemsWithBadge}
            active={state.active}
            onActive={ev => onState({active: ev.id})}
          />
        )}
      </StateContainer>
    </View>
  ),
};

const ShowcaseList = createShowcaseList(SideNav);

export default [
  {
    name: 'Showcase',
    component: ShowcaseList,
    props: {
      rows: [
        oneElement,
        multipleElements,
        withActive,
        withIcon,
        withBadges,
        withDivider,
        onWhiteBg,
        withCustomColor,
        withCustomColorBg,
      ],
    },
  },
  {
    name: 'Stateful',
    component: ShowcaseList,
    props: {
      rows: [stateful],
    },
  },
];
