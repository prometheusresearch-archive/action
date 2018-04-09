/**
 * @flow
 */

import * as React from 'react';
import {TouchableOpacity, View} from 'react-native-web';
import {MdExitToApp} from 'react-icons/lib/md';

import {Nav, NavButton} from './Nav.js';
import {OutlineButton} from './OutlineButton.js';
import * as cfg from './config.js';
import {type FixtureList, createShowcaseList, StateContainer} from './FixtureUtil.js';

const ShowcaseList = createShowcaseList(Nav);

const items = [
  {id: 'home', render: props => <NavButton {...props} title="Home" />},
  {id: 'documentation', render: props => <NavButton {...props} title="Documentation" />},
  {id: 'publications', render: props => <NavButton {...props} title="Publications" />},
];

const itemsExtra = [
  {
    id: 'exit',
    render: props => (
      <TouchableOpacity {...props}>
        <MdExitToApp />
      </TouchableOpacity>
    ),
  },
];

const noNav = {
  title: 'No Nav',
  element: <Nav breadcrumb={[]} />,
};

const withNav = {
  title: 'With Nav',
  element: <Nav breadcrumb={[]} items={items} />,
};

const withNavExtra = {
  title: 'With Nav',
  element: <Nav breadcrumb={[]} items={items} itemsExtra={itemsExtra} />,
};

const withBreadcrumb = {
  title: 'With Breadcrumb',
  element: (
    <Nav
      items={items}
      breadcrumb={[
        {
          id: 'docs',
          title: 'Documentation',
        },
        {
          id: 'api',
          title: 'API References',
        },
        {
          id: 'combinators',
          title: 'Combinators',
        },
      ]}
    />
  ),
};

const withActive = {
  title: 'On background',
  element: (
    <View style={{backgroundColor: cfg.color.white}}>
      <Nav
        items={items}
        active="documentation"
        breadcrumb={[
          {
            id: 'docs',
            title: 'Documentation',
          },
          {
            id: 'api',
            title: 'API References',
          },
          {
            id: 'combinators',
            title: 'Combinators',
          },
        ]}
      />
    </View>
  ),
};

const onBg = {
  title: 'On background',
  element: (
    <View style={{backgroundColor: cfg.color.white}}>
      <Nav
        items={items}
        active="documentation"
        breadcrumb={[
          {
            id: 'docs',
            title: 'Documentation',
          },
          {
            id: 'api',
            title: 'API References',
          },
          {
            id: 'combinators',
            title: 'Combinators',
          },
        ]}
      />
    </View>
  ),
};

const customOutlineColor = {
  title: 'Custom Outline Color',
  element: (
    <Nav
      items={items}
      outlineColor={cfg.color.indigo}
      active="documentation"
      breadcrumb={[
        {
          id: 'docs',
          title: 'Documentation',
        },
        {
          id: 'api',
          title: 'API References',
        },
        {
          id: 'combinators',
          title: 'Combinators',
        },
      ]}
    />
  ),
};

const customOutlineColorOnBg = {
  title: 'Custom Outline Color (on background)',
  element: (
    <View style={{backgroundColor: cfg.color.pinkLightest}}>
      <Nav
        items={items}
        outlineColor={cfg.color.indigo}
        active="documentation"
        breadcrumb={[
          {
            id: 'docs',
            title: 'Documentation',
          },
          {
            id: 'api',
            title: 'API References',
          },
          {
            id: 'combinators',
            title: 'Combinators',
          },
        ]}
      />
    </View>
  ),
};

const stateful = {
  title: 'Stateful',
  element: (
    <View style={{backgroundColor: cfg.color.pinkLightest}}>
      <StateContainer initialState={{active: 'home'}}>
        {({state, onState}) => (
          <Nav
            items={items}
            outlineColor={cfg.color.indigo}
            active={state.active}
            onActive={ev => onState({active: ev.id})}
            breadcrumb={[
              {
                id: 'docs',
                title: 'Documentation',
              },
              {
                id: 'api',
                title: 'API References',
              },
              {
                id: 'combinators',
                title: 'Combinators',
              },
            ]}
          />
        )}
      </StateContainer>
    </View>
  ),
};

const fixtures: FixtureList = [
  {
    name: 'Showcase',
    component: ShowcaseList,
    props: {
      rows: [
        noNav,
        withNav,
        withNavExtra,
        withBreadcrumb,
        withActive,
        onBg,
        customOutlineColor,
        customOutlineColorOnBg,
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

export default fixtures;
