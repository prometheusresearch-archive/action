/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import type {FixtureList} from './FixtureUtil.js';
import {OutlineButton} from './OutlineButton.js';
import {Breadcrumb} from './Breadcrumb.js';
import * as cfg from './config.js';
import {
  MdHome,
  MdExitToApp,
  MdHelpOutline,
  MdKeyboardArrowRight,
} from 'react-icons/lib/md';

const borderWidth = cfg.borderWidth.size2;

function NavTitle({title, textColor}) {
  return (
    <View>
      <TouchableOpacity>
        <Text
          style={{
            color: textColor,
            fontSize: cfg.fontSize.xLarge,
            fontWeight: cfg.fontWeight.extrabold,
          }}>
          {title}
        </Text>
      </TouchableOpacity>
    </View>
  );
}

function NavElement({children}) {
  return (
    <View
      style={{
        justifyContent: 'center',
        paddingHorizontal: cfg.padding.size2,
      }}>
      {children}
    </View>
  );
}

function NavButton({title, textColor}) {
  return (
    <View
      style={{
        flexDirection: 'row',
        alignItems: 'center',
      }}>
      <TouchableOpacity>
        <Text style={{color: textColor, fontWeight: cfg.fontWeight.bold}}>{title}</Text>
      </TouchableOpacity>
    </View>
  );
}

function Nav(props) {
  const borderColor = props.outlineColor;
  const textColor = props.outlineColor;
  return (
    <View style={{padding: cfg.padding.size4}}>
      <View
        style={{
          borderWidth,
          borderColor,
          borderRadius: cfg.borderRadius.small,
        }}>
        <View
          style={{
            padding: cfg.padding.size4,
            flexDirection: 'row',
            alignItems: 'center',
          }}>
          <NavTitle title="Action" textColor={textColor} />
          <View
            style={{flex: 1, flexDirection: 'row', paddingHorizontal: cfg.padding.size8}}>
            <NavElement>
              <NavButton textColor={textColor} title="Home" />
            </NavElement>
            <NavElement>
              <NavButton textColor={textColor} title="Documentation" />
            </NavElement>
            <NavElement>
              <NavButton textColor={textColor} title="Publications" />
            </NavElement>
            <NavElement>
              <NavButton textColor={textColor} title="About" />
            </NavElement>
            <NavElement>
              <OutlineButton strokeColor={textColor} label="Try Action" />
            </NavElement>
          </View>
          <View
            style={{
              flexDirection: 'row',
            }}>
            <TouchableOpacity style={{paddingHorizontal: cfg.padding.size2}}>
              <MdExitToApp color={textColor} size={cfg.fontSize.large} />
            </TouchableOpacity>
          </View>
        </View>
        {props.breadcrumb &&
          props.breadcrumb.length > 0 && (
            <View
              style={{
                borderTopWidth: cfg.borderWidth.default,
                borderTopColor: borderColor,
              }}>
              <Breadcrumb items={props.breadcrumb} textColor={textColor} />
            </View>
          )}
      </View>
    </View>
  );
}

Nav.defaultProps = {outlineColor: cfg.color.black};

function NavOnNonTransparentBackground() {
  return (
    <View style={{height: '100vh', backgroundColor: cfg.color.pinkLightest}}>
      <Nav
        outlineColor={cfg.color.indigoDark}
        breadcrumb={[
          {title: 'Documentation'},
          {title: 'API References'},
          {title: 'Combinators'},
        ]}
      />
    </View>
  );
}
NavOnNonTransparentBackground.displayName = 'Nav';

const fixtures: FixtureList = [
  {
    name: 'Default',
    component: Nav,
    props: {
      breadcrumb: [],
    },
  },
  {
    name: 'With Breadcrumb',
    component: Nav,
    props: {
      breadcrumb: [
        {title: 'Documentation'},
        {title: 'API References'},
        {title: 'Combinators'},
      ],
    },
  },
  {
    name: 'Custom Outline Color',
    component: Nav,
    props: {
      outlineColor: cfg.color.indigo,
      breadcrumb: [
        {title: 'Documentation'},
        {title: 'API References'},
        {title: 'Combinators'},
      ],
    },
  },
  {
    name: 'Custom Outline Color (on non-transparent background)',
    component: NavOnNonTransparentBackground,
    props: {},
  },
];

export default fixtures;
