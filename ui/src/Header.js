/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import {Workflow} from './Workflow';
import * as W from 'workflow';

export type ScreenConfig<ScreenId> = {
  [id: ScreenId]: {
    title: string,
    Component: React.ComponentType<{
      renderHeader: ({
        breadcrumb: Array<BreadcrumbItem>,
        toolbar?: ?React.Node,
      }) => React.Node,
    }>,
  },
};

export type BreadcrumbItem = {
  title: string,
  onPress: () => void,
};

type P<ScreenId> = {
  screens: ScreenConfig<ScreenId>,
  activeScreen: ScreenId,
  onScreen: ScreenId => void,
  breadcrumb: Array<BreadcrumbItem>,
  toolbar?: ?React.Node,
};

export function Header<ScreenId>(props: P<ScreenId>) {
  return (
    <View style={{padding: 10}}>
      <View
        style={{
          boxShadow: '0px 1px 0px 0px #BBB',
          borderRadius: 2,
          flexDirection: 'column',
          border: '1px solid #BBB',
        }}>
        <View style={{flexDirection: 'row', alignItems: 'center'}}>
          <View style={{paddingLeft: 20, paddingRight: 20, paddingVertical: 10}}>
            <Text style={{fontWeight: '900', fontSize: '18pt'}}>action</Text>
          </View>
          <View style={{flexDirection: 'row'}}>
            {Object.keys(props.screens).map(id => {
              const screen = props.screens[id];
              return (
                <NavButton
                  key={id}
                  isActive={props.activeScreen === id}
                  onPress={props.onScreen.bind(null, id)}>
                  {screen.title}
                </NavButton>
              );
            })}
          </View>
        </View>
        {props.breadcrumb.length > 0 && (
          <View style={{borderTop: '1px solid #bbb'}}>
            <Breadcrumb breadcrumb={props.breadcrumb} />
          </View>
        )}
        {props.toolbar != null && (
          <View style={{borderTop: '1px solid #bbb'}}>{props.toolbar}</View>
        )}
      </View>
    </View>
  );
}

export function Breadcrumb(props: {breadcrumb: Array<BreadcrumbItem>}) {
  return (
    <View
      style={{
        flexDirection: 'row',
        alignItems: 'center',
      }}>
      {props.breadcrumb.map((item, idx) => {
        return <BreadcrumbButton onPress={item.onPress} title={item.title} />;
      })}
    </View>
  );
}

function BreadcrumbButton(props) {
  return (
    <TouchableOpacity onPress={props.onPress}>
      <View style={{paddingHorizontal: 10, paddingVertical: 7, flexDirection: 'row'}}>
        <Text style={{fontSize: '9pt', fontWeight: props.isActive ? '600' : '200'}}>
          {props.title}
        </Text>
        <Text style={{fontSize: '9pt', fontWeight: '200', paddingLeft: 15}}>›</Text>
      </View>
    </TouchableOpacity>
  );
}

function NavButton({children, isActive, onPress}) {
  return (
    <TouchableOpacity onPress={onPress}>
      <View
        style={{
          paddingHorizontal: 10,
          paddingVertical: 5,
        }}>
        <Text
          style={{
            textAlign: 'center',
            fontWeight: isActive ? '600' : '300',
          }}>
          {children}
        </Text>
      </View>
    </TouchableOpacity>
  );
}
