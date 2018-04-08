/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import {OutlineButton} from './OutlineButton.js';
import {type BreadcrumbItem, Breadcrumb} from './Breadcrumb.js';
import * as cfg from './config.js';

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

type NavButtonProps = {
  title: string,
  active: boolean,
  outlineColor?: string,
  onPress: () => void,
};

export function NavButton({title, active, outlineColor, onPress}: NavButtonProps) {
  return (
    <View
      style={{
        flexDirection: 'row',
        alignItems: 'center',
      }}>
      <TouchableOpacity onPress={onPress}>
        <Text
          style={{
            color: outlineColor,
            fontSize: active ? cfg.fontSize.base : cfg.fontSize.small,
            fontWeight: active ? cfg.fontWeight.black : cfg.fontWeight.semibold,
          }}>
          {title}
        </Text>
      </TouchableOpacity>
    </View>
  );
}

type NavItem = {
  id: string,
  render: ({
    outlineColor: string,
    id: string,
    active: boolean,
    onPress: () => void,
  }) => React.Node,
};

type P = {
  outlineColor: string,
  breadcrumb: Array<BreadcrumbItem>,
  items?: Array<NavItem>,
  itemsExtra?: Array<NavItem>,
  active?: string,
  onActive: ({id: string}) => void,
};

export function Nav({outlineColor, items, itemsExtra, breadcrumb, active, onActive}: P) {
  const borderColor = outlineColor;
  const textColor = outlineColor;
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
            {items != null &&
              items.map(item => (
                <NavElement key={item.id}>
                  {item.render({
                    id: item.id,
                    active: active != null && item.id === active,
                    outlineColor,
                    onPress: onActive.bind(null, {id: item.id}),
                  })}
                </NavElement>
              ))}
          </View>
          <View style={{flexDirection: 'row'}}>
            {itemsExtra != null &&
              itemsExtra.map(item => (
                <NavElement key={item.id}>
                  {item.render({
                    id: item.id,
                    active: active != null && item.id === active,
                    outlineColor,
                    onPress: onActive.bind(null, {id: item.id}),
                  })}
                </NavElement>
              ))}
          </View>
        </View>
        {breadcrumb &&
          breadcrumb.length > 0 && (
            <View
              style={{
                borderTopWidth: cfg.borderWidth.default,
                borderTopColor: borderColor,
              }}>
              <Breadcrumb items={breadcrumb} textColor={textColor} />
            </View>
          )}
      </View>
    </View>
  );
}

Nav.defaultProps = {
  outlineColor: cfg.color.black,
  onActive: _ev => {},
};
