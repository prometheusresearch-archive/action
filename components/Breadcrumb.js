/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import {OutlineButton} from './OutlineButton.js';
import * as cfg from './config.js';
import {MdHome, MdKeyboardArrowRight} from 'react-icons/lib/md';

export type BreadcrumbItem = {id: string, title: string};

type BreadcrumbButtonProps = {
  title: string,
  textColor: string,
  onPress: () => void,
};

export function BreadcrumbButton({title, textColor, onPress}: BreadcrumbButtonProps) {
  return (
    <View>
      <TouchableOpacity onPress={onPress} style={{paddingHorizontal: cfg.padding.size2}}>
        <Text
          style={{
            color: textColor,
            fontWeight: cfg.fontWeight.normal,
            fontSize: cfg.fontSize.small,
          }}>
          {title}
        </Text>
      </TouchableOpacity>
    </View>
  );
}

type P<Item: BreadcrumbItem> = {
  items: Array<Item>,
  textColor: string,
  onSelect: Item => void,
};

export function Breadcrumb<Item: BreadcrumbItem>({onSelect, items, textColor}: P<Item>) {
  let itemsRendered = items.map(item => (
    <View
      key={item.id}
      style={{
        flexDirection: 'row',
        alignItems: 'flex-end',
      }}>
      <BreadcrumbButton
        onPress={onSelect.bind(null, item)}
        title={item.title}
        textColor={textColor}
      />
      <MdKeyboardArrowRight size={cfg.fontSize.base} color={textColor} />
    </View>
  ));
  return (
    <View
      style={{
        flexDirection: 'row',
        padding: cfg.padding.size2,
      }}>
      <View
        style={{
          flexDirection: 'row',
          alignItems: 'flex-end',
        }}>
        <View style={{paddingHorizontal: cfg.padding.size2}}>
          <MdHome color={textColor} />
        </View>
        <MdKeyboardArrowRight color={textColor} size={cfg.fontSize.base} />
      </View>
      {itemsRendered}
    </View>
  );
}

Breadcrumb.defaultProps = {
  textColor: cfg.color.black,
  onSelect: _item => {},
};
