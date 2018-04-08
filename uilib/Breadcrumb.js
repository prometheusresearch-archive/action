/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import type {FixtureList} from './fixture.js';
import {OutlineButton} from './OutlineButton.js';
import * as cfg from './config.js';
import {MdHome, MdKeyboardArrowRight} from 'react-icons/lib/md';

export type BreadcrumbItem = {title: string};

export function BreadcrumbButton({title, textColor}: {title: string, textColor: string}) {
  return (
    <View style={{paddingHorizontal: cfg.padding.size2}}>
      <TouchableOpacity>
        <Text
          style={{
            color: textColor,
            fontWeight: cfg.fontWeight.semibold,
            fontSize: cfg.fontSize.xSmall,
          }}>
          {title}
        </Text>
      </TouchableOpacity>
    </View>
  );
}

type P = {
  items: Array<BreadcrumbItem>,
  textColor: string,
};

export function Breadcrumb({items, textColor}: P) {
  let itemsRendered = items.map(item => (
    <View
      style={{
        flexDirection: 'row',
        alignItems: 'flex-end',
      }}>
      <BreadcrumbButton title={item.title} textColor={textColor} />
      <MdKeyboardArrowRight size={cfg.fontSize.small} color={textColor} />
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
        <MdKeyboardArrowRight color={textColor} size={cfg.fontSize.small} />
      </View>
      {itemsRendered}
    </View>
  );
}

Breadcrumb.defaultProps = {
  textColor: cfg.color.black,
};
