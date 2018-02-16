/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import type {Interaction} from './types.js';

export function NextToolbar({
  next,
  onClick,
}: {
  next: Array<Interaction>,
  onClick: Interaction => *,
}) {
  const items = [];
  for (const item of next) {
    const onPress = item.frame != null ? () => onClick(item) : null;
    const title = item.ui.renderTitle(item.context, item.dataTitle);
    items.push({title, onPress});
  }

  const buttons = items.map((item, idx) => {
    const style = {paddingRight: 10, fontWeight: '200'};
    return (
      <TouchableOpacity key={idx} onPress={item.onPress}>
        <Text style={style}>{item.title}</Text>
      </TouchableOpacity>
    );
  });

  return (
    <View style={{flexDirection: 'row', padding: 10}}>
      <View style={{flexDirection: 'row'}}>{buttons}</View>
    </View>
  );
}
