/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import type {Interaction} from './types.js';

export function AlternativesToolbar({
  current,
  alternatives,
  onClick,
}: {
  current: Interaction,
  alternatives: Array<Interaction>,
  onClick: Interaction => *,
}) {
  const items = [];
  for (const item of alternatives) {
    const onPress = item.frame != null ? () => onClick(item) : null;
    const title = item.ui.renderTitle(item.context, item.dataTitle);
    const active = current.ui.id === item.ui.id;
    items.push({title, onPress, active});
  }

  const buttons = items.map((item, idx) => {
    const style = {paddingRight: 10, fontWeight: item.active ? '600' : '200'};
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
