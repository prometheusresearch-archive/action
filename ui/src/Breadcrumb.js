/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import type {Interaction} from './types.js';

export function Breadcrumb({
  current,
  trace,
  onClick,
}: {
  current: Interaction,
  trace: Array<Interaction>,
  onClick: Interaction => *,
}) {
  const items = [];
  for (const item of trace) {
    const onPress = item.frame != null ? () => onClick(item) : null;
    items.push({title: item.ui.renderTitle(item.context, item.dataTitle), onPress});
  }

  const buttons = items.map((item, idx) => {
    const style = {paddingRight: 10, fontWeight: '200'};
    return (
      <TouchableOpacity key={idx} onPress={item.onPress}>
        <View style={{flexDirection: 'row'}}>
          <Text style={style}>{item.title}</Text>
          <Text style={{fontWeight: '200', paddingLeft: 0, paddingRight: 10}}>→</Text>
        </View>
      </TouchableOpacity>
    );
  });

  buttons.push(
    <View>
      <Text key="current" style={{paddingRight: 10, fontWeight: '600'}}>
        {current.ui.renderTitle(current.context, current.dataTitle)}
      </Text>
    </View>,
  );

  return (
    <View style={{flexDirection: 'row', padding: 10}}>
      <View style={{flexDirection: 'row'}}>{buttons}</View>
    </View>
  );
}
