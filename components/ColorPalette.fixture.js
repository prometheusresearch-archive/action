/**
 * @flow
 */

import * as cfg from './config.js';
import * as React from 'react';
import {View, Text} from 'react-native-web';
import type {FixtureList} from './FixtureUtil.js';

function FillColorCard({color, id}) {
  return (
    <View style={{width: 200}}>
      <View style={{width: '100%', height: 100, backgroundColor: color}} />
      <View style={{paddingVertical: cfg.padding.size2}}>
        <Text
          style={{
            fontWeight: cfg.fontWeight.bold,
            color,
            fontSize: cfg.fontSize.xxLarge,
          }}>
          {id}
        </Text>
        <Text style={{fontFamily: cfg.fontFamily.mono, color: cfg.color.greyDarker}}>
          {color}
        </Text>
      </View>
    </View>
  );
}

function ColorPalette() {
  const fillColors = [];
  for (let key in cfg.color) {
    fillColors.push(
      <View style={{padding: cfg.padding.size4}}>
        <FillColorCard color={cfg.color[key]} key={key} id={key} />
      </View>,
    );
  }
  return (
    <View>
      <View style={{flexDirection: 'row', flexWrap: 'wrap'}}>{fillColors}</View>
    </View>
  );
}

const fixtures: FixtureList = [
  {
    component: ColorPalette,
    props: {},
  },
];

export default fixtures;
