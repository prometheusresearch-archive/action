/**
 * @flow
 */

import * as React from 'react';
import {View, Text} from 'react-native-web';

export function Loading() {
  return (
    <View style={{flex: 1, padding: 10}}>
      <Text>Loading...</Text>
    </View>
  );
}
