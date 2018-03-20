/**
 * @flow
 */

import * as React from 'react';
import {View, Text} from 'react-native-web';

type P = {
  error: string,
};

export function Error(props: P) {
  return (
    <View>
      <Text>Error: {props.error}</Text>
    </View>
  );
}
