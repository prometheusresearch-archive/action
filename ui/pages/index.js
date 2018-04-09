/**
 * @flow
 */

import type {PageProps} from 'next';
import React from 'react';
import {App} from '../ui/App';
import {StyleSheet, Text, View} from 'react-native-web';

export default (props: PageProps) => (
  <App {...props}>
    <Text>ok</Text>
  </App>
);
