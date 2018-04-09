/**
 * @flow
 */

import type {PageProps} from 'next';
import React from 'react';
import {App} from '../ui/App';
import {StyleSheet, Text, View} from 'react-native-web';
import {Console} from '../ui/Console.js';

export default (props: PageProps) => (
  <App {...props}>
    <Console />
  </App>
);
