/**
 * @flow
 */

import invariant from 'invariant';
import React, {Component} from 'react';
import {View, Text} from 'react-native-web';
import * as W from 'workflow';
import type {Result, State, UI, Query} from 'workflow';
import {Loading} from './Loading.js';

type P = {
  query: Query,
};

export function Pick(props: P) {
  const data = W.runQuery(props.query);
  return (
    <View>
      <Text>Pick</Text>
      <Text style={{fontFamily: 'monospace'}}>{JSON.stringify(data)}</Text>
    </View>
  );
}
