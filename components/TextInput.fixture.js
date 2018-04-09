/**
 * @flow
 */

import * as React from 'react';
import {View} from 'react-native-web';
import * as cfg from './config.js';
import {createShowcaseList} from './FixtureUtil.js';
import {TextInput} from './TextInput.js';

const ShowcaseList = createShowcaseList(TextInput);

const simple = {
  title: 'Default',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <TextInput value="" />
    </View>
  ),
};

const withValue = {
  title: 'With a Value',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <TextInput value="Hello, world!" />
    </View>
  ),
};

const withPlaceholder = {
  title: 'With a Placeholder',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <TextInput placeholder="Enter your story here..." />
    </View>
  ),
};

const multiline = {
  title: 'Multiline',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <TextInput multiline placeholder="Enter your story here..." />
    </View>
  ),
};

const multilineConfigNumLines = {
  title: 'Multiline (with configurable numbers of lines)',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <TextInput multiline numberOfLines={5} placeholder="Enter your story here..." />
    </View>
  ),
};

const errorState = {
  title: 'Error State',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <TextInput error={true} value="" />
    </View>
  ),
};

const withCustomColor = {
  title: 'With a Custom Color',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <TextInput value="" color={cfg.color.indigoDark} />
    </View>
  ),
};

const withCustomColorOnBg = {
  title: 'With a Custom Color',
  element: (
    <View style={{backgroundColor: cfg.color.pinkLightest, padding: cfg.padding.size4}}>
      <TextInput value="" color={cfg.color.indigoDark} />
    </View>
  ),
};

export default {
  component: ShowcaseList,
  props: {
    rows: [
      simple,
      withValue,
      withPlaceholder,
      multiline,
      multilineConfigNumLines,
      errorState,
      withCustomColor,
      withCustomColorOnBg,
    ],
  },
};
