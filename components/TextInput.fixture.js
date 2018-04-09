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
  element: <TextInput value="" />,
};

const withValue = {
  title: 'With a Value',
  element: <TextInput value="Hello, world!" />,
};

const withPlaceholder = {
  title: 'With a Placeholder',
  element: <TextInput placeholder="Enter your story here..." />,
};

const multiline = {
  title: 'Multiline',
  element: <TextInput multiline placeholder="Enter your story here..." />,
};

const multilineConfigNumLines = {
  title: 'Multiline (with configurable numbers of lines)',
  element: (
    <TextInput multiline numberOfLines={5} placeholder="Enter your story here..." />
  ),
};

const withCustomColor = {
  title: 'With a Custom Color',
  element: <TextInput value="" color={cfg.color.indigoDark} />,
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
      withCustomColor,
      withCustomColorOnBg,
    ],
  },
};
