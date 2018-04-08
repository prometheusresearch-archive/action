/**
 * @flow
 */

import * as React from 'react';
import {View, Text} from 'react-native-web';
import {type FixtureList, createShowcaseMatrix} from './FixtureUtil.js';
import {OutlineButton} from './OutlineButton.js';
import * as cfg from './config.js';

const displayName = OutlineButton.displayName || OutlineButton.name;
const ShowcaseMatrix = createShowcaseMatrix(displayName);

const sizes = ['large', 'medium', 'small'];

const fixtures: FixtureList = [
  {
    component: ShowcaseMatrix,
    props: {
      rows: [
        {
          title: 'Default',
          columns: sizes.map(size => <OutlineButton size={size} label="Press Me" />),
        },
        {
          title: 'Custom stroke color',
          columns: sizes.map(size => (
            <OutlineButton size={size} label="Press Me" strokeColor={cfg.color.blue} />
          )),
        },
        {
          title: 'Custom fill color',
          columns: sizes.map(size => (
            <OutlineButton size={size} label="Press Me" fillColor={cfg.color.blue} />
          )),
        },
        {
          title: 'Custom stroke & fill color',
          columns: sizes.map(size => (
            <OutlineButton
              size={size}
              label="Press Me"
              strokeColor={cfg.color.blueDarker}
              fillColor={cfg.color.red}
            />
          )),
        },
      ],
    },
  },
];

export default fixtures;
