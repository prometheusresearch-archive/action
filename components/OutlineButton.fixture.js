/**
 * @flow
 */

import * as React from 'react';
import {View, Text} from 'react-native-web';
import {type FixtureList, createShowcaseMatrix} from './FixtureUtil.js';
import {OutlineButton} from './OutlineButton.js';
import * as cfg from './config.js';

const ShowcaseMatrix = createShowcaseMatrix(OutlineButton);

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
            <OutlineButton size={size} label="Press Me" outlineColor={cfg.color.indigo} />
          )),
        },
        {
          title: 'Custom fill color',
          columns: sizes.map(size => (
            <OutlineButton
              size={size}
              label="Press Me"
              fillColor={cfg.color.pinkLightest}
            />
          )),
        },
        {
          title: 'Custom stroke & fill color',
          columns: sizes.map(size => (
            <OutlineButton
              size={size}
              label="Press Me"
              outlineColor={cfg.color.indigo}
              fillColor={cfg.color.pinkLightest}
            />
          )),
        },
      ],
    },
  },
];

export default fixtures;
