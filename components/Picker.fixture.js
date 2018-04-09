/**
 * @flow
 */

import * as React from 'react';
import {View} from 'react-native-web';
import * as cfg from './config.js';
import {createShowcaseList} from './FixtureUtil.js';
import {Picker} from './Picker.js';

const ShowcaseList = createShowcaseList(Picker);

const simple = {
  title: 'Default',
  element: (
    <Picker
      options={[
        {label: '', value: 'unknown'},
        {label: 'Female', value: 'female'},
        {label: 'Male', value: 'male'},
      ]}
    />
  ),
};

const withSelectedValue = {
  title: 'With Selected Value',
  element: (
    <Picker
      selectedValue="male"
      options={[
        {label: '', value: 'unknown'},
        {label: 'Female', value: 'female'},
        {label: 'Male', value: 'male'},
      ]}
    />
  ),
};

export default {
  component: ShowcaseList,
  props: {
    rows: [simple, withSelectedValue],
  },
};
