/**
 * @flow
 */

import * as React from 'react';
import {View} from 'react-native-web';
import * as cfg from './config.js';
import {createShowcaseList} from './FixtureUtil.js';
import {FormField} from './FormField.js';
import {TextInput} from './TextInput.js';

const ShowcaseList = createShowcaseList(FormField);

const onlyInput = {
  title: 'Only Input',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <FormField renderInput={props => <TextInput {...props} />} />
    </View>
  ),
};

const withLabel = {
  title: 'With Label',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <FormField label="First Name" renderInput={props => <TextInput {...props} />} />
    </View>
  ),
};

const withHint = {
  title: 'With Hint',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <FormField
        label="First Name"
        hint="Enter your first name here"
        renderInput={props => <TextInput {...props} />}
      />
    </View>
  ),
};

const onBg = {
  title: 'On Background',
  element: (
    <View style={{padding: cfg.padding.size4, backgroundColor: cfg.color.white}}>
      <FormField
        label="First Name"
        hint="Enter your first name here"
        renderInput={props => <TextInput {...props} />}
      />
    </View>
  ),
};

const withCustomColor = {
  title: 'With Custom Color',
  element: (
    <View style={{padding: cfg.padding.size4}}>
      <FormField
        outlineColor={cfg.color.indigoDark}
        label="First Name"
        hint="Enter your first name here"
        renderInput={props => <TextInput {...props} />}
      />
    </View>
  ),
};

const withCustomColorOnBg = {
  title: 'With Custom Color (On Background)',
  element: (
    <View style={{padding: cfg.padding.size4, backgroundColor: cfg.color.pinkLightest}}>
      <FormField
        outlineColor={cfg.color.indigoDark}
        label="First Name"
        hint="Enter your first name here"
        renderInput={props => <TextInput {...props} />}
      />
    </View>
  ),
};

export default {
  component: ShowcaseList,
  props: {
    rows: [onlyInput, withLabel, withHint, onBg, withCustomColor, withCustomColorOnBg],
  },
};
