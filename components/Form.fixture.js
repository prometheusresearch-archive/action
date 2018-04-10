/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import * as cfg from './config.js';
import {createShowcaseList} from './FixtureUtil.js';
import {FormField} from './FormField.js';
import {TextInput} from './TextInput.js';
import {OutlineButton} from './OutlineButton.js';

const ShowcaseList = createShowcaseList(function Form() {});

function Wrapper({children, style}) {
  return <View style={{...style, paddingBottom: cfg.padding.size2}}>{children}</View>;
}
Wrapper.defaultProps = {
  style: null,
};

function SubmitButton({label, style}) {
  return <OutlineButton label={label} outlineColor={cfg.color.greenDark} style={style} />;
}

const loginForm = {
  title: 'Login Form',
  element: (
    <View
      style={{padding: cfg.padding.size4, width: 450, backgroundColor: cfg.color.white}}>
      <Wrapper>
        <FormField
          label="Email"
          renderInput={props => <TextInput {...props} placeholder="name@domain.com" />}
        />
      </Wrapper>
      <Wrapper>
        <FormField
          label="Password"
          renderInput={props => (
            <TextInput {...props} secureTextEntry={true} placeholder="password" />
          )}
        />
      </Wrapper>
      <Wrapper style={{flexDirection: 'row', paddingTop: cfg.padding.size2}}>
        <SubmitButton
          label="Login"
          style={{
            flex: 1,
            borderRightWidth: 0,
            borderTopRightRadius: 0,
            borderBottomRightRadius: 0,
          }}
        />
        <OutlineButton
          label="Login with RexID"
          style={{flex: 1, borderTopLeftRadius: 0, borderBottomLeftRadius: 0}}
        />
      </Wrapper>
    </View>
  ),
};

export default [
  {
    name: 'Showcase',
    component: ShowcaseList,
    props: {
      rows: [loginForm],
    },
  },
];
