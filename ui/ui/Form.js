/**
 * @flow
 */

import * as React from 'react';
import {View} from 'react-native-web';
import type {State} from 'core';
import * as W from 'core';
import {ScreenTitle} from './ScreenTitle.js';
import * as cfg from 'components/config';
import {DataCard} from 'components/DataCard';
import {FormField} from 'components/FormField';
import {TextInput} from 'components/TextInput';
import {OutlineButton} from 'components/OutlineButton';

type P = {
  state: State,
  onState: State => void,
};

type S = {
  // $FlowFixMe: ...
  value: Object,
};

export class Form extends React.Component<P, S> {
  constructor(props: P) {
    super(props);
    let {data} = this.fetch();
    this.state = {value: data};
  }

  fetch() {
    const result = W.query(
      this.props.state,
      `
      {
        title: title,
        data: data,
        type: data:meta.type,
        mutation: mutation,
      }
    `,
    );
    // $FlowFixMe: ...
    const {title, data, type, mutation} = result;
    return {title, data, type, mutation};
  }

  onChange = (key: string, update: string | number) => {
    const value = {...this.state.value, [key]: update};
    this.setState({value});
  };

  onSubmit = mutation => {
    const value = this.state.value;
    const state = W.mutate(mutation, value, this.props.state);
    this.props.onState(state);
  };

  render() {
    let props = this.props;
    let {title, data, type, mutation} = this.fetch();
    const entityName = getEntityName(type, data);
    return (
      <View>
        <ScreenTitle>{title}</ScreenTitle>
        <UpdateForm value={this.state.value} onChange={this.onChange} />
        <View style={{flexDirection: 'row', paddingVertical: cfg.padding.size2}}>
          <View style={{paddingRight: cfg.padding.size2}}>
            <OutlineButton
              label="Save"
              onPress={this.onSubmit.bind(null, mutation)}
              outlineColor={cfg.color.greenDarker}
              fillColor={cfg.color.greenLightest}
            />
          </View>
          <View style={{paddingRight: cfg.padding.size1}}>
            <OutlineButton label="Reset" />
          </View>
        </View>
      </View>
    );
  }
}

function UpdateForm(props: {
  value: {[name: string]: string},
  onChange: (string, string) => void,
}) {
  const fields = [];
  for (let key in props.value) {
    fields.push(
      <View key={key} style={{paddingBottom: cfg.padding.size2}}>
        <FormField
          label={capitalize(key)}
          renderInput={inputProps => (
            <TextInput
              {...inputProps}
              value={props.value[key]}
              onChangeText={props.onChange.bind(null, key)}
            />
          )}
        />
      </View>,
    );
  }
  return <View>{fields}</View>;
}

function CreateForm(props: {
  value: {[name: string]: string},
  onChange: (string, string) => void,
}) {
  const fields = [];
  for (let key in props.value) {
    fields.push(
      <View key={key} style={{paddingBottom: cfg.padding.size2}}>
        <FormField
          label={capitalize(key)}
          renderInput={inputProps => (
            <TextInput
              {...inputProps}
              value={props.value[key]}
              onChangeText={props.onChange.bind(null, key)}
            />
          )}
        />
      </View>,
    );
  }
  return <View>{fields}</View>;
}

function getEntityName({type}, data) {
  if (type.type === 'entity') {
    const entityName = capitalize(type.name);
    return `${entityName} ${data.id}`;
  } else if (data.id != null) {
    return `${data.id}`;
  } else {
    return null;
  }
}

function capitalize(value) {
  return value[0].toUpperCase() + value.slice(1);
}
