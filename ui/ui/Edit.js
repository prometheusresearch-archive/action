/**
 * @flow
 */

import * as React from 'react';
import {View} from 'react-native-web';
import * as W from 'core';
import type {State} from 'core';
import {ScreenTitle} from './ScreenTitle.js';
import * as cfg from 'components/config';
import {DataCard} from 'components/DataCard';
import {FormField} from 'components/FormField';
import {TextInput} from 'components/TextInput';
import {OutlineButton} from 'components/OutlineButton';

type P = {
  state: State,
};

type S = {
  // $FlowFixMe: ...
  value: Object,
};

export class Edit extends React.Component<P, S> {
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
      }
    `,
    );
    // $FlowFixMe: ...
    const {title, data, type} = result;
    return {title, data, type};
  }

  onChange = (key: string, update: any) => {
    const value = {...this.state.value, [key]: update};
    this.setState({value});
  };

  render() {
    let props = this.props;
    let {title, data, type} = this.fetch();
    const entityName = getEntityName(type, data);
    const fields = [];
    for (let key in this.state.value) {
      fields.push(
        <View key={key} style={{paddingBottom: cfg.padding.size2}}>
          <FormField
            label={capitalize(key)}
            renderInput={props => (
              <TextInput
                {...props}
                value={this.state.value[key]}
                onChangeText={this.onChange.bind(null, key)}
              />
            )}
          />
        </View>,
      );
    }
    return (
      <View>
        <ScreenTitle>{title}</ScreenTitle>
        {fields}
        <View style={{flexDirection: 'row', paddingVertical: cfg.padding.size2}}>
          <View style={{paddingRight: cfg.padding.size2}}>
            <OutlineButton
              label="Save"
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
