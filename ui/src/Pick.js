/**
 * @flow
 */

import * as React from 'react';
import type {Action} from 'api/Workflow';
import {Text, ScrollView, View, TouchableOpacity} from 'react-native-web';

type Config = {
  id: string,
  title: string,
  entityName: string,
  fields: Array<string>,
};

export function configure(config: Config) {
  const action: Action = {
    type: 'View',
    id: config.id,
    title: config.title,

    requires: {},
    provides: {
      row: {type: 'EntityType', name: config.entityName, fields: {}},
    },

    query(context) {
      const fields = config.fields.join(', ');
      return `${config.entityName}__list {${fields}}`;
    },

    render(context, data, onContext) {
      return (
        <Component config={config} context={context} data={data} onContext={onContext} />
      );
    },
  };
  return action;
}

type Props = {
  data: any,
  context: any,
  onContext: Function,
  config: Config,
};

function Component(props: Props) {
  const items = props.data[`${props.config.entityName}__list`].map(row => {
    const onPress = id => () => {
      props.onContext({[props.config.entityName]: {id, __type: props.config.entityName}});
    };
    const isSelected =
      props.context[props.config.entityName] != null &&
      props.context[props.config.entityName].id === row.id;
    const style = isSelected ? {fontWeight: '600'} : {fontWeight: '200'};
    return (
      <TouchableOpacity key={row.code} onPress={onPress(row.id)}>
        <View style={{padding: 5}}>
          <Text style={style}>
            {row.code},
            {row.sex}
          </Text>
        </View>
      </TouchableOpacity>
    );
  });
  return <ScrollView>{items}</ScrollView>;
}
