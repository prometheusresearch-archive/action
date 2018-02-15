/**
 * @flow
 */

import * as React from 'react';
import * as Workflow from 'workflow';
import {Text, ScrollView, View, TouchableOpacity} from 'react-native-web';

type Config = {
  id: string,
  entityName: string,
  fields: Array<string>,
  renderTitle?: (Workflow.Context, ?Workflow.DataSet) => React.Element<*>,
};

export function configure(config: Config): Workflow.Workflow<*> {
  const ui = {
    id: config.id,
    renderTitle(context, data) {
      if (config.renderTitle) {
        return config.renderTitle(context, data);
      } else {
        return <Text>Pick {config.entityName}</Text>;
      }
    },
    render(context, data, onContext) {
      return (
        <Component config={config} context={context} data={data} onContext={onContext} />
      );
    },
  };
  const query = context => {
    const fields = config.fields.join(', ');
    return `${config.entityName}__list {${fields}}`;
  };
  const queryTitle = _context => null;
  return Workflow.interaction({
    requires: {},
    provides: {
      [config.entityName]: Workflow.entityType(config.entityName),
    },
    query,
    queryTitle,
    ui,
  });
}

type Props = {
  data: Workflow.DataSet,
  context: Workflow.Context,
  onContext: Workflow.Context => *,
  config: Config,
};

function Component(props: Props) {
  const items = props.data[`${props.config.entityName}__list`].map(row => {
    const onPress = id => () => {
      const nextContext = {
        ...props.context,
        [props.config.entityName]: Workflow.entity(props.config.entityName, {id}),
      };
      props.onContext(nextContext);
    };
    const isSelected =
      props.context[props.config.entityName] != null &&
      props.context[props.config.entityName].value.id === row.id;
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
