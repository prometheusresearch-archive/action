/**
 * @flow
 */

import * as React from 'react';
import {Text, View} from 'react-native-web';
import * as Workflow from 'workflow';
import * as types from './types.js';

type Config = {
  id: string,
  entityName: string,
  fields: Array<string>,
  renderTitle?: (types.Context, ?types.DataSet) => React.Element<*>,
};

export function configure(config: Config): types.Workflow {
  const ui = {
    id: config.id,
    renderTitle(context, data) {
      if (config.renderTitle) {
        return config.renderTitle(context, data);
      } else if (data != null) {
        return (
          <Text>
            View {data[config.entityName]._meta.title} {config.entityName}
          </Text>
        );
      } else {
        return <Text>View {config.entityName}</Text>;
      }
    },
    render(context, data, onContext) {
      return (
        <View>
          <pre>{JSON.stringify(data[config.entityName], null, 2)}</pre>
        </View>
      );
    },
  };
  const query = context => {
    const fields = config.fields.join(', ');
    return `
      ${config.entityName}(id: ${context[config.entityName].value.id}) {
        ${fields}
      }
    `;
  };
  const queryTitle = context => {
    return `
      ${config.entityName}(id: ${context[config.entityName].value.id}) {
        _meta{title}
      }
    `;
  };
  return Workflow.interaction({
    requires: {
      [config.entityName]: Workflow.entityType(config.entityName),
    },
    provides: {},
    query,
    queryTitle,
    ui,
  });
}