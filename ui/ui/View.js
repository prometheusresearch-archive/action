/**
 * @flow
 */

import * as React from 'react';
import * as ReactNative from 'react-native-web';
import * as W from 'core';
import type {State} from 'core';
import {ScreenTitle} from './ScreenTitle.js';
import {DataCard} from 'components/DataCard';

type P = {
  state: State,
};

export function View(props: P) {
  const result = W.query(
    props.state,
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
  const entityName = getEntityName(type, data);
  return (
    <ReactNative.View>
      <DataCard title={title} tagLine={entityName} data={data} />
    </ReactNative.View>
  );
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
