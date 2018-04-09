/**
 * @flow
 */

import * as React from 'react';
import {View, Text, StyleSheet, processColor} from 'react-native-web';
import * as cfg from './config.js';

type P = {
  title?: React.Node,
  tagLine?: React.Node,
  outlineColor: string,
  data: mixed,
  renderField?: RenderField,
};

type RenderField = ({
  data: mixed,
  path: string,
  renderField: () => React.Node,
}) => React.Node;

let style = StyleSheet.create({
  header: {
    paddingBottom: cfg.padding.size4,
  },
  title: {
    fontSize: cfg.fontSize.xxLarge,
    fontWeight: cfg.fontWeight.bold,
  },
  tagLine: {
    fontSize: cfg.fontSize.small,
    fontWeight: cfg.fontWeight.semibold,
    color: cfg.color.greyDark,
  },
  fields: {
    paddingVertical: cfg.padding.size2,
  },
  fieldsObjectKeys: {
    textAlign: 'right',
    flex: 1,
  },
  fieldsObjectKey: {
    padding: cfg.padding.size2,
    overflow: 'hidden',
  },
  fieldsObjectKeyText: {
    fontFamily: cfg.fontFamily.sans,
    fontWeight: cfg.fontWeight.medium,
    textOverflow: 'ellipsis',
    whiteSpace: 'nowrap',
    overflow: 'hidden',
    userSelect: 'none',
  },
  fieldsObjectValues: {
    flex: 3,
  },
  fieldsObjectValue: {
    padding: cfg.padding.size2,
  },
});

export function DataCard(props: P) {
  let fields = renderField(props, props.data, [], props.renderField, true);
  return (
    <View>
      {props.title != null && (
        <View style={style.header}>
          {props.title != null && (
            <Text style={[style.title, {color: props.outlineColor}]}>{props.title}</Text>
          )}
          {props.tagLine != null && (
            <Text style={[style.tagLine, {color: processColor(props.outlineColor, 0.6)}]}>
              {props.tagLine}
            </Text>
          )}
        </View>
      )}
      <View style={style.fields}>{fields}</View>
    </View>
  );
}

DataCard.defaultProps = {
  outlineColor: cfg.color.black,
};

function renderField(props, data, path, override, overridable) {
  if (overridable && override != null) {
    return override({
      data,
      path: path.join('.'),
      renderField: () => renderField(props, data, path, override, false),
    });
  }
  if (data == null) {
    return (
      <View>
        <Text>-</Text>
      </View>
    );
  } else if (data === true) {
    return (
      <View>
        <Text>true</Text>
      </View>
    );
  } else if (data === false) {
    return (
      <View>
        <Text>false</Text>
      </View>
    );
  } else if (typeof data === 'string') {
    return (
      <View>
        <Text>{data}</Text>
      </View>
    );
  } else if (typeof data === 'number') {
    return (
      <View>
        <Text>{String(data)}</Text>
      </View>
    );
  } else if (Array.isArray(data)) {
    let items = data.map((item, id) => (
      <View key={id}>{renderField(props, data, path.concat(['*']), override, true)}</View>
    ));
    return <View>{items}</View>;
  } else if (typeof data === 'object') {
    const keys = [];
    const values = [];
    for (let key in data) {
      keys.push(
        <View style={style.fieldsObjectKey}>
          <Text title={key} style={[style.fieldsObjectKeyText]}>
            {key}
          </Text>
        </View>,
      );
      values.push(
        <View style={style.fieldsObjectValue}>
          {renderField(props, data[key], path.concat([key]), override, true)}
        </View>,
      );
    }
    return (
      <View style={{flexDirection: 'row'}}>
        <View style={style.fieldsObjectKeys}>{keys}</View>
        <View style={style.fieldsObjectValues}>{values}</View>
      </View>
    );
  } else {
    return (
      <View>
        <Text>Unable to render fields</Text>
      </View>
    );
  }
}
