/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import {type BreadcrumbItem} from './Header.js';
import Textarea from 'react-textarea-autosize';
import * as W from 'workflow';
import {Workflow} from './Workflow.js';

type P = {
  renderHeader: ({
    breadcrumb: Array<BreadcrumbItem>,
    toolbar?: ?React.Node,
  }) => React.Node,
};

type S = {
  value: string,
  result: ?W.ParseResult,
  view: ?React.Node,
  error: ?string,
};

const initialState = {
  value: '',
  result: null,
  view: null,
  error: null,
};

export class Console extends React.Component<P, S> {
  state = initialState;

  onValue = (value: string) => {
    this.setState(state => {
      if (value == '') {
        return initialState;
      }
      const result = W.parse(value);
      let view = state.view;
      let error = null;
      console.log(result);
      if (result.error != null) {
        error = result.error;
      } else if (result.ui != null) {
        view = <Workflow key={value} startState={result.ui} />;
      } else {
        view = <Data data={result.data} />;
      }
      return {...state, error, view, value, result};
    });
  };
  render() {
    return (
      <View>
        {this.props.renderHeader({breadcrumb: []})}
        <View style={{padding: 10}}>
          <View style={{paddingVertical: 5}}>
            <Hint message="Enter action expression and see the evaluated result" />
          </View>
          <Input
            isError={this.state.result != null && this.state.result.error != null}
            value={this.state.value}
            onValue={this.onValue}
          />
        </View>
        {this.state.error != null && (
          <View style={{padding: 10}}>
            <Error message={this.state.error} />
          </View>
        )}
        <View style={{padding: 10}}>{this.state.view}</View>
      </View>
    );
  }
}

function Input(props) {
  return (
    <View
      style={{
        borderStyle: 'solid',
        borderWidth: 1,
        borderColor: props.isError ? 'red' : '#bbb',
      }}>
      <Textarea
        minRows={3}
        style={{
          border: 'none',
          padding: 15,
          fontFamily: 'Menlo, Monaco, monospace',
          fontSize: '10pt',
          color: '#444',
          outline: 'none',
        }}
        value={props.value}
        onChange={e => props.onValue(e.target.value)}
      />
    </View>
  );
}

function Error(props) {
  return (
    <View>
      <Text style={{fontSize: '10pt', color: 'red'}}>{props.message}</Text>
    </View>
  );
}

function Hint(props) {
  return (
    <View>
      <Text style={{fontSize: '10pt', color: '#888'}}>{props.message}</Text>
    </View>
  );
}

function Data(props) {
  return (
    <View>
      <Text
        style={{fontFamily: 'Menlo, Monaco, monospace', fontSize: '10pt', color: '#444'}}>
        {JSON.stringify(props.data, null, 2)}
      </Text>
    </View>
  );
}