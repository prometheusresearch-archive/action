/**
 * @flow
 */

import * as React from 'react';
import outdent from 'outdent/lib/index';
import {View, Text, TouchableOpacity} from 'react-native-web';
import Textarea from 'react-textarea-autosize';
import * as W from 'core';
import {Workflow} from './Workflow.js';
import {TextInput} from 'components/TextInput';
import {Picker} from 'components/Picker';
import {FormField} from 'components/FormField';

type P = {};

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
  constructor(props: P) {
    super(props);
    this.state = this.getStateFromQuery(outdent``);
  }

  getStateFromQuery = (value: string) => {
    if (value === '') {
      return initialState;
    }
    const result = W.parse(value);
    let view = null;
    let error = null;
    if (result.error != null) {
      error = result.error;
    } else if (result.ui != null) {
      view = <Workflow key={value} startState={result.ui} />;
    } else {
      view = <Data data={result.data} />;
    }
    return {error, view, value, result};
  };

  onClear = () => {
    this.setState(this.getStateFromQuery(''));
  };

  onValue = (value: string) => {
    this.setState(state => {
      const nextState = this.getStateFromQuery(value);
      if (nextState.view == null) {
        // $FlowFixMe: ...
        nextState.view = state.view;
      }
      return nextState;
    });
  };

  render() {
    return (
      <View>
        <Help value={this.state.value} onPress={this.onValue} />
        <View style={{padding: 10}}>
          <FormField
            label="...or enter your query:"
            error={this.state.result != null && this.state.result.error != null}
            renderInput={props => (
              <TextInput
                {...props}
                monospace={true}
                multiline={true}
                numberOfLines={12}
                value={this.state.value}
                onChangeText={this.onValue}
              />
            )}
          />
          <TouchableOpacity onPress={this.onClear}>
            <View style={{padding: 5}}>
              <Text style={{color: '#666'}}>⌫ Clear Query</Text>
            </View>
          </TouchableOpacity>
        </View>
        {this.state.error != null && (
          <View style={{padding: 10}}>
            <Error message={this.state.error} />
          </View>
        )}
        <View style={{padding: 10, paddingBottom: 300}}>{this.state.view}</View>
      </View>
    );
  }
}

function Help({value, onPress}) {
  function Item({title, query}) {
    return <option value={query}>{title}</option>;
  }
  const onChange = value => {
    onPress(value);
  };

  const options = [
    {label: '', value: ''},
    {label: 'Data: List of regions', value: 'region'},
    {label: 'Data: List of nations', value: 'region.nation'},
    {label: 'Data: First region', value: 'region:first'},
    {label: 'Screen: List of all regions', value: 'region:pick'},
    {label: 'Screen: View first region', value: 'region:first:view'},
    {
      label: 'Screen: Basic customers per region report',
      value: outdent`
          region {
            label: name,
            value: nation.customer:count
          }:barChart(title: "Customers per Region")
        `,
    },
    {
      label: 'Workflow: Simple workflow with regions',
      value: outdent`
          render(region:pick(title: "Regions")) {
            render(value:view)
          }
        `,
    },
    {
      label: 'Workflow: Nested workflow with regions',
      value: outdent`
          render(region:pick(title: "Regions")) {

            render(value:view),
            render(value.nation:pick(title: "Nations")) {

              render(value:view),
              render(value.customer:pick(title: "Customers")) {
                render(value:view),
              }

            }

          }
        `,
    },
    {
      label: 'Workflow: Custom data views',
      value: outdent`
          render(region:pick(title: "Regions")) {

            render(value:view(
              title: "Region"
            )),

            render(value {
              nationCount: nation:count,
              customerCount: nation.customer:count,
              name: name,
            }:view(
              title: "Region Statistics"
            )),

            render(value.nation {
              label: name,
              value: customer:count,
            }:barChart(
              title: "Customers per Nation"
            ))

          }
        `,
    },
    {
      label: 'Workflow: Edit regions',
      value: outdent`
          render(region:pick(title: "Regions")) {

            render(value:edit(
              title: value.name,
              spec: :update {
                name: $value.name,
                comment: $value.comment,
              }
            ))

          }
        `,
    },
  ];
  return (
    <View style={{padding: 10}}>
      <FormField
        label="Choose from one of the example queries below:"
        renderInput={props => (
          <Picker {...props} options={options} value={value} onValueChange={onChange} />
        )}
      />
    </View>
  );
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
        autoComplete="off"
        autoCorrect="off"
        autoCapitalize="off"
        spellCheck="false"
        minRows={3}
        maxRows={15}
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
