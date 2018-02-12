/**
 * @flow
 */

import './App.css';

import React, {Component} from 'react';
import type {Action} from 'api/Workflow';
import {View} from 'react-native-web';
import {Workflow} from './Workflow.js';
import * as Pick from './Pick.js';

type State = {
  data: mixed,
};

const pickIndividual: Action = Pick.configure({
  id: 'pickIndividual',
  title: 'Pick Individual',
  entityName: 'individual',
  fields: ['id', 'code', 'sex'],
});

const viewMale: Action = {
  type: 'View',
  id: 'viewMale',
  title: 'View Male',

  requires: {
    individual: {type: 'EntityType', name: 'individual'},
  },
  provides: {},

  query(context) {
    return `individual(id: ${context.individual.id}) {id,code,sex}`;
  },

  render(context, data, onContext) {
    return (
      <div>
        <pre>{JSON.stringify(data.individual, null, 2)}</pre>
      </div>
    );
  },
};

const viewFemale: Action = {
  type: 'View',
  id: 'viewFemale',
  title: 'View Female',

  requires: {
    individual: {type: 'EntityType', name: 'individual'},
  },
  provides: {},

  query(context) {
    return `individual(id: ${context.individual.id}) {id,code,sex}`;
  },

  render(context, data, onContext) {
    return (
      <div>
        <pre>{JSON.stringify(data.individual, null, 2)}</pre>
      </div>
    );
  },
};

const ifMale: Action = {
  type: 'Guard',
  id: 'ifMale',

  requires: {
    individual: {type: 'EntityType', name: 'individual'},
  },
  provides: {},

  query(context) {
    return `individual(id: ${context.individual.id}) {id,code,sex}`;
  },

  allowed(context, data) {
    return data.individual.sex === 'male';
  },
};

function seq(...actions) {
  return {
    type: 'Sequence',
    id: 'sequence',
    sequence: actions,
  };
}

function choice(...actions) {
  return {
    type: 'Choice',
    id: 'choice',
    choice: actions,
  };
}

const workflow = seq(pickIndividual, choice(seq(ifMale, viewMale), viewFemale));

export default class App extends Component<{}, State> {
  render() {
    return (
      <View style={{flex: 1, height: '100%'}}>
        <Workflow initialAction={workflow} />
      </View>
    );
  }
}
