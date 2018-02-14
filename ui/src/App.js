/**
 * @flow
 */

import './App.css';

import React, {Component} from 'react';
import {View} from 'react-native-web';
import {Workflow as WorkflowUI} from './Workflow.js';
import * as Pick from './Pick.js';
import * as Workflow from 'workflow';

type State = {
  data: mixed,
};
const pickIndividual = Pick.configure({
  id: 'pickIndividual',
  title: 'Pick Individual',
  entityName: 'individual',
  fields: ['id', 'code', 'sex'],
});

const viewMale = Workflow.interaction({
  requires: {
    individual: Workflow.entityType('individual'),
  },
  provides: {},
  query(context) {
    return `individual(id: ${context.individual.value.id}) {id,code,sex}`;
  },
  ui: {
    id: 'viewMale',
    title: 'View Male',

    render(context, data, onContext) {
      return (
        <div>
          <pre>{JSON.stringify(data.individual, null, 2)}</pre>
        </div>
      );
    },
  },
});

const viewFemale = Workflow.interaction({
  requires: {
    individual: Workflow.entityType('individual'),
  },
  provides: {},
  query(context) {
    return `individual(id: ${context.individual.value.id}) {id,code,sex}`;
  },
  ui: {
    id: 'viewFemale',
    title: 'View Female',

    render(context, data, onContext) {
      return (
        <div>
          <pre>{JSON.stringify(data.individual, null, 2)}</pre>
        </div>
      );
    },
  },
});

const viewIndividual = Workflow.interaction({
  requires: {
    individual: Workflow.entityType('individual'),
  },
  provides: {},
  query(context) {
    return `individual(id: ${context.individual.value.id}) {id,code,sex}`;
  },
  ui: {
    id: 'viewIndividual',
    title: 'View Individual',

    render(context, data, onContext) {
      return (
        <div>
          <pre>{JSON.stringify(data.individual, null, 2)}</pre>
        </div>
      );
    },
  },
});

const ifMale = Workflow.guard({
  requires: {
    individual: Workflow.entityType('individual'),
  },

  query(context) {
    return `individual(id: ${context.individual.value.id}) {id,code,sex}`;
  },

  check(context, data) {
    return data.individual.sex === 'male';
  },
});

const ifFemale = Workflow.guard({
  requires: {
    individual: Workflow.entityType('individual'),
  },

  query(context) {
    return `individual(id: ${context.individual.value.id}) {id,code,sex}`;
  },

  check(context, data) {
    return data.individual.sex === 'female';
  },
});

const workflow = Workflow.sequence([
  Workflow.action(pickIndividual),
  Workflow.choice([
    Workflow.sequence([Workflow.action(ifMale), Workflow.action(viewMale)]),
    Workflow.sequence([Workflow.action(ifFemale), Workflow.action(viewFemale)]),
    Workflow.action(viewIndividual),
  ]),
]);

export default class App extends Component<{}, State> {
  render() {
    return (
      <View style={{flex: 1, height: '100%'}}>
        <WorkflowUI workflow={workflow} />
      </View>
    );
  }
}
