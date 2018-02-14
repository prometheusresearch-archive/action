/**
 * @flow
 */

import './App.css';

import React, {Component} from 'react';
import {View, Text} from 'react-native-web';
import {Workflow as WorkflowUI} from './Workflow.js';
import * as PickAction from './Pick.js';
import * as ViewAction from './View.js';
import * as Workflow from 'workflow';

type State = {
  data: mixed,
};

const individual = Workflow.entityType('individual');
const site = Workflow.entityType('site');

const pickIndividual = PickAction.configure({
  id: 'pickIndividual',
  title: 'Pick Individual',
  entityName: 'individual',
  fields: ['id', 'code', 'sex'],
});

const viewMale = ViewAction.configure({
  entityName: 'individual',
  id: 'viewMale',
  renderTitle: () => <Text>View Male</Text>,
  fields: ['id', 'code', 'sex'],
});

const viewFemale = ViewAction.configure({
  entityName: 'individual',
  id: 'viewFemale',
  renderTitle: () => <Text>View Female</Text>,
  fields: ['id', 'code', 'sex'],
});

const viewIndividual = ViewAction.configure({
  entityName: 'individual',
  id: 'viewIndividual',
  title: 'View Individual',
  fields: ['id', 'code', 'sex'],
});

const viewSite = ViewAction.configure({
  entityName: 'site',
  id: 'viewSite',
  title: 'View Site',
  fields: ['id', 'code', 'title'],
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
  requires: {individual},

  query(context) {
    return `individual(id: ${context.individual.value.id}) {id,code,sex}`;
  },

  check(context, data) {
    return data.individual.sex === 'female';
  },
});

const querySiteForIndividual = Workflow.query({
  requires: {individual},
  provides: {site},

  query(context) {
    return `individual(id: ${context.individual.value.id}) {site{id}}`;
  },

  update(context, data) {
    const site = Workflow.entity('site', {id: data.individual.site.id});
    return {...context, site};
  },
});

function createWorkflow() {
  const {sequence, choice, action} = Workflow;
  return sequence([
    action(pickIndividual),
    choice([
      sequence([
        action(viewIndividual),
        action(querySiteForIndividual),
        action(viewSite),
      ]),
      sequence([action(ifMale), action(viewMale)]),
      sequence([action(ifFemale), action(viewFemale)]),
    ]),
  ]);
}

const workflow = createWorkflow();

export default class App extends Component<{}, State> {
  render() {
    return (
      <View style={{flex: 1, height: '100%'}}>
        <WorkflowUI workflow={workflow} />
      </View>
    );
  }
}
