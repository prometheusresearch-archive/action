/**
 * @flow
 */

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

/**
 * pickSite:
 *   type: pick
 *   entity: site
 *   fields: [id, code, title]
 */
const pickSite = PickAction.configure({
  id: 'pickSite',
  title: 'Pick site',
  entityName: 'site',
  fields: ['id', 'code', 'title'],
});

/**
 * pickIndividual:
 *   type: pick
 *   entity: individual
 *   fields: [id, code, sex]
 */
const pickIndividual = PickAction.configure({
  id: 'pickIndividual',
  title: 'Pick Individual',
  entityName: 'individual',
  fields: ['id', 'code', 'sex'],
});

/**
 * viewMale:
 *   type: view
 *   entity: individual
 *   fields: [id, code, sex]
 */
const viewMale = ViewAction.configure({
  entityName: 'individual',
  id: 'viewMale',
  renderTitle: () => <Text>View Male</Text>,
  fields: ['id', 'code', 'sex'],
});

/**
 * viewFemale:
 *   type: view
 *   entity: individual
 *   fields: [id, code, sex]
 */
const viewFemale = ViewAction.configure({
  entityName: 'individual',
  id: 'viewFemale',
  renderTitle: () => <Text>View Female</Text>,
  fields: ['id', 'code', 'sex'],
});

/**
 * viewIndividual:
 *   type: view
 *   entity: individual
 *   fields: [id, code, sex]
 */
const viewIndividual = ViewAction.configure({
  entityName: 'individual',
  id: 'viewIndividual',
  title: 'View Individual',
  fields: ['id', 'code', 'sex'],
});

/**
 * viewSite:
 *   type: view
 *   entity: site
 *   fields: [id, code, title]
 *
 * TODO: idea - nested model
 * viewEnrolledIndividual:
 *   type: view
 *   entity: study.study_enrollment.individiual
 *   fields: [id, code, title]
 */
const viewSite = ViewAction.configure({
  entityName: 'site',
  id: 'viewSite',
  title: 'View Site',
  fields: ['id', 'code', 'title'],
});

/**
 * ifMale:
 *   type: guard
 *   require:
 *   - individual
 *   query: individual:find(id=$individual).sex = 'male'
 */
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

/**
 * ifFemale:
 *   type: guard
 *   require:
 *   - individual
 *   query: individual:find(id=$individual).sex = 'female'
 */
const ifFemale = Workflow.guard({
  requires: {individual},

  query(context) {
    return `individual(id: ${context.individual.value.id}) {id,code,sex}`;
  },

  check(context, data) {
    return data.individual.sex === 'female';
  },
});

/**
 * querySiteForIndividual:
 *   type: query
 *   require:
 *   - individual
 *   site: individual:find(id=$individual).site.id
 */
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

/**
 * start:
 * - pickIndividual:
 *   - viewIndividual:
 *     - querySiteForIndividual:
 *       - viewSite
 *   - ifMale:
 *     - viewMale
 *   - ifFemale:
 *     - viewFemale
 * - pickSite:
 *   - viewSite
 */
function createWorkflow() {
  const {sequence, choice} = Workflow;
  return choice([
    sequence([
      pickIndividual,
      choice([
        sequence([viewIndividual, querySiteForIndividual, viewSite]),
        sequence([ifMale, viewMale]),
        sequence([ifFemale, viewFemale]),
      ]),
    ]),
    sequence([pickSite, viewSite]),
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
