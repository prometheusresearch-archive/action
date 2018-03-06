/**
 * @flow
 */

import invariant from 'invariant';
import React, {Component} from 'react';
import {View, Text} from 'react-native-web';
import {Workflow as WorkflowUI} from './Workflow.js';
import * as PickAction from './Pick.js';
import * as ViewAction from './View.js';
import * as Workflow from 'workflow';
import * as t from './types.js';
import {Loading} from './Loading.js';
//import * as apiTypes from 'api/types';

type State = {
  workflow: ?t.Workflow,
};

export default class App extends Component<{}, State> {
  state = {
    workflow: null,
  };

  render() {
    const {workflow} = this.state;
    return (
      <View style={{flex: 1, height: '100%'}}>
        {workflow == null && <Loading />}
        {workflow != null && <WorkflowUI workflow={workflow} />}
      </View>
    );
  }

  async componentDidMount() {
    const workflow = await boot({url: '/graphql'});
    this.setState({workflow});
  }
}

async function boot(config: {url: string}): Promise<t.Workflow> {
  const query = `
    _workflow {
      workflow
    }
  `;
  const resp = await fetch(`${config.url}?query=query{${query}}`);
  const data = await resp.json();
  const workflow: apiTypes.Workflow = data.data._workflow.workflow;
  return configureWorkflow(workflow);
}

function configureWorkflow(workflow: apiTypes.Workflow): t.Workflow {
  const {start, nodes} = workflow;

  function configureAction(action: apiTypes.Action) {
    switch (action.type) {
      case 'PickAction':
        return PickAction.configure({
          id: action.id,
          title: action.title,
          entityName: action.entity,
          fields: action.fields,
        });
      case 'ViewAction':
        return ViewAction.configure({
          id: action.id,
          title: action.title,
          entityName: action.entity,
          fields: action.fields,
        });
      case 'GuardAction': {
        const requires = {};
        for (const {name, type} of action.require) {
          requires[name] = Workflow.entityType(type);
        }
        return Workflow.guard({
          requires,
          query(context) {
            const args = [];
            for (const {name, type} of action.require) {
              args.push(`${name}: "${context[name].value.id}"`);
            }
            return `
              _workflow {
                values {
                  ${action.id}(${args.join(', ')})
                }
              }
            `;
          },
          check(context, data) {
            return Boolean(data._workflow.values[action.id]);
          },
        });
      }
      case 'QueryAction': {
        const requires = {};
        for (const {name, type} of action.require) {
          requires[name] = Workflow.entityType(type);
        }
        return Workflow.query({
          requires,
          provides: {},

          query(context) {
            const args = [];
            for (const {name, type} of action.require) {
              args.push(`${name}: "${context[name].value.id}"`);
            }
            const query = [];
            for (const {type} of action.query) {
              query.push(`${type.name}`);
            }
            return `
              _workflow {
                values {
                  ${action.id}(${args.join(', ')}) {
                    ${query.join(', ')}
                  }
                }
              }
            `;
          },

          update(context, data) {
            const update = {};
            for (const {type} of action.query) {
              update[type.name] = Workflow.entity(type.type, {
                id: data._workflow.values[action.id][type.name],
              });
            }
            return {...context, ...update};
          },
        });
      }
      default:
        invariant(false, 'Unknown action type: %s', action.type);
    }
  }

  function configureNode(node: apiTypes.WorkflowNode) {
    switch (node.type) {
      case 'WorkflowRef':
        return configureNode(nodes[node.ref]);
      case 'WorkflowSequence':
        const sequence = node.sequence.map(node => configureNode(node));
        return Workflow.sequence(sequence);
      case 'WorkflowChoice':
        const choice = node.choice.map(node => configureNode(node));
        return Workflow.choice(choice);
      case 'WorkflowAction':
        return configureAction(node.action);
      default:
        invariant(false, 'Unknown workflow type: %s', node.type);
    }
  }

  const node = configureNode(start);

  return node;
}
