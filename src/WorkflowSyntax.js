/**
 * @flow
 */

const invariant = require('invariant');
const YAML = require('yaml-js');
const {MappingNode, SequenceNode} = require('yaml-js/lib/nodes');

import * as Workflow from './Workflow.js';

export function fromString(yaml: string): Workflow.Action {
  const node = YAML.compose(yaml);
  return fromNode(node);
}

export function fromNode(node: YAML.Node): Workflow.Action {
  invariant(node instanceof MappingNode, 'expected MappingNode');

  const thunks = [];

  return null;
}
