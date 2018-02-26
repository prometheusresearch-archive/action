/**
 * @flow
 */

const invariant = require('invariant');
const yaml = require('yaml-js');

import * as t from './types.js';
import * as Universe from './Universe.js';
import * as fs from './lib/fs';
import * as ConfigWorkflow from './ConfigWorkflow.js';

type Config = {
  entity?: {[name: string]: EntityConfig},
  workflow: ConfigWorkflow.Workflow,
};

type EntityConfig = {
  entity: string,
  where?: string,
  fields?: {[name: string]: string},
};

export async function configureOf(
  univ: t.Universe,
  filename: string,
): Promise<{workflow: ConfigWorkflow.Workflow, univ: t.Universe}> {
  const data = await fs.readFile(filename);

  // TODO: validate instead
  // flowlint unclear-type:off
  const struct: Config = (yaml.load(data): any);

  // Create a fresh universe per workflow.
  const ownUniv = Universe.fresh(univ);

  // Read entity config.
  if (struct.entity != null) {
    for (const name in struct.entity) {
      const {
        entity: baseEntityTypeName,
        where: maskRefinement,
        fields: fieldsRefinement,
      } = struct.entity[name];
      const baseEntityType = univ.entity[baseEntityTypeName];
      invariant(
        baseEntityType != null,
        'Refinement of the unknown entity type: %s',
        baseEntityTypeName,
      );
      let fields = baseEntityType.fields;

      if (fieldsRefinement != null) {
        fields = {...fields};
        for (const name in fieldsRefinement) {
          invariant(
            fields[name] == null,
            'Refinement of the existing field name %s for entity: %s',
            name,
            baseEntityTypeName,
          );
          // TODO: handle types somehow
          fields[name] = {type: 'StringType'};
        }
      }
      Universe.defineEntity(ownUniv, {
        type: 'EntityType',
        name,
        fields,
        table: baseEntityType.table,
      });
    }
  }

  const node = yaml.compose(data);
  const workflow = ConfigWorkflow.parse(node).workflow;

  return {univ: ownUniv, workflow};
}
