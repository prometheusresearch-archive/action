/**
 * @flow
 */

import * as W from '../ConfigWorkflow.js';
import outdent from 'outdent';

test('simple actions', function() {
  const w = W.parseString(outdent`
    workflow:
      start:
        type: pick
        entity: individual
        fields: []
  `);
  expect(w).toMatchSnapshot();
});

test('complex actions', function() {
  const w = W.parseString(outdent`
    workflow:
      pickIndividual:
        type: pick
        entity: individual
        fields: []
      viewIndividual:
        type: view
        entity: individual
        fields: []
      makeIndividual:
        type: make
        entity: individual

      start:
      - pickMakeIndividual:
        - viewIndividual
      - makeIndividual

      pickMakeIndividual:
      - pickIndividual
      - makeIndividual
  `);
  expect(w).toMatchSnapshot();
});

test('deep sequence opt', function() {
  const w = W.parseString(outdent`
    workflow:
      pickIndividual:
        type: pick
        entity: individual
        fields: []
      viewIndividual:
        type: view
        entity: individual
        fields: []
      makeIndividual:
        type: make
        entity: individual

      start:
      - pickIndividual:
        - viewIndividual:
          - makeIndividual
  `);
  expect(w).toMatchSnapshot();
});

test('recursive actions', function() {
  const w = W.parseString(outdent`
    workflow:
      pickIndividual:
        type: pick
        entity: individual
        fields: []
      editIndividual:
        type: make
        entity: individual

      start:
      - pickIndividual:
        - editIndividual:
          - start
  `);
  expect(w).toMatchSnapshot();
});
