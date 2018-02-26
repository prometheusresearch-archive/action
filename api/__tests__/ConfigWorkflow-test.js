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
  `);
  expect(w).toMatchSnapshot();
});

test('complex actions', function() {
  const w = W.parseString(outdent`
    workflow:
      pick-individual:
        type: pick
        entity: individual
      view-individual:
        type: view
        entity: individual
      make-individual:
        type: make
        entity: individual

      start:
      - pick-make-individual:
        - view-individual
      - make-individual

      pick-make-individual:
      - pick-individual
      - make-individual
  `);
  expect(w).toMatchSnapshot();
});

test('deep sequence opt', function() {
  const w = W.parseString(outdent`
    workflow:
      pick-individual:
        type: pick
        entity: individual
      view-individual:
        type: view
        entity: individual
      make-individual:
        type: make
        entity: individual

      start:
      - pick-individual:
        - view-individual:
          - make-individual
  `);
  expect(w).toMatchSnapshot();
});

test('recursive actions', function() {
  const w = W.parseString(outdent`
    workflow:
      pick-individual:
        type: pick
        entity: individual
      edit-individual:
        type: make
        entity: individual

      start:
      - pick-individual:
        - edit-individual:
          - start
  `);
  expect(w).toMatchSnapshot();
});
