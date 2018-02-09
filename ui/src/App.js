/**
 * @flow
 */

import React, {Component} from 'react';
import logo from './logo.svg';
import './App.css';
import type {Action} from 'api/Workflow';
import {Workflow} from './Workflow.js';

type State = {
  data: mixed,
};

const pickIndividual: Action = {
  type: 'View',
  id: 'pickIndividual',

  requires: {},
  provides: {
    individual: {type: 'EntityType', name: 'individual', fields: {}},
  },

  query(context) {
    return `individual__list {id,code}`;
  },

  render(context, data, onContext) {
    const items = data.individual__list.map(individual => {
      const onClick = id => () => {
        onContext({individual: {id, __type: 'individual'}});
      };
      return (
        <div key={individual.code} onClick={onClick(individual.id)}>
          {individual.code}
        </div>
      );
    });
    return (
      <div>
        <h3>Pick Individual</h3>
        <div>{items}</div>
      </div>
    );
  },
};

const viewIndividual: Action = {
  type: 'View',
  id: 'viewIndividual',

  requires: {
    individual: {type: 'EntityType', name: 'individual', fields: {}},
  },
  provides: {},

  query(context) {
    return `individual(id: ${context.individual.id}) {id,code,sex}`;
  },

  render(context, data, onContext) {
    return (
      <div>
        <h3>View Individual</h3>
        <pre>{JSON.stringify(data.individual, null, 2)}</pre>
      </div>
    );
  },
};

const workflow = {
  type: 'Sequence',
  id: 'sequence',
  sequence: [pickIndividual, viewIndividual],
};

export default class App extends Component<{}, State> {
  render() {
    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to React</h1>
        </header>
        <p className="App-intro">
          To get started, edit <code>src/App.js</code> and save to reload.
        </p>
        <Workflow initialAction={workflow} debugState={true} />
      </div>
    );
  }
}
