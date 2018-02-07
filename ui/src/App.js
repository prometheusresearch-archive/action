/**
 * @flow
 */

import React, {Component} from 'react';
import logo from './logo.svg';
import './App.css';

type State = {
  data: mixed,
};

class App extends Component<{}, State> {
  state = {data: null};
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
        <pre>{JSON.stringify(this.state.data, null, 2)}</pre>
      </div>
    );
  }

  async componentDidMount() {
    const resp = await fetch('/graphql?query={individual__list{code}}');
    const data = await resp.json();
    this.setState({data});
  }
}

export default App;
