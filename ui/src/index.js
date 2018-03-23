/**
 * @flow
 */

import './index.css';

import React from 'react';
import ReactDOM from 'react-dom';
import {App} from './App';
import * as W from 'workflow';

const container = document.getElementById('root');
if (container != null) {
  ReactDOM.render(<App startState={W.start} />, container);
} else {
  console.error('unable to find #root element');
}
