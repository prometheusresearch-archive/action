/**
 * @noflow
 */

import './index.css';

import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';

const container = document.getElementById('root');
if (container != null) {
  ReactDOM.render(<App />, container);
} else {
  console.error('unable to find #root element');
}
