/**
 * @noflow
 */

import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import App from './App';

const container = document.getElementById('root');
if (container != null) {
  ReactDOM.render(<App />, container);
} else {
  console.error('unable to find #root element');
}
