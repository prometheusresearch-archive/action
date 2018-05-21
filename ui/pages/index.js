/**
 * @flow
 */

import type {PageProps} from 'next';
import React from 'react';
import * as Core from 'core';
import {StyleSheet, Text, View} from 'react-native-web';
import {App} from '../ui/App.js';
import {Workflow} from '../ui/Workflow.js';

const workflowConfig = Core.parseWorkflow(`
  main =
    | goto regionWorkflow
    | goto nationWorkflow

  regionWorkflow =
    render region:pick(title: "Regions");
    goto regionWorkflowHandle

  regionWorkflowHandle =
    | render value:view
    | render value:form(spec: :update { name: $value.name });
      goto regionWorkflowHandle

  nationWorkflow =
    render nation:pick(title: "Nations");
    render value:view
`);

export default (props: PageProps) => (
  <App {...props}>
    <Workflow db={Core.db} workflowConfig={workflowConfig} />
  </App>
);
