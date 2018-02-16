/**
 * @flow
 */

import type {Element} from 'react';
import type {
  Context,
  DataSet,
  Interaction as BaseInteraction,
  Frame as BaseFrame,
  Workflow as BaseWorkflow,
} from 'workflow';

export type {Context, DataSet};

export type UI = {
  +id: string,
  +renderTitle: (context: Context, data: ?DataSet) => Element<*>,
  +render: (context: Context, data: DataSet, onContext: (Context) => *) => Element<*>,
};

export type Interaction = BaseInteraction<UI>;
export type Workflow = BaseWorkflow<UI>;
export type Frame = BaseFrame<UI>;
