/**
 * @flow
 */

import * as React from 'react';
import type {Action, Context, Query, Data} from 'api/Workflow';
import * as Executor from 'api/WorkflowExecutor';

type Props = {
  initialAction: Action,
  initialContext: Context,
};

type State = {
  context: Context,
  data: Data,
  onContext: Context => void,
  render: (Context, Data, (Context) => void) => React.Element<*>,
};

export class Workflow extends React.Component<Props, State> {
  static defaultProps = {
    initialContext: {},
  };

  constructor(props: Props) {
    super(props);
    this.state = {
      context: {},
      data: {},
      onContext: _context => {},
      render: (context, data, onContext) => <div>loading</div>,
    };
  }

  waitForUserInput = (
    context: Context,
    data: Data,
    onContext: Context => void,
    render: (Context, Data, (Context) => void) => React.Element<*>,
  ) => {
    this.setState({render, context, data, onContext});
  };

  waitForData = async (query: Query<*>) => {
    const resp = await fetch(`http://localhost:3001/graphql?query=query{${query}}`);
    const data = await resp.json();
    return data.data;
  };

  render() {
    const {context, data, onContext, render} = this.state;
    return <div>{render(context, data, onContext)}</div>;
  }

  componentDidMount() {
    Executor.run({
      initialAction: this.props.initialAction,
      initialContext: this.props.initialContext,
      waitForUserInput: this.waitForUserInput,
      waitForData: this.waitForData,
    });
  }
}
