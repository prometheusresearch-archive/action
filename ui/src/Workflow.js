/**
 * @flow
 */

import invariant from 'invariant';
import * as React from 'react';
import * as W from 'workflow';
import {View, Text} from 'react-native-web';
import {Breadcrumb} from './Breadcrumb.js';
import {NextToolbar} from './NextToolbar.js';
import {AlternativesToolbar} from './AlternativesToolbar.js';
import * as types from './types.js';

type Props = {
  workflow: types.Workflow,
};

type State = {
  frame: types.Frame,
  interaction: ?types.Interaction,
};

export class Workflow extends React.Component<Props, State> {
  config: W.Config;

  waitForData = async (query: W.Query) => {
    const resp = await fetch(`http://localhost:3001/graphql?query=query{${query}}`);
    const data = await resp.json();
    return data.data;
  };

  constructor(props: Props) {
    super(props);
    this.config = {waitForData: this.waitForData};
    this.state = {
      frame: W.init(props.workflow),
      interaction: null,
    };
  }

  onContext = async (nextContext: W.Context) => {
    const {interaction, frame} = await W.nextToInteraction(
      this.config,
      nextContext,
      this.state.frame,
    );
    if (interaction != null) {
      this.setState({interaction, frame});
    }
  };

  onBreadcrumbClick = async (p: types.Interaction) => {
    const {interaction, frame} = await W.runToInteraction(this.config, p.frame);
    if (interaction != null) {
      this.setState({interaction, frame});
    }
  };

  async componentDidMount() {
    const {interaction, frame} = await W.runToInteraction(this.config, this.state.frame);
    if (interaction != null) {
      this.setState({interaction, frame});
    }
  }

  render() {
    const {interaction} = this.state;
    console.log('INTERACTION', interaction);
    if (interaction == null) {
      return (
        <View style={{flex: 1, padding: 10}}>
          <Text>Loading...</Text>
        </View>
      );
    }
    const {context, data, dataTitle, ui, prev, next, alternatives} = interaction;
    invariant(data != null, 'Data was not loaded');
    const title = ui.renderTitle(context, dataTitle);
    return (
      <View style={{flex: 1}}>
        <View>
          <Breadcrumb
            current={interaction}
            trace={prev}
            onClick={this.onBreadcrumbClick}
          />
        </View>
        {alternatives != null &&
          alternatives.length > 0 && (
            <View>
              <AlternativesToolbar
                current={interaction}
                alternatives={alternatives}
                onClick={this.onBreadcrumbClick}
              />
            </View>
          )}
        <View style={{padding: 10, flex: 1}}>
          <View style={{paddingBottom: 10}}>
            <Text style={{fontWeight: '600', fontSize: 18}}>{title}</Text>
          </View>
          {next != null &&
            next.length > 0 && (
              <View>
                <NextToolbar next={next} onClick={this.onBreadcrumbClick} />
              </View>
            )}
          <View style={{flex: 1}}>{ui.render(context, data, this.onContext)}</View>
        </View>
      </View>
    );
  }
}
