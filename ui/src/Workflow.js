/**
 * @flow
 */

import * as React from 'react';
import * as W from 'workflow';
import {View, Button, Text, TouchableOpacity} from 'react-native-web';

function Breadcrumb({
  current,
  trace,
  onClick,
}: {
  current: W.Interaction<UI>,
  trace: Array<W.Interaction<UI>>,
  onClick: (W.Interaction<UI>) => *,
}) {
  const items = [];
  for (const item of trace) {
    const onPress = item.frame != null ? () => onClick(item) : null;
    items.push({title: item.ui.renderTitle(item.context, item.dataTitle), onPress});
  }

  const buttons = items.map((item, idx) => {
    const style = {paddingRight: 10, fontWeight: '200'};
    return (
      <TouchableOpacity key={idx} onPress={item.onPress}>
        <Text style={style}>{item.title}</Text>
      </TouchableOpacity>
    );
  });

  buttons.push(
    <Text key="current" style={{paddingRight: 10, fontWeight: '600'}}>
      {current.ui.renderTitle(current.context, current.dataTitle)}
    </Text>,
  );

  return (
    <View style={{flexDirection: 'row', padding: 10}}>
      <View>
        <Text>Prev: </Text>
      </View>
      <View style={{flexDirection: 'row'}}>{buttons}</View>
    </View>
  );
}

function Next({
  next,
  onClick,
}: {
  next: Array<W.Interaction<UI>>,
  onClick: (W.Interaction<UI>) => *,
}) {
  const items = [];
  for (const item of next) {
    const onPress = item.frame != null ? () => onClick(item) : null;
    const title = item.ui.renderTitle(item.context, item.dataTitle);
    items.push({title, onPress});
  }

  const buttons = items.map((item, idx) => {
    const style = {paddingRight: 10, fontWeight: '200'};
    return (
      <TouchableOpacity key={idx} onPress={item.onPress}>
        <Text style={style}>{item.title}</Text>
      </TouchableOpacity>
    );
  });

  return (
    <View style={{flexDirection: 'row', padding: 10}}>
      <View>
        <Text>Next: </Text>
      </View>
      <View style={{flexDirection: 'row'}}>{buttons}</View>
    </View>
  );
}

function Alternatives({
  current,
  alternatives,
  onClick,
}: {
  current: W.Interaction<UI>,
  alternatives: Array<W.Interaction<UI>>,
  onClick: (W.Interaction<UI>) => *,
}) {
  const items = [];
  for (const item of alternatives) {
    const onPress = item.frame != null ? () => onClick(item) : null;
    const title = item.ui.renderTitle(item.context, item.dataTitle);
    const active = current.ui.id === item.ui.id;
    items.push({title, onPress, active});
  }

  const buttons = items.map((item, idx) => {
    const style = {paddingRight: 10, fontWeight: item.active ? '600' : '200'};
    return (
      <TouchableOpacity key={idx} onPress={item.onPress}>
        <Text style={style}>{item.title}</Text>
      </TouchableOpacity>
    );
  });

  return (
    <View style={{flexDirection: 'row', padding: 10}}>
      <View>
        <Text>Alternatives: </Text>
      </View>
      <View style={{flexDirection: 'row'}}>{buttons}</View>
    </View>
  );
}

export type UI = {
  +id: string,
  +renderTitle: (context: W.Context, data: ?W.DataSet) => React.Element<*>,
  +render: (
    context: W.Context,
    data: W.DataSet,
    onContext: (W.Context) => *,
  ) => React.Element<*>,
};

type Props = {
  workflow: W.Workflow<UI>,
};

type State = {
  frame: W.Frame<UI>,
  interaction: ?W.Interaction<UI>,
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

  onBreadcrumbClick = async (p: W.Interaction<UI>) => {
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
    const {interaction, frame} = this.state;
    console.log('INTERACTION', interaction);
    if (interaction == null) {
      return (
        <View style={{flex: 1, padding: 10}}>
          <Text>Loading...</Text>
        </View>
      );
    }
    const {context, data, dataTitle, ui, prev, next, alternatives} = interaction;
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
              <Alternatives
                current={interaction}
                alternatives={alternatives}
                onClick={this.onBreadcrumbClick}
              />
            </View>
          )}
        {next != null &&
          next.length > 0 && (
            <View>
              <Next next={next} onClick={this.onBreadcrumbClick} />
            </View>
          )}
        <View style={{padding: 10, flex: 1}}>
          <View style={{paddingBottom: 10}}>
            <Text style={{fontWeight: '600', fontSize: 18}}>{title}</Text>
          </View>
          <View style={{flex: 1}}>{ui.render(context, data, this.onContext)}</View>
        </View>
      </View>
    );
  }
}
