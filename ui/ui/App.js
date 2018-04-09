/**
 * @flow
 */

import * as React from 'react';
import Router from 'next/router';
import * as cfg from 'components/config';
import {Nav, NavButton} from 'components/Nav';
import {StyleSheet, Text, View} from 'react-native-web';

const style = StyleSheet.create({
  base: {
    backgroundColor: cfg.color.white,
    height: '100vh',
  },
  content: {
    padding: cfg.padding.size4,
  },
});

type P = {
  url: {pathname: string},
  children: React.Node,
};

export class App extends React.Component<P> {
  onActive = (ev: {id: string}) => {
    Router.push(ev.id);
  };

  render() {
    const items = [
      {id: '/', render: props => <NavButton {...props} title="Home" />},
      {id: '/demo', render: props => <NavButton {...props} title="Demo" />},
    ];
    return (
      <View style={style.base}>
        <Nav
          active={this.props.url.pathname}
          onActive={this.onActive}
          items={items}
          breadcrumb={[]}
        />
        <View style={style.content}>{this.props.children}</View>
      </View>
    );
  }
}
