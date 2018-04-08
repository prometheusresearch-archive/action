/**
 * @flow
 */

import * as React from 'react';
import {ScrollView, View, Text} from 'react-native-web';
import type {FixtureList} from './FixtureUtil.js';
import {OutlineButton} from './OutlineButton.js';
import * as cfg from './config.js';

const backgroundColor = cfg.color.white;
const outlineColor = cfg.color.black;

function PrimaryButton({label}) {
  return <OutlineButton label={label} strokeColor={cfg.color.black} size="large" />;
}

function Slide({children, backgroundColor}) {
  return (
    <View
      style={{
        height: '80vh',
        width: '100%',
        alignItems: 'center',
        justifyContent: 'center',
        backgroundColor: backgroundColor,
      }}>
      {children}
    </View>
  );
}

function InfoSlide({children}) {
  return (
    <Slide backgroundColor={backgroundColor}>
      <View
        style={{
          height: '100%',
          width: '100%',
          padding: cfg.padding.size6,
        }}>
        <View
          style={{
            borderRadius: cfg.borderRadius.default,
            height: '100%',
            width: '100%',
            borderColor: outlineColor,
            borderWidth: cfg.borderWidth.size2,
            padding: cfg.padding.size8,
          }}>
          {children}
        </View>
      </View>
    </Slide>
  );
}

function SlideTitle({title}) {
  return (
    <Text
      style={{
        fontFamily: cfg.fontFamily.sans,
        fontSize: cfg.fontSize.xxxxLarge,
        fontWeight: cfg.fontWeight.extrabold,
        color: outlineColor,
      }}>
      {title}
    </Text>
  );
}

function Landing() {
  return (
    <ScrollView
      style={{
        height: '100vh',
        width: '100%',
      }}>
      <Slide backgroundColor={backgroundColor}>
        <View>
          <Text
            style={{
              fontFamily: cfg.fontFamily.sans,
              color: cfg.color.black,
              fontSize: cfg.fontSize.xxxxxLarge,
              fontWeight: cfg.fontWeight.black,
            }}>
            Action
          </Text>
          <Text
            style={{
              fontFamily: cfg.fontFamily.sans,
              color: cfg.color.black,
              fontSize: cfg.fontSize.xxLarge,
              fontWeight: cfg.fontWeight.semibold,
            }}>
            Language for Composable Data-Driven Workflows
          </Text>
        </View>
        <View style={{flexDirection: 'row', padding: cfg.padding.size4}}>
          <View style={{borderWidth: '1em', borderColor: 'transparent'}}>
            <PrimaryButton label="Try Action" />
          </View>
          <View style={{borderWidth: '1em', borderColor: 'transparent'}}>
            <PrimaryButton label="Documentation" />
          </View>
        </View>
      </Slide>
      <InfoSlide>
        <SlideTitle title="Motivation" />
      </InfoSlide>
      <InfoSlide>
        <SlideTitle title="Structure" />
      </InfoSlide>
      <InfoSlide>
        <SlideTitle title="Research" />
      </InfoSlide>
      <Slide backgroundColor={backgroundColor}>
        <Text
          style={{
            color: cfg.color.black,
            fontFamily: cfg.fontFamily.sans,
            fontWeight: cfg.fontWeight.semibold,
            fontSize: cfg.fontSize.xLarge,
          }}>
          Prometheus Research, LLC
        </Text>
      </Slide>
    </ScrollView>
  );
}

const fixtures: FixtureList = [
  {
    component: Landing,
    props: {},
  },
];

export default fixtures;
