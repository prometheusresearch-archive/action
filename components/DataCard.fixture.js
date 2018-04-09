/**
 * @flow
 */

import * as React from 'react';
import {View} from 'react-native-web';
import * as cfg from './config.js';
import {createShowcaseList} from './FixtureUtil.js';
import {DataCard} from './DataCard.js';

const ShowcaseList = createShowcaseList(DataCard);

const simple = {
  title: 'Default',
  element: (
    <DataCard
      data={{
        name: 'Andrey Popp',
        birthdate: '1987-05-08',
      }}
    />
  ),
};

const withTitle = {
  title: 'With Title',
  element: (
    <DataCard
      title="Individual"
      data={{
        name: 'Andrey Popp',
        birthdate: '1987-05-08',
      }}
    />
  ),
};

const withTagLine = {
  title: 'With Tag Line',
  element: (
    <DataCard
      title="Individual"
      tagLine="db.individual.42"
      data={{
        name: 'Andrey Popp',
        birthdate: '1987-05-08',
      }}
    />
  ),
};

const onBackground = {
  title: 'On Background',
  element: (
    <View style={{padding: cfg.padding.size4, backgroundColor: cfg.color.white}}>
      <DataCard
        title="Individual"
        tagLine="db.individual.42"
        data={{
          name: 'Andrey Popp',
          birthdate: '1987-05-08',
        }}
      />
    </View>
  ),
};

const customColor = {
  title: 'Custom Color',
  element: (
    <DataCard
      outlineColor={cfg.color.indigoDark}
      title="Individual"
      tagLine="db.individual.42"
      data={{
        name: 'Andrey Popp',
        birthdate: '1987-05-08',
      }}
    />
  ),
};

const customColorOnBg = {
  title: 'Custom Color (On Background)',
  element: (
    <View style={{padding: cfg.padding.size4, backgroundColor: cfg.color.pinkLightest}}>
      <DataCard
        outlineColor={cfg.color.indigoDark}
        title="Individual"
        tagLine="db.individual.42"
        data={{
          name: 'Andrey Popp',
          birthdate: '1987-05-08',
        }}
      />
    </View>
  ),
};

const labelOverflow = {
  title: 'Label Overflow',
  element: (
    <View
      style={{padding: cfg.padding.size4, backgroundColor: cfg.color.white, width: 500}}>
      <DataCard
        title="Individual"
        tagLine="db.individual.42"
        data={{
          aVeryLongLabelOkWorks: 'Hi',
        }}
      />
    </View>
  ),
};

export default {
  component: ShowcaseList,
  props: {
    rows: [
      simple,
      withTitle,
      withTagLine,
      onBackground,
      customColor,
      customColorOnBg,
      labelOverflow,
    ],
  },
};
