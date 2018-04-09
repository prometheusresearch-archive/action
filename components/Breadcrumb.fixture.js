/**
 * @flow
 */

import * as React from 'react';
import {View, Text} from 'react-native-web';
import {type Fixture, createShowcaseList} from './FixtureUtil.js';
import {Breadcrumb} from './Breadcrumb.js';
import * as cfg from './config.js';

const empty = {
  title: 'Empty',
  element: <Breadcrumb items={[]} />,
};
const oneElement = {
  title: 'One Element',
  element: (
    <Breadcrumb
      items={[
        {
          id: 'home',
          title: 'Home',
        },
      ]}
    />
  ),
};
const manyElements = {
  title: 'Many Elements',
  element: (
    <Breadcrumb
      items={[
        {
          id: 'docs',
          title: 'Documentation',
        },
        {
          id: 'api',
          title: 'API Reference',
        },
        {
          id: 'combinators',
          title: 'Combinators',
        },
      ]}
    />
  ),
};

const customTextColor = {
  title: 'Custom Text Color',
  element: (
    <Breadcrumb
      textColor={cfg.color.indigoDark}
      items={[
        {
          id: 'docs',
          title: 'Documentation',
        },
        {
          id: 'api',
          title: 'API Reference',
        },
        {
          id: 'combinators',
          title: 'Combinators',
        },
      ]}
    />
  ),
};

const ShowcaseList = createShowcaseList(Breadcrumb);

const fixture: Fixture = {
  component: ShowcaseList,
  props: {
    rows: [empty, oneElement, manyElements, customTextColor],
  },
};

export default fixture;
