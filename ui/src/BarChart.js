/**
 * @flow
 */
import * as React from 'react';
import {Bar} from '@vx/shape';
import {Group} from '@vx/group';
import {scaleBand, scaleLinear} from '@vx/scale';
import {max} from 'd3-array';
import {withScreenSize} from '@vx/responsive';
import {Text} from '@vx/text';
import {PatternLines} from '@vx/pattern';
import {AxisBottom} from '@vx/axis';
import type {State} from 'core';
import * as W from 'core';
import {ScreenTitle} from './ScreenTitle.js';
import {View} from 'react-native-web';

// accessors
const x = d => d.label;
const y = d => d.value;

type P = {
  width?: number,
  height?: number,
  state: State,
  toolbar: React.Node,
};

export const BarChart = withScreenSize(({state, width = 400, height = 400}: P) => {
  const {title, data} = W.query(
    state,
    `
      {
        title: title,
        data: data,
      }
    `,
  );
  if (width < 10) return null;

  // bounds
  const xMax = width;
  const yMax = height - 120;

  // scales
  const xScale = scaleBand({
    rangeRound: [0, xMax],
    domain: data.map(x),
    padding: 0.4,
  });
  const yScale = scaleLinear({
    rangeRound: [yMax, 0],
    domain: [0, max(data, y)],
  });

  return (
    <View>
      <ScreenTitle>{title}</ScreenTitle>
      <svg width={width} height={height}>
        <PatternLines
          id="lines"
          height={5}
          width={5}
          stroke={'black'}
          strokeWidth={1}
          orientation={['diagonal']}
        />
        <Group top={40}>
          {data.map((d, i) => {
            const barHeight = yMax - yScale(y(d));
            return (
              <Group key={`bar-${x(d)}`}>
                <Text fontSize={14} x={xScale(x(d))} y={yMax - barHeight - 5}>
                  {y(d)}
                </Text>
                <Bar
                  width={xScale.bandwidth()}
                  height={barHeight}
                  x={xScale(x(d))}
                  y={yMax - barHeight}
                  stroke="#000"
                  strokeWidth={1}
                  fill="url('#lines')"
                  data={{x: x(d), y: y(d)}}
                  onClick={data => event => {
                    alert(`clicked: ${JSON.stringify(data)}`);
                  }}
                />
              </Group>
            );
          })}
        </Group>
        <AxisBottom
          scale={xScale}
          top={yMax + 40}
          label={title}
          stroke={'#1b1a1e'}
          tickTextFill={'#1b1a1e'}
        />
      </svg>
    </View>
  );
});
