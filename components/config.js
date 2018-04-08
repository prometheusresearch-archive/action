/*
 * @flow
 *
 * Based on Tailwind - The Utility-First CSS Framework
 *
 * @license MIT
 */

let color = {
  transparent: 'transparent',

  black: '#22292f',
  greyDarkest: '#3d4852',
  greyDarker: '#606f7b',
  greyDark: '#8795a1',
  grey: '#b8c2cc',
  greyLight: '#dae1e7',
  greyLighter: '#f1f5f8',
  greyLightest: '#f8fafc',
  white: '#ffffff',

  redDarkest: '#3b0d0c',
  redDarker: '#621b18',
  redDark: '#cc1f1a',
  red: '#e3342f',
  redLight: '#ef5753',
  redLighter: '#f9acaa',
  redLightest: '#fcebea',

  orangeDarkest: '#462a16',
  orangeDarker: '#613b1f',
  orangeDark: '#de751f',
  orange: '#f6993f',
  orangeLight: '#faad63',
  orangeLighter: '#fcd9b6',
  orangeLightest: '#fff5eb',

  yellowDarkest: '#453411',
  yellowDarker: '#684f1d',
  yellowDark: '#f2d024',
  yellow: '#ffed4a',
  yellowLight: '#fff382',
  yellowLighter: '#fff9c2',
  yellowLightest: '#fcfbeb',

  greenDarkest: '#0f2f21',
  greenDarker: '#1a4731',
  greenDark: '#1f9d55',
  green: '#38c172',
  greenLight: '#51d88a',
  greenLighter: '#a2f5bf',
  greenLightest: '#e3fcec',

  tealDarkest: '#0d3331',
  tealDarker: '#20504f',
  tealDark: '#38a89d',
  teal: '#4dc0b5',
  tealLight: '#64d5ca',
  tealLighter: '#a0f0ed',
  tealLightest: '#e8fffe',

  blueDarkest: '#12283a',
  blueDarker: '#1c3d5a',
  blueDark: '#2779bd',
  blue: '#3490dc',
  blueLight: '#6cb2eb',
  blueLighter: '#bcdefa',
  blueLightest: '#eff8ff',

  indigoDarkest: '#191e38',
  indigoDarker: '#2f365f',
  indigoDark: '#5661b3',
  indigo: '#6574cd',
  indigoLight: '#7886d7',
  indigoLighter: '#b2b7ff',
  indigoLightest: '#e6e8ff',

  purpleDarkest: '#21183c',
  purpleDarker: '#382b5f',
  purpleDark: '#794acf',
  purple: '#9561e2',
  purpleLight: '#a779e9',
  purpleLighter: '#d6bbfc',
  purpleLightest: '#f3ebff',

  pinkDarkest: '#451225',
  pinkDarker: '#6f213f',
  pinkDark: '#eb5286',
  pink: '#f66d9b',
  pinkLight: '#fa7ea8',
  pinkLighter: '#ffbbca',
  pinkLightest: '#ffebef',
};

module.exports = {
  color: color,

  screen: {
    small: '576px',
    medium: '768px',
    large: '992px',
    xLarge: '1200px',
  },

  fontFamily: {
    sans: [
      'system-ui',
      'BlinkMacSystemFont',
      '-apple-system',
      'Segoe UI',
      'Roboto',
      'Oxygen',
      'Ubuntu',
      'Cantarell',
      'Fira Sans',
      'Droid Sans',
      'Helvetica Neue',
      'sans-serif',
    ].join(', '),
    serif: [
      'Constantia',
      'Lucida Bright',
      'Lucidabright',
      'Lucida Serif',
      'Lucida',
      'DejaVu Serif',
      'Bitstream Vera Serif',
      'Liberation Serif',
      'Georgia',
      'serif',
    ].join(', '),
    mono: [
      'Menlo',
      'Monaco',
      'Consolas',
      'Liberation Mono',
      'Courier New',
      'monospace',
    ].join(', '),
  },

  fontSize: {
    xSmall: '.75rem', // 12px
    small: '.875rem', // 14px
    base: '1rem', // 16px
    large: '1.125rem', // 18px
    xLarge: '1.25rem', // 20px
    xxLarge: '1.5rem', // 24px
    xxxLarge: '1.875rem', // 30px
    xxxxLarge: '2.25rem', // 36px
    xxxxxLarge: '3rem', // 48px
  },

  fontWeight: {
    hairline: '100',
    thin: '200',
    light: '300',
    normal: '400',
    medium: '500',
    semibold: '600',
    bold: '700',
    extrabold: '800',
    black: '900',
  },

  lineHeight: {
    none: 1,
    tight: 1.25,
    normal: 1.5,
    loose: 2,
  },

  backgroundColor: color,

  backgroundSize: {
    auto: 'auto',
    cover: 'cover',
    contain: 'contain',
  },

  borderWidth: {
    default: '1px',
    size0: '0',
    size2: '2px',
    size4: '4px',
    size8: '8px',
  },

  borderColor: global.Object.assign({default: color.greyLight}, color),

  borderRadius: {
    none: '0',
    small: '.125rem',
    default: '.25rem',
    large: '.5rem',
    full: '9999px',
  },

  width: {
    auto: 'auto',
    px: '1px',
    size1: '0.25rem',
    size2: '0.5rem',
    size3: '0.75rem',
    size4: '1rem',
    size6: '1.5rem',
    size8: '2rem',
    size10: '2.5rem',
    size12: '3rem',
    size16: '4rem',
    size24: '6rem',
    size32: '8rem',
    size48: '12rem',
    size64: '16rem',
    size1of2: '50%',
    size1of3: '33.33333%',
    size2of3: '66.66667%',
    size1of4: '25%',
    size3of4: '75%',
    size1of5: '20%',
    size2of5: '40%',
    size3of5: '60%',
    size4of5: '80%',
    size1of6: '16.66667%',
    size5of6: '83.33333%',
    full: '100%',
    screen: '100vw',
  },

  height: {
    auto: 'auto',
    px: '1px',
    size1: '0.25rem',
    size2: '0.5rem',
    size3: '0.75rem',
    size4: '1rem',
    size6: '1.5rem',
    size8: '2rem',
    size10: '2.5rem',
    size12: '3rem',
    size16: '4rem',
    size24: '6rem',
    size32: '8rem',
    size48: '12rem',
    size64: '16rem',
    full: '100%',
    screen: '100vh',
  },

  minWidth: {
    full: '100%',
  },

  minHeight: {
    full: '100%',
    screen: '100vh',
  },

  maxWidth: {
    xSmall: '20rem',
    small: '30rem',
    medium: '40rem',
    large: '50rem',
    xLarge: '60rem',
    xxLarge: '70rem',
    xxxLarge: '80rem',
    xxxxLarge: '90rem',
    xxxxxLarge: '100rem',
    full: '100%',
  },

  maxHeight: {
    full: '100%',
    screen: '100vh',
  },

  padding: {
    px: '1px',
    size0: '0',
    size1: '0.25rem',
    size2: '0.5rem',
    size3: '0.75rem',
    size4: '1rem',
    size6: '1.5rem',
    size8: '2rem',
  },

  margin: {
    auto: 'auto',
    px: '1px',
    size0: '0',
    size1: '0.25rem',
    size2: '0.5rem',
    size3: '0.75rem',
    size4: '1rem',
    size6: '1.5rem',
    size8: '2rem',
  },

  negativeMargin: {
    px: '1px',
    size0: '0',
    size1: '0.25rem',
    size2: '0.5rem',
    size3: '0.75rem',
    size4: '1rem',
    size6: '1.5rem',
    size8: '2rem',
  },

  boxShadow: {
    default: '0 2px 4px 0 rgba(0,0,0,0.10)',
    medium: '0 4px 8px 0 rgba(0,0,0,0.12), 0 2px 4px 0 rgba(0,0,0,0.08)',
    large: '0 15px 30px 0 rgba(0,0,0,0.11), 0 5px 15px 0 rgba(0,0,0,0.08)',
    inner: 'inset 0 2px 4px 0 rgba(0,0,0,0.06)',
    none: 'none',
  },

  svgFill: {
    current: 'currentColor',
  },

  svgStroke: {
    current: 'currentColor',
  },
};
