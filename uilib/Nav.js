/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity} from 'react-native-web';
import {OutlineButton} from './OutlineButton.js';
import {type BreadcrumbItem, Breadcrumb} from './Breadcrumb.js';
import * as cfg from './config.js';

const borderWidth = cfg.borderWidth.size2;

function NavTitle({title, textColor}) {
  return (
    <View>
      <TouchableOpacity>
        <Text
          style={{
            color: textColor,
            fontSize: cfg.fontSize.xLarge,
            fontWeight: cfg.fontWeight.extrabold,
          }}>
          {title}
        </Text>
      </TouchableOpacity>
    </View>
  );
}

function NavElement({children}) {
  return (
    <View
      style={{
        justifyContent: 'center',
        paddingHorizontal: cfg.padding.size2,
      }}>
      {children}
    </View>
  );
}

type NavButtonProps = {
  title: string,
  textColor: string,
};

export function NavButton({title, textColor}: NavButtonProps) {
  return (
    <View
      style={{
        flexDirection: 'row',
        alignItems: 'center',
      }}>
      <TouchableOpacity>
        <Text style={{color: textColor, fontWeight: cfg.fontWeight.bold}}>{title}</Text>
      </TouchableOpacity>
    </View>
  );
}

type P = {
  outlineColor: string,
  breadcrumb: Array<BreadcrumbItem>,
  renderNav?: (props: {outlineColor: string}) => Array<React.Node>,
  renderNavExtra?: (props: {outlineColor: string}) => Array<React.Node>,
};

export function Nav({outlineColor, renderNav, renderNavExtra, breadcrumb}: P) {
  const borderColor = outlineColor;
  const textColor = outlineColor;
  const nav = renderNav != null ? renderNav({outlineColor}) : [];
  const navExtra = renderNavExtra != null ? renderNavExtra({outlineColor}) : [];
  return (
    <View style={{padding: cfg.padding.size4}}>
      <View
        style={{
          borderWidth,
          borderColor,
          borderRadius: cfg.borderRadius.small,
        }}>
        <View
          style={{
            padding: cfg.padding.size4,
            flexDirection: 'row',
            alignItems: 'center',
          }}>
          <NavTitle title="Action" textColor={textColor} />
          <View
            style={{flex: 1, flexDirection: 'row', paddingHorizontal: cfg.padding.size8}}>
            {nav.map(element => <NavElement>{element}</NavElement>)}
          </View>
          <View style={{flexDirection: 'row'}}>
            {navExtra.map(element => <NavElement>{element}</NavElement>)}
          </View>
        </View>
        {breadcrumb &&
          breadcrumb.length > 0 && (
            <View
              style={{
                borderTopWidth: cfg.borderWidth.default,
                borderTopColor: borderColor,
              }}>
              <Breadcrumb items={breadcrumb} textColor={textColor} />
            </View>
          )}
      </View>
    </View>
  );
}

Nav.defaultProps = {outlineColor: cfg.color.black};
