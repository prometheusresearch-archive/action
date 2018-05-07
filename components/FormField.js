/**
 * @flow
 */

import * as React from 'react';
import {View, Text, TouchableOpacity, StyleSheet, processColor} from 'react-native-web';
import * as cfg from './config.js';

let style = StyleSheet.create({
  base: {},
  label: {
    paddingBottom: cfg.padding.size1,
  },
  labelText: {
    fontWeight: cfg.fontWeight.medium,
    fontSize: cfg.fontSize.small,
  },
  hintText: {
    fontWeight: cfg.fontWeight.medium,
    fontSize: cfg.fontSize.small,
  },
  labelErrorText: {
    color: cfg.color.red,
  },
});

type P = {
  error?: boolean,
  label?: React.Node,
  hint?: React.Node,
  renderInput: ({ref: React.Ref<*>, outlineColor: string}) => React.Node,
  outlineColor: string,
};

export class FormField extends React.Component<P> {
  static defaultProps = {
    outlineColor: cfg.color.black,
  };

  _inputRef: React.Ref<*> = React.createRef();

  onLabelPress = () => {
    if (this._inputRef.current != null) {
      // $FlowFixMe: ...
      this._inputRef.current.focus();
    }
  };

  render() {
    let labelTextColorStyle = {
      color: this.props.outlineColor,
    };
    let hintTextColorStyle = {
      color: processColor(cfg.color.black, 0.6),
    };
    return (
      <View>
        {this.props.label != null && (
          <TouchableOpacity
            activeOpacity={1.0}
            accessible={false}
            onPress={this.onLabelPress}
            style={style.label}>
            {this.props.label != null && (
              <View>
                <Text
                  style={[
                    style.labelText,
                    labelTextColorStyle,
                    Boolean(this.props.error) && style.labelErrorText,
                  ]}>
                  {this.props.label}
                </Text>
              </View>
            )}
            {this.props.hint != null && (
              <View>
                <Text
                  style={[
                    style.hintText,
                    hintTextColorStyle,
                    Boolean(this.props.error) && style.labelErrorText,
                  ]}>
                  {this.props.hint}
                </Text>
              </View>
            )}
          </TouchableOpacity>
        )}
        <View>
          {this.props.renderInput({
            error: this.props.error,
            ref: this._inputRef,
            outlineColor: this.props.outlineColor,
          })}
        </View>
      </View>
    );
  }
}
