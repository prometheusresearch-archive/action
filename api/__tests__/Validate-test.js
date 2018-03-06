/**
 * @flow
 */

import * as v from '../Validate.js';

test('scalar', function() {
  const p = v.string();
  const parse = s => v.parseStringWith({}, p, s);
  expect(parse('hello')).toEqual('hello');
  expect(parse('"hello"')).toEqual('hello');
  expect(parse("'hello'")).toEqual('hello');
  expect(() => parse('1')).toThrowError(v.ParseError);
});

test('mapping', function() {
  const p = v.mapping(v.string());
  const parse = s => v.parseStringWith({}, p, s);
  expect(parse('{}')).toEqual({});
  expect(parse('{a: b}')).toEqual({a: 'b'});
  expect(() => parse('hello')).toThrowError(v.ParseError);
  expect(() => parse('{a: 42}')).toThrowError(v.ParseError);
});

test('record', function() {
  const p = v.record({
    a: v.string(),
  });
  const parse = s => v.parseStringWith({}, p, s);
  expect(() => parse('{}')).toThrowError(v.ParseError);
  expect(parse('{a: b}')).toEqual({a: 'b'});
  expect(() => parse('{a: 21}')).toThrowError(v.ParseError);
  expect(() => parse('{a: b, b: c}')).toThrowError(v.ParseError);
  expect(() => parse('hello')).toThrowError(v.ParseError);
});

test('sequence', function() {
  const p = v.sequence(v.string());
  const parse = s => v.parseStringWith({}, p, s);
  expect(parse('[]')).toEqual([]);
  expect(parse('[hello]')).toEqual(['hello']);
  expect(() => parse('{}')).toThrowError(v.ParseError);
  expect(() => parse('[42]')).toThrowError(v.ParseError);
});

test('optional', function() {
  const p = v.optional(v.string());
  const parse = s => v.parseStringWith({}, p, s);
  expect(parse('hello')).toEqual('hello');
  expect(parse('null')).toEqual(null);
  expect(() => parse('42')).toThrowError(v.ParseError);
});

test('recursive', function() {
  // basic linked list vlaidator
  const p = v.recursive(p =>
    v.record({
      value: v.string(),
      next: v.optional(p),
    }),
  );
  const parse = s => v.parseStringWith({}, p, s);
  expect(parse('{value: hello, next: null}')).toEqual({value: 'hello', next: null});
  expect(parse('{value: hello, next: {value: oops, next: null}}')).toEqual({
    value: 'hello',
    next: {value: 'oops', next: null},
  });
  expect(() => parse('{value: hello, next: 42}')).toThrowError(v.ParseError);
  expect(() => parse('{value: 42, next: null}')).toThrowError(v.ParseError);
});
