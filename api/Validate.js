/**
 * @flow
 */

import invariant from 'invariant';
import * as yaml from 'yaml-js';
import {ScalarNode, MappingNode, SequenceNode, Node} from 'yaml-js/lib/nodes';

export {ScalarNode, MappingNode, SequenceNode, Node};

export type Position<+Ctx: {}> = {
  +context: Ctx,
  +description: string,
  +prev: ?Position<Ctx>,
  when(message: string): Position<Ctx>,
  error(message: string): void,
};

export function createPos<Ctx: {}>(
  description?: string = '<root>',
  context: Ctx,
): Position<Ctx> {
  const pos: Position<Ctx> = {
    context,
    description,

    prev: null,
    when(description): Position<Ctx> {
      return {...pos, description, prev: pos};
    },
    error(reason: string) {
      throw new Error(reason);
    },
  };
  return pos;
}

export type Parser<V, Ctx = {}> = {
  parse(Position<Ctx>, Node): V,
  refine<NV>((Position<Ctx>, V, Node) => NV): Parser<NV, Ctx>,
};

export class ParseError extends Error {
  reason: string;
  pos: Position<*>;
  constructor(reason: string, pos: Position<*>) {
    const context = [];
    let p = pos;
    while (p != null) {
      context.push(`  ${p.description}`);
      p = p.prev;
    }
    const message = `${reason}\n${context.join('\n')}`;
    super(message);
    this.reason = reason;
    this.pos = pos;
  }
}

export function createParser<V, C>({
  parse,
}: {
  parse: (Position<C>, Node) => V,
}): Parser<V, C> {
  const parser = {
    parse,
    refine<NV>(f: (Position<C>, V, Node) => NV): Parser<NV, C> {
      return createParser({
        parse(pos, node) {
          const v = parser.parse(pos, node);
          return f(pos, v, node);
        },
      });
    },
  };
  return parser;
}

export function any<Ctx: {}>(): Parser<mixed, Ctx> {
  function parse(node) {
    if (node instanceof ScalarNode) {
      return node.value;
    } else if (node instanceof MappingNode) {
      const result = {};
      for (const [key, value] of node.value) {
        result[String(parse(key))] = parse(value);
      }
      return result;
    } else if (node instanceof SequenceNode) {
      const result = [];
      for (const value of node.value) {
        result.push(parse(value));
      }
      return result;
    } else {
      invariant(false, 'malformed YAML tree');
    }
  }
  return createParser({
    parse(pos, node) {
      return parse(node);
    },
  });
}

export function string<Ctx: {}>(): Parser<string, Ctx> {
  return createParser({
    parse(pos, node) {
      if (node instanceof ScalarNode && node.tag === 'tag:yaml.org,2002:str') {
        return node.value;
      }
      throw new ParseError('expected string', pos);
    },
  });
}

export function constant<Ctx: {}>(value: string): Parser<string, Ctx> {
  return createParser({
    parse(pos, node) {
      if (node instanceof ScalarNode && node.tag === 'tag:yaml.org,2002:str') {
        if (node.value !== value) {
          throw new ParseError(`expected "${value}"`, pos);
        }
        return node.value;
      }
      throw new ParseError('expected string', pos);
    },
  });
}

type ObjParserToValue<Ctx: {}, I> = $ObjMap<I, <V>(Parser<V, Ctx>) => V>;

export function record<Ctx: {}, F: *, V: ObjParserToValue<Ctx, F>>(
  fields: F,
  options?: {ignoreUnknownFields: boolean} = {ignoreUnknownFields: false},
): Parser<V, Ctx> {
  return createParser({
    parse(pos: Position<Ctx>, node) {
      if (node instanceof MappingNode) {
        const result = {};
        for (const [keyNode, valueNode] of node.value) {
          const key = string().parse(pos, keyNode);
          const valueParser = fields[key];
          if (valueParser == null) {
            if (options.ignoreUnknownFields) {
              continue;
            } else {
              throw new ParseError(`unknown field ${key}`, pos);
            }
          }
          const valuePos = pos.when(`parsing key ${key}`);
          result[key] = valueParser.parse(valuePos, valueNode);
        }
        for (const key in fields) {
          if (!(key in result)) {
            throw new ParseError(`missing field ${key}`, pos);
          }
        }
        // $FlowFixMe: ...
        return ((result: any): V);
      }
      throw new ParseError('expected mapping', pos);
    },
  });
}

export function sequence<Ctx: {}, V>(childParser: Parser<V, Ctx>): Parser<Array<V>, Ctx> {
  return createParser({
    parse(pos, node) {
      if (node instanceof SequenceNode) {
        const result = [];
        for (let idx = 0; idx < node.value.length; idx++) {
          const childNode = node.value[idx];
          const valuePos = pos.when(`parsing index ${idx}`);
          result.push(childParser.parse(valuePos, childNode));
        }
        return result;
      }
      throw new ParseError('expected sequence', pos);
    },
  });
}

export function mappingGeneral<Ctx: {}, K, V, R>({
  keyParser,
  valueParser,
  initial,
  build,
}: {
  keyParser: Parser<K, Ctx>,
  valueParser: Parser<V, Ctx>,
  initial: R,
  build: (R, K, V) => R,
}): Parser<R, Ctx> {
  return createParser({
    parse(pos: Position<Ctx>, node) {
      if (node instanceof MappingNode) {
        let value = initial;
        for (const [keyNode, valueNode] of node.value) {
          const k = keyParser.parse(pos, keyNode);
          const v = valueParser.parse(pos, valueNode);
          value = build(value, k, v);
        }
        return value;
      }
      throw new ParseError('expected mapping', pos);
    },
  });
}

export function mapping<Ctx: {}, V>(
  childParser: Parser<V, Ctx>,
  keyParser?: Parser<string, Ctx> = string(),
): Parser<{[key: string]: V}, Ctx> {
  return createParser({
    parse(pos: Position<Ctx>, node) {
      if (node instanceof MappingNode) {
        const result = {};
        for (const [keyNode, valueNode] of node.value) {
          const key = keyParser.parse(pos, keyNode);
          const valuePos = pos.when(`parsing key ${key}`);
          const value = childParser.parse(valuePos, valueNode);
          result[key] = value;
        }
        return result;
      }
      throw new ParseError('expected mapping', pos);
    },
  });
}

export function optional<Ctx: {}, V>(childParser: Parser<V, Ctx>): Parser<?V, Ctx> {
  return createParser({
    parse(pos, node) {
      if (node instanceof ScalarNode && node.tag === 'tag:yaml.org,2002:null') {
        return null;
      }
      return childParser.parse(pos, node);
    },
  });
}

type Match<Ctx: {}> = {
  match(pos: Position<Ctx>, node: Node): boolean,
};

type Case<Ctx: {}, V> = [Match<Ctx> | true, Parser<V, Ctx>];

type SwitchCase2<Ctx: {}, A, B> = (Case<Ctx, A>, Case<Ctx, B>) => Parser<A | B, Ctx>;
type SwitchCase3<Ctx: {}, A, B, C> = (
  Case<Ctx, A>,
  Case<Ctx, B>,
  Case<Ctx, C>,
) => Parser<A | B | C, Ctx>;
type SwitchCase4<Ctx: {}, A, B, C, D> = (
  Case<A>,
  Case<B>,
  Case<C>,
  Case<D>,
) => Parser<A | B | C | D, Ctx>;

function switchCaseImpl(...cases): Parser<*, *> {
  return createParser({
    parse(pos, node: Node) {
      for (const [match, parser] of cases) {
        if (match === true) {
          return parser.parse(pos, node);
        } else if (match.match(pos, node)) {
          return parser.parse(pos, node);
        }
      }
      throw new ParseError('no cases matched', pos);
    },
  });
}

export const switchCase: SwitchCase2<*, *> &
  SwitchCase3<*, *, *> &
  // $FlowFixMe: ...
  SwitchCase4<*, *, *, *> = (switchCaseImpl: any);

export function recursive<Ctx: {}, V>(
  f: (Parser<V, Ctx>) => Parser<V, Ctx>,
): Parser<V, Ctx> {
  let memoizedParser = null;
  const parser = createParser({
    parse(pos, node) {
      if (memoizedParser == null) {
        memoizedParser = f(parser);
      }
      return memoizedParser.parse(pos, node);
    },
  });
  return parser;
}

type Lazy<V, Ctx: {}> = {
  node: Node,
  parseWith(ctx: Ctx, parser: Parser<V, Ctx>): V,
};

const sentinel = {};

export function lazy<V, Ctx: {}, NCtx: {}>(): Parser<Lazy<V, NCtx>, Ctx> {
  return createParser({
    parse(pos, node) {
      let result = sentinel;
      const parseWith = (ctx, parser) => {
        const nextPos = createPos(pos.description, ctx);
        if (result === sentinel) {
          result = parser.parse(nextPos, node);
        }
        // $FlowFixMe: ...
        return (result: any);
      };
      return {node, parseWith};
    },
  });
}

export function getMappingNodeValueNodeByKey(key: string, node: Node) {
  if (node instanceof MappingNode) {
    for (const [keyNode, valueNode] of node.value) {
      if (
        keyNode instanceof ScalarNode &&
        keyNode.tag === 'tag:yaml.org,2002:str' &&
        keyNode.value === key
      ) {
        return valueNode;
      }
    }
    return null;
  }
  return null;
}

export function hasFieldOfType<Ctx: {}>(
  fieldName: string,
  fieldParser: Parser<*, Ctx>,
): Match<Ctx> {
  return {
    match(pos, node) {
      if (node instanceof MappingNode) {
        const valueNode = getMappingNodeValueNodeByKey(fieldName, node);
        if (valueNode == null) {
          return false;
        }
        if (!isValid(pos, fieldParser, valueNode)) {
          return false;
        }
        return true;
      }
      return false;
    },
  };
}

export const onSequence: Match<*> = {
  match(_pos, node: Node): boolean {
    return node instanceof SequenceNode;
  },
};

export const onMapping: Match<*> = {
  match(_pos, node: Node): boolean {
    return node instanceof MappingNode;
  },
};

export const onScalar: Match<*> = {
  match(_pos, node: Node): boolean {
    return node instanceof ScalarNode;
  },
};

export function parseWith<Ctx: {}, V>(ctx: Ctx, parser: Parser<V, Ctx>, node: Node): V {
  const pos = createPos('<root>', ctx);
  return parser.parse(pos, node);
}

export function parseStringWith<Ctx: {}, V>(
  ctx: Ctx,
  parser: Parser<V, Ctx>,
  value: string,
): V {
  const node = yaml.compose(value);
  return parseWith(ctx, parser, node);
}

function isValid<Ctx>(pos: Position<Ctx>, parser: Parser<*, Ctx>, node: Node): boolean {
  try {
    parser.parse(pos, node);
    return true;
  } catch (err) {
    if (err instanceof ParseError) {
      return false;
    } else {
      throw err;
    }
  }
}
