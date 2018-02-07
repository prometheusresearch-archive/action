/**
 * Universe is a collection of entities present in an application.
 *
 * @flow
 */

import * as t from './types.js';

export function create(): t.Universe {
  return {
    entity: {},
  };
}

export function fresh(univ: t.Universe): t.Universe {
  return {
    ...univ,
    entity: {...univ.entity},
  };
}

export function defineEntity(univ: t.Universe, entity: t.EntityType) {
  univ.entity[entity.name] = entity;
}
