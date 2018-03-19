# RexAction2

## 1.2.2018

### Motivation

- Expressiveness
- Robustness
- UX
- DX

### Implementation notes

- JS application
- Interfacing directly with DB

### Config language

```
start:

- action: pick-individual
  choice:
  - view-individual
  - edit-individual
  - enroll-individual

- action: make-individual
  choice:
  - when: individual[!enrolled]
    then: enroll-individual
  - otherwise: enroll-individual

entity:

  individual:
   entity: individual
    fields:
    - *
    - name: enrolled
      query: ...

  enrolled_individual:
    entity: individual
    where: enrolled


actions:

  pick-individual:
    type: pick
    entity: individual

  make-individual:
    type: make
    entity: individual

  enroll-individual:
    type: edit
    entity: individual[!enrolled]
```

## 2.2.2018

### Entity configuration langauge

New entities could be configured based on existing ones via YAML config. We call
this process a refinement.

The basic syntax for refinements is

```
entity:
  male:
    entity: individual
    ...
```

This defined a new entity called `male` which is based on pre-existing entity
called `individual` (the ellipsis `...` means "other config keys", to be
described below).

There are two ways to refine an entity:

1. Add new fields
2. Add masks

The invariant is that a refinement entity is a subtype of a base entity — a
`male` can be used anywhere `individual` is expected.

- TODO: Should we have a way to define new entities which has a subset of fields of a
  base entity? This is not a refinement with subtyping invariant.

- TODO: Should we have a way to define new entities which has nominal tyope structure
  — e.g. subtyping doesn't hold and those entities are completely different than
  base ones.

#### Refinement by adding fields

Example:

```
entity:
  enrolledIndividual:
    entity: individual
    fields:
      hasEnrollments: exists(study_enrollment)
```

This defined an entity called `enrolledIndividual` which has an additional
field called `hasEnrollments`.

#### Refinement by adding a mask

Example:

```
entity:
  female:
    entity: individual
    where: sex = 'female'
```

This defines a new entity called `female` which restricts `individual` by only
allowing those for which the `sex = 'female'` predicate evaluates is true.

#### Context dependent refinements

It is possible to define a refinement which depends on current context.

For example we can say that individual is enrolled in some concrete study:

```
entity:
  enrolledIndividual:
    entity: individual
    require:
    - study: study
    where: exists(study_enrollment.study = $study)
```

Another example would be to add a fields which depends on some value being in
context:

```
entity:
  enrolledIndividual:
    entity: individual
    require:
    - study: study
    where: exists(study_enrollment.study = $study)
    fields:
      enrollmentDate: top(study_enrollment.filter(study=$study).date)
```

### Workflow definition language

Workflow is defined as a grammar which specifies productions for UI, the
start of the workflow is defined with `start` non terminal symbol, the terminal
symbols are concrete actions.

Such grammar is restricted by the context type and requirements specified by
actions.

```
workflow:

  start:
  - pick-or-make-individual:
    - view-individual
    - edit-individual

  pick-or-make-individual:
  - pick-individual
  - make-individual

  make-and-enroll-individual:
  - make-individual:
    - enroll-individual

  pick-individual:
    type: pick
    entity: individual

  make-individual:
    type: make
    entity: individual

  enroll-individual:
    type: process
    require:
    - individual: individual
    - study: study
    execute:
      /insert(study_enrollment := {
        individual := $individual,
        study := $study,
      })
```

- TODO: Describe execution semantics and type based dispatch.
- TODO: Describe execution semantics and type based dispatch.

#### Custom dispatch

Sometimes is is beneficial to do a custom dispatch without introducing more
entity types. For that reason we have a match construct:

```
start:
- make-individual:
  match:
  - case: individual.age > 21
    then:
    - enroll-individual
  - otherwise:
    then:
    - view-individual
```

- TODO: custom dispatch which introduces bindings into the context

### Composing workflows

### Experiment: concrete syntax with explicit context bindings

```
start() = {
  individual = pick-or-make-individual();
  view-individual(individual) | edit-individual(individual)
}

pick-or-make-individual() = {
  pick-individual() | make-individual()
}

make-and-enroll-individual() = {
  make-individual();
  enroll-individual();
}

pick-individual() = pick { entity: individual }

make-individual() = make { entity: individual }

view-individual(individual) = view(individual)

edit-individual(individual) = edit(individual)

enroll-individual(individual: individual, study: study) = process {
  execute:
    /insert(study_enrollment := {
      individual := $individual,
      study := $study,
    })
}
```

## 5.2.2018

### Data layer

- [ ] Reflect SQL structure into data model
  - [ ] Relations
    - [x] o2m
    - [x] m2o
    - [ ] m2m ???
    - [ ] o2o ???
  - [ ] Scalars
    - [ ] Basic scalars
      - [x] Int
      - [x] Text
      - [x] Bool
      - [ ] Date
      - [ ] Time
      - [ ] DateTime
      - [ ] JSON
      - [ ] ...
- [ ] Map data model to GraphQL types
  - [ ] `entity(id)`
  - [ ] `entity__list(limit, offset)`
  - [ ] `entity { m2o }`
  - [ ] `entity { o2m }`

## 6.2.2018

Working on syntax embedded in YAML.

```
workflow:
  start:
    action
```

```
workflow:
  start:
  - action
  - another-action
```

```
workflow:
  start:
    action:
      another-action
```

```
workflow:
  start:
    action:
      another-action
```

## 6.3.2018

UI as a Query

```

let pickIndividual = individual.pick
let viewIndividual = individual.view

pickIndividual {
  navigate(id: "42") {
    viewIndividual { ui, data }
  }
}

```

## 19.3.2018

Rabbit is extended with UI types and values. Thus queries represent recipes on
how to get concrete datasets or how to render concrete UI for concrete datasets.

Examples of such queries:

* Render a list of all individuals:

  ```
  individual.pick
  ```

* Render a list of all a subset of individuals:

  ```
  individual.filter(sex = 'male').pick
  ```

* Render an individual after a list:

  ```
  individual.pick.value.view
  ```

  Note how you can navigate through `pick` query combinator.

Now that we have representation for screens we need a language to compose such
screens into workflows. I propose to define a `Workflow` monad on top of
queries.

Examples of workflows:

* Render a list of individuals with a view afterwards:

  ```
  render(individual.pick) {
    render(view)
  }
  ```

* Render a list of individuals with a view and a related site view afterwards:

  ```
  render(individual.pick) {
    render(view)
    render(site.view)
  }
  ```

* Render a list of individuals with a view based on the selected individual:

  ```
  render(individual.pick) {
    render(switch(
      sex = 'male', view(title: "View Male"),
      sex = 'female', view(title: "View Female")
    ))
  }
  ```
