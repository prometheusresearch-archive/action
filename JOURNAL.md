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

  Now we can get a value of the selected item:

  ```
  individual.pick.value
  ```

  In this case the value is always `null` due to the fact that no selection has
  been made yet.

  To render a list of all individuals with a selected individual:

  ```
  individual.pick(id: "id")
  ```

  And not to get the selected value:

  ```
  individual.pick(id: "id").value
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

## 20.04.2018

### Thoughts on workflow language

Currently what we have is a monadic language on top of query langauge. This
isn't enough as we are unable to represent recursive workflows.

The  proposal is to model workflow as a grammar

#### Primitives

There are primitives:

- Screen

  Screens are specified with `render(q)` syntax where `q` is a query which
  results in a UI (result of `:pick`, `:view` and so on).

#### Combinators

There are combinators:

- Sequence

  Given workflows `A` and `B` then `A ; B` is a workflow which executes `B`
  after `A`.

- Alternation

  Given workflows `A` and `B` then `A | B` is a workflow which executes either
  `A` or `B`.

- Navigation

  Given workflows `A` and a query `q` then `q -> A` is a workflow which follows
  query q and then executes `A`.

  Another form is `A -> q` in which query `q` executes against an output of
  workflow `A`.

  In the first form if `q` evaluates to `null` then workflow is not executed.
  This allows to use navigation as a branching mechanism.

#### Storing and reusing values

And a *store* construct which allows to store the results of workflow executions
and reuse them later:

```
let individual = render(indivdual:pick) -> value
```

Note that only certain values can be stored. We intent to allow to store only
valyes of the `Entity` type (more types of values can be enabled later if
needed, like for passing sets as queries).

#### Examples

A complete "real-world" example:

```
main =                        # An entry point, which
  | regionWorkflow            # ... executes either regionWorkflow or
  | nationWorkflow            # ... nationWorkflow

nationWorkflow =              # This is the nationWorkflow definition, which
  render nation:pick;         # ... renders nation:pick screen
  render value:view           # ... then renders value:view screen

regionWorkflow =              # This is the regionWorkflow definition, which
  render region:pick;         # ... renders the region:pick screen
  | render value:view         # ... then either renders the value:view
  | value -> nationWorkflow   # ... or navigates to value and then starts the nationWorkflow
```

 An example of a recursive workflow which allows to traverse the recursive
 structure of indivduals with links amongh other individuals.

```
 main =
  individual -> individualWorkflow

individualWorkflow =
  render :pick;
  | render value:view
  | value.parents -> individualWorkflow
  | value.children -> individualWorkflow
```

An example of using Navigation as a branching mechanism:

```
main =
  todoWorkflow

todoWorkflow =
  pickOrCreateTodo;
  processTodo

pickOrCreateTodo =
  | pickTodo
  | createTodo

pickTodo =
  render(todo:pick) -> value

createTodo =
  render todo:form(spec: :create {name: $value.name, status: "new"})

processTodo =
  | render :view
  | :filter(status != "completed") -> completeTodo

completeTodo =
  render :form(spec: :update {status: "completed"})
```

An example of preserving values from the previous steps:

```
main = enrollmentWorkflow

enrollmentWorkflow =
  let study = render /study:pick;            # Pick the study from the list
  let individual = render /individual:pick;  # Pick the individual from the list

  render /enrollment:form(         # Render an enrollment form which
    title: "Enroll individual",    # ... uses the values from the previous
    spec: :create {                # ... steps of the workflow.
      study: $study,
      individual: $individual,
      date: now(),
    }
  )
```

Now consider the case we want to factor out the complex enrollment screen into a
separate workflow. We clearly need to allow to parametrize the workflows:

```
main = enrollmentWorkflow

enrollmentWorkflow =
  let study =                        # Pick the study from the list
    render /study:pick -> value;     # ... and store the result in `study` var

  let individual =                   # Pick the individual from the list
    render /individual:pick;         # ... and store the result in `individual` var

  enrollIndividual(
    study: $study,
    individual: $individual
  )

enrollIndividual(study, individual) =
  render /enrollment:form(         # Render an enrollment form which
    title: "Enroll individual",    # ... uses the `study` and `individual`
    spec: :create {                # ... values from the previous steps.
      study: $study,
      individual: $individual,
      date: now(),
    }
  )
```

#### Modules

Each source file which contains workflow language expressions is treated as a
module unit.

We can mark certain workflows in a module as exported:

```
# A.workflow source file

export pickIndividual =
  render individual:pick
```

Which allows those exported names to be referred by other workflow modules:

```
# B.workflow source file

main =
  A.pickIndividual
```

### Todo workflow as found in Rex Study

```
main =
  | viewTodo -> value; processTodo
  | viewTodoSkipped -> value

processTodo =
  | fulfillMeasureEntryStart; enterMeasure
  | enterMeasure
  | reconcileMeasure; main
  | useExistingMeasure; measureWizard
  | fulfillSample
  | useExistingSample; sampleWizard
  | fulfillConsent
  | useExistingConsent; consentWizard
  | measureWizard
  | sampleWizard
  | consentWizard
  | completeTodo
  | editDataEntryProperties; main
  | editEntryMeasureAdhocTodo; main
  | editPortalSmsMeasureAdhocTodo; main
  | editPortalMeasureAdhocTodo; main
  | editSampleAdhocTodo; main
  | editConsentAdhocTodo; main
  | editOtherAdhocTodo; main
  | skipTodo; viewTodoSkipped
  | viewTodoDelete; dropTodo

sampleWizard = ...
consentWizard = ...
measureWizard = ...

viewTodo = render :view

enterMeasure = ...

viewTodoSkipped =
  render :view;
  render :edit(spec: :update {status: "new"});
  main
```
