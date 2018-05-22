# Action Workflow Language

Action Workflow Language builds on top of Action Query Language and provides a
way for incremental query construction by the means of manipulating user
interfaces.

To speak less fancy — Workflow Language allows to define User Interface which
operate on data.

## Overview

The *workflow configuration* is a set of named workflows:

```
ID =
  WORKFLOW

ID2 =
  WORKFLOW2
```

Workflows can also be parametrized by query fragments:

```
ID(query1, query2) =
  ...
```

The execution always starts with a workflow a named `main`:

```
main =
  ...
```

The *workflow* then is defined as:

```
type workflow =
  | Query of query
  | NavigateAnd of query * workflow
  | Name of id
  | Seq of workflow list
  | Par of workflow list
```

Where:

- `Query`

  The workflow which executes the `query` it holds and presents its result to a
  user as a UI screen.

- `NavigateAnd of query * workflow`

  The workflow composition which preprends the `query` to the `workflow`. Which
  means effectively that before executing the `workflow` the system must
  navigate using `query` first.

- `Name of id`

  The workflow which navigates to another workflow named with `id` in the
  workflow configuration (see definition above).

- `Seq of workflow list`

  The workflow which is obtained by the sequental composition of the list of
  workflows. Workflows are executed one by one.

- `Par of workflow list`

  The workflow which is obtained by the parallel composition of the list of
  workflows. The parallel means that user can choose any of the workflows
  presented and proceed with it.

We will use the following syntax for each of the worfklow cases presented above:

- `Q` for `Query`
- `goto Q` for `Name`
- `Q -> W` for `NavigateAnd`
- `W1 ; W2` for `Seq`
- `W1 | W2` for `Par`

The precedence is descending from higher to lower in this list for `->`, `;` and
`|` composition operators. Parens `(` and `)` can be used for grouping to
override precedence.

## Examples

### Pick and View workflow

Allows to select an individual from a list and view details about the record:

```
main = individual:pick ; value:view
```

### Pick and View related record

Allows to select an individual from a list and view details about the related
country record.

Could be done both ways, the simplest is just to modify the query:

```
main = individual:pick ; value.country:view
```

But if you have "view country" workflow factored out as a separate workflow then
you can you use `NavigateAnd` composition:

```
main = individual:pick ; value.country -> viewCountry

viewCountry = :view
```

In this naive examples it doesn't really make much sense but consider the case
when `viewCountry` is a ready to use complex workflow you want to reuse.

## Pick or Make workflow

Allows to pick or make a record and then proceeed to view screen:

```
main = pickOrMake ; value:view

pickOrMake(id) =
  | individual:pick(id: $id)
  | individual:make ; goto pickOrMake(id: value.id)
```
