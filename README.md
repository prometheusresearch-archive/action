# Action

Composable workflows for relational data.

## Repository structure

- `api`

  Data services, GraphQL endpoing, CRUD.

- `ui`

  User Interface which renders workflows with the help of `workflow` execution
  engine and `api` data services.

- `workflow`

  Workflow execution engine decoupled from the UI.

## Development

```
% make bootstrap
% make -C workflow build
% make ui/serve-dev
% make api/serve-dev
```
