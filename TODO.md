# TODO

- [ ] Workflow Language redesign

  - [x] AST and related utilities
  - [ ] Typing
  - [ ] Interpreter & bindings to UI
  - [ ] Add parametrized workflows

- [ ] Redesign UI components
  - [ ] Pick

    Idea: use react-virtualized

  - [ ] View
  - [ ] Form

- [ ] Query Combinators backends

  - [ ] SQL backend

    Idea: translate to RA/RC (relatinal algebra/calculus) and then generate SQL

  - [ ] JSON backend

    JSON backend exists but it's not optimized

    - [x] Initial implementation

    - [ ] Optimizations

      Idea: look at NRA/NRC (nested relatinal algebra/calculus)

  - [ ] Compile query both to SQL and JSON

      Idea: we need a way to compile QC down to GraphQL level language (no
      arbitrary queyr constructs, all filters and such are hidden behind API).
