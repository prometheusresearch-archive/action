# TODO

- [x] Expose metadata via query API

- [ ] In-memory JSONDatabase redesign
  - [x] Use multiple normalized stores per entity at the root and model
        relations as links
  - [ ] Back & circular references

- [ ] Create/Edit support for databases

- [ ] Add more combinators
  - [ ] Chain/Bind
  - [ ] Filter
  - [ ] String interpolation
  - [ ] Boolean ops
  - [ ] Numeric ops
  - [ ] String concat
- [ ] Add conditionals to workflow language
- [ ] Choice eliminators
- [ ] Add locations to lexer/parser
- [ ] Improve error reporting to provide more context (location, contextual
      info)
- [ ] Add pretty-printer
- [ ] Allow editing workflow "live"
- [ ] Compilation into SQL
- [ ] Better query/workflow editing experience
