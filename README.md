# Action

Langauge for composable data-driven workflows.

Action is an experimental language based on [Query Combinators][] paper which
defines a notion of UI values and workflows over them.

[Query Combinators]: https://arxiv.org/abs/1702.08409

## Development

Install esy@0.1.8:

```
npm install --global esy@0.1.8
```

Clone the repository:

```
git clone https://github.com/prometheusresearch/action
```

Run bootstrap:

```
make bootstrap
```

If you work only on UI portion of action:

```
make serve
```

If you work only on core portion of action:

```
cd workflow
make watch
esy vim ./src/
```

If you work on both you need both `make serve` and `make watch-workflow`
running.
