# Agent instructions

## Build & test

```sh
stack build
stack test                              # all tests
stack test kantour-core --test-arguments='--match=Completeness'  # single test group
```

Single test group pattern matches against `describe`/`context` strings. Tests are auto-discovered via `hspec-discover` (any `*Spec.hs` under `kantour-core/test/`).

System dep: `liblzma-dev` (needed by `lzma` package for xz compression in master data tests).

## Project structure

Two packages under `stack.yaml`:
- `kantour-core` — library, all modules under `Kantour.Core`
- `kantour-cli` — library + executable `kantour` (entrypoint: `kantour-cli/mains/Main.hs` → `Kantour.Main` → `Kantour.Subcommand`)

## Master data pipeline

Master data (`api_start2.json.xz`) is parsed in stages:
- **`Direct/`** types — raw JSON decoding with `parseKcMstJson` (generic aeson via field-renaming). Each type has `HasKnownFields` listing all `api_*` fields it expects.
- **`CompletenessSpec.hs`** uses `CollectExtra` to detect unhandled JSON fields — unknown fields make the test pend.
- **`Org/`** types — higher-level abstraction over Direct types, no test coverage yet.
- Test fixture: `kantour-core/data/api_start2.json.xz`.

Code style rules in `code-style.md`. Run `fourmolu --mode=inplace` on changed files.

HLint configured to ignore: Avoid lambda, Eta reduce, Use const, Use infix, Reduce duplication, Use if, Use newtype, Use head, Redundant pure, Use uncurry, Use camelCase, Use list literal pattern.

Default GHC extensions in `package.yaml` — no need to enable per-file.
