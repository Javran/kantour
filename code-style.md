# Code Style Rules

## General Principles

- Use `Fourmolu` for formatting (configured in `fourmolu.yaml`)
- Never repeat function names for pattern matching — use `\cases` instead
  - Wrong: `calcDelay _ Nothing _ = Nothing; calcDelay now pd (Just t) = ...`
  - Correct: `calcDelay = \cases { _ _ Nothing -> Nothing; now pd (Just t) -> ... }`
- Use strict `StateT` for state
- Use `BlockArguments` extension (no need for `$` in do-blocks)
- For well-known functions from `base`, don't use explicit import lists
- Never use backtick infix notation (e.g., `` `lookup` `` or `` `M.member` ``). Always use prefix form: `lookup key map`, `M.member key map`, `elem x list`, `notElem x list`
- Don't use unstable aligns on let bindings unless it's a single line
  - Use this pattern:
    ```
    let
      xs1 = ...
      xs2 = ...
    ```
  - NOT this (unstable align):
    ```
    let xs1 = ...
        xs2 = ...
    ```
- Never make an import statement less visible (e.g., adding explicit import list) unless asked to address lint/warnings
- Use `(\_ -> a)` instead of `const a` for clarity
- **Never drop comments** — preserve TODO comments, explanatory comments, and any other comments in edited regions
- Keep constraints on method default implementations, not on the class itself
- Use `ExceptT` for error handling to avoid nested case statements (pyramid of doom)
- Prefer readable code: if original code uses expressions like `60 * 30`, keep them (don't replace with computed values like `1800`)

## Floating Point / Scientific

- Use explicit type application for floating point operations
- Prefer `floor @_ @Int m`, `round @_ @Int x`, `ceiling @_ @Int y` over `::` syntax
- Use `toRealFloat @Double` for converting Scientific to Double
- Use `24 * 60` instead of hardcoded values like `1440`

## Record Access

- Use `OverloadedRecordDot` extension
- Use `NoFieldSelectors` with `DuplicateRecordFields`
- **Avoid spaces around dots**: `.foo` NOT `. foo` (critical with `OverloadedRecordDot`)

## Imports

- Use leading comma style for imports
- Use leading export style
- Don't use explicit import lists for well-known functions from `base`
- Use `NoImportQualifiedPost` (qualified imports go before module name)
- **Never import any module more than once** — if you need both the module's types and functions, use qualified imports
  - Wrong: `import Data.Map (Map)` then use `Map k v`
  - Wrong: `import Data.Map; import qualified Data.Map as M`
  - Correct: `import qualified Data.Map as M` then use `M.Map k v`
  - Same applies to `Data.Text`, `Data.Aeson`, `Data.OrdPSQ`, `Data.Sequence`, etc.

## Naming Conventions

- Use camelCase for function/variable names
- Use PascalCase for type names
- Use CamelCase for type variables when explicit

## Error Handling

- Use exceptions from `safe-exceptions` package (`tryAny`, `catchAny`)
- Use `MonadThrow`/`MonadCatch` from `mtl`
- Avoid partial functions; use pattern matching exhaustively
- Use `ExceptT` for error handling in scheduler tasks

## Fourmolu Configuration

Key settings (see `fourmolu.yaml` for full config):
- Indentation: 2 spaces
- Comma style: leading
- `respectful: true` preserves user formatting

## HLint Configuration

Some rules are ignored (see `.hlint.yaml` for full list). Common ones: avoid lambda, eta reduce, reduce duplication, use head/if/infix/newtype.

## Editor / LSP Notes

- Uses `haskell-language-server` via Stack (`hie.yaml`)
- Fourmolu runs on save — be careful with `OverloadedRecordDot` and dots
- If LSP auto-formats and breaks record dot syntax, disable format-on-save for this project
