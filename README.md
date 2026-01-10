# MESS-B

**Minimal Explanatory Stack Subtrace with Boundaries**

MESS-B filters Nix evaluator stack traces to their causal essence while preserving semantic boundaries between user code and library internals.

## The Problem

Nix error traces are nightmares. A simple "package imported as module" error produces 55 frames spanning 19KB, of which 2 contain actionable information and 0 appear in user code.

## The Solution

```
$ nix build 2>&1 | mess-b

error[E-INFINITE-RECURSION]: infinite recursion encountered

  --> /nix/store/.../nixos/common.nix:109:20
   | [boundary: module-system → user-code]

  --> .../lib/modules.nix:508:28
   | while evaluating the module argument 'stdenv'

hint: a package derivation may have been imported as a module

  (6/55 frames shown, 89% reduction)
```

## Installation

```bash
# With cabal
cabal install mess-b

# With nix
nix profile install github:weyl-ai/mess-b
```

## Usage

```bash
# Pipe from nix
nix build 2>&1 | mess-b

# From file
mess-b -i error.log

# Adjust frame limit
mess-b -n 5

# Verbose output
mess-b --verbose
```

## Library Usage

```haskell
import MessB

main :: IO ()
main = do
  rawTrace <- getContents
  case parseAndFilter rawTrace of
    Left errorMessage -> putStrLn $ "Parse error: " <> errorMessage
    Right filtered -> putStrLn $ renderCompact filtered
```

## How It Works

1. **Parse** - Extract structured frames from raw Nix output
1. **Classify** - Categorize each frame (UserCode, ModuleSystem, LibraryInternal, etc.)
1. **Detect Boundaries** - Find transitions between code regions (where blame transfers)
1. **Filter** - Retain high-value frames, boundaries, and user code
1. **Hint** - Generate diagnostic hints from known error patterns
1. **Render** - Format output with boundary annotations and statistics

### Classification Strategy

Three-tier fallback:

1. **Explicit role** - Frame has annotation (HighConfidence)
1. **Ontology lookup** - Function name in known-functions table (HighConfidence)
1. **Path heuristics** - File path patterns (MediumConfidence)

Key invariant: User code is **never** misclassified as library internal.

### Boundary Detection

Boundaries mark transitions between code regions:

- `module-system → user-code` - Blame transfers to user
- `library → user-code` - External code enters user space
- `overlay → package-set` - Overlay composition point

### Known Error Patterns

| Pattern | Hint |
|---------|------|
| InfiniteRecursion + `stdenv` in module context | Package imported as module |
| InfiniteRecursion + `fix` + `extends` | Overlay recursion |
| AttributeMissing + `callPackage` | Missing dependency |

## Development

```bash
# Build
cabal build

# Test
cabal test

# Run property tests
cabal test --test-option=--hedgehog-tests=10000
```

## License

MIT

## Credits

Weyl AI - [weyl.ai](https://weyl.ai)
