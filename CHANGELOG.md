# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0.0] - 2026-01-09

### Added

- Initial release
- Stack trace parsing from Nix evaluator output
- Frame classification (UserCode, ModuleSystem, LibraryInternal, PackageSet, Overlay, FixedPoint)
- Three-tier classification strategy (explicit role, ontology lookup, path heuristics)
- Boundary detection between code regions
- Diagnostic hint generation for known error patterns
- Configurable frame filtering
- Multiple output formats (compact, verbose, rust-style)
- Property-based tests with Hedgehog
- CLI application
