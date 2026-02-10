# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.5.0] - 2025-02-10

### Added
- **Middleware support**: New generic function `eglot-codelens-provide-codelens` that allows users to filter, transform, or extend CodeLens data for specific server types before caching. Users can define methods for their LSP servers to customize CodeLens behavior.

### Changed
- Forward declare `eglot-codelens-mode` variable to improve byte-compilation
- Documentation formatting improvements (line wrapping for long docstrings)

### Fixed
- `eglot-codelens--change-begin-line` now properly returns 1 instead of nil when change tracking is interrupted

### Developer
- Added `--strict` flag to `checkdoc` lint target in Makefile

## [0.4.1] - 2025-02-09

### Fixed
- Ensure `change-begin-line` returns 1 instead of nil
- Fix mock for `eglot-codelens--docver` after defsubst refactoring

### Changed
- Improve document version retrieval with symbol indirection

## [0.4.0] - 2025-02-08

### Added
- Screenshot to documentation

### Changed
- Update screenshot in documentation
