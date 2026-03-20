# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.6.0] - 2025-02-12

### Added
- **Line flash effect**: Visual feedback using `pulse-momentary-highlight-one-line` when executing CodeLens at a line
- **New command**: `eglot-codelens-execute-dwim` scans backwards from current line to find and execute the nearest CodeLens

### Changed
- `eglot-codelens-execute-at-line` now requires a `line` parameter (when called from Elisp, the line number must be provided)
- `eglot-codelens-provide-codelens` now accepts a `uri` parameter for better middleware support

### Fixed
- Reworded docstrings to satisfy strict checkdoc

### Developer
- Set `persist-credentials` to false in test job workflow

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
