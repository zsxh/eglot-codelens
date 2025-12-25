#!/bin/bash
# Test runner script for eglot-codelens
# Usage: ./run-tests.sh

set -e

# Add current directory to load path
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Running eglot-codelens tests..."
echo "============================"

# Run tests in batch mode with:
# -q: Don't load init file (avoid user config interference)
# -batch: Run in batch mode (non-interactive)
# -L .: Add current directory to load path
emacs -q -batch -L . \
    -l eglot-codelens.el \
    -l tests/eglot-codelens-test.el \
    -f ert-run-tests-batch-and-exit

echo ""
echo "============================"
echo "All tests passed!"
