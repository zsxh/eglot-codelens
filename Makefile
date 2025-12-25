.PHONY: test clean help

help:
	@echo "Available targets:"
	@echo "  make test    - Run all tests"
	@echo "  make clean   - Remove generated files"
	@echo "  make compile - Byte compile the package"

test:
	@echo "Running eglot-codelens tests..."
	@emacs -q -batch -L . -l eglot-codelens.el -l tests/eglot-codelens-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	@echo "Byte compiling eglot-codelens.el..."
	@emacs -q -batch -f batch-byte-compile eglot-codelens.el

clean:
	@echo "Cleaning generated files..."
	@rm -f eglot-codelens.elc
	@rm -f tests/eglot-codelens-test.elc
