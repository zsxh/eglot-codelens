.PHONY: test clean help lint checkdoc package-lint

help:
	@echo "Available targets:"
	@echo "  make test        - Run all tests"
	@echo "  make compile     - Byte compile the package"
	@echo "  make clean       - Remove generated files"
	@echo "  make checkdoc    - Check documentation style"
	@echo "  make package-lint - Check package compliance"
	@echo "  make lint        - Run all linters"

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

checkdoc:
	@echo "Running checkdoc..."
	@emacs -q -batch \
		-l checkdoc \
		--eval "(setq checkdoc-autofix-flag t)" \
		--eval "(checkdoc-file \"eglot-codelens.el\")"

package-lint:
	@echo "Running package-lint..."
	@emacs -q -batch \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) \
			(package-refresh-contents) \
			(package-install 'package-lint))" \
		-l package-lint \
		-f package-lint-batch-and-exit \
		eglot-codelens.el

lint: checkdoc package-lint compile
	@echo "All linters passed!"
