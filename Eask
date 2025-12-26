;;; Eask --- Eask configuration for eglot-codelens  -*- lexical-binding: t; -*-

;;; Commentary:

(package "eglot-codelens"
         "0.3"
         "CodeLens support for Eglot")

(website-url "https://github.com/zsxh/eglot-codelens")
(keywords "eglot" "codelens" "tools")

(package-file "eglot-codelens.el")

;; Source archives
(source "gnu")
(source "nongnu")
(source "melpa")

;; Emacs version requirement
(depends-on "emacs" "30.1")

;; Runtime dependencies are specified in the package file,
;; but we can also declare them here for clarity
(depends-on "compat" "30.1.0.0")
(depends-on "eglot" "1.19")
(depends-on "jsonrpc" "1.0.26")

;; Development dependencies
(development
 (depends-on "ert")                     ; Built-in test framework
 (depends-on "package-lint")            ; Package linting
 )

;; Scripts for common tasks
(script "test" "echo \"Running tests...\" && eask test ert tests/eglot-codelens-test.el")
(script "lint" "echo \"Linting...\" && eask lint checkdoc && eask lint package")
(script "clean" "eask clean all")
