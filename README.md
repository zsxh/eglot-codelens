# eglot-codelens.el

CodeLens support for Eglot, displaying LSP CodeLens information as overlays above code.

## Features

- Display CodeLens information as overlays above code
- Support mouse clicks and keyboard interaction
- Integration with Eglot
- VS Code icon conversion to nerd-icons (optional)

## Requirements

- Emacs 26.3+
- Eglot 1.19+

## Installation

### use-package (Emacs 30+)

```elisp
(use-package eglot-codelens
  :vc (:url "https://github.com/zsxh/eglot-codelens"
       :rev :newest)
  :hook (eglot-managed-mode . eglot-codelens-mode))
```

### Manual

Download `eglot-codelens.el` and add it to your `load-path`:

```elisp
(require 'eglot-codelens)
(add-hook 'eglot-managed-mode-hook #'eglot-codelens-mode)
```

## Usage

The package automatically enables CodeLens when Eglot connects to a server that supports it.

### Manual Control

```elisp
;; Enable in current buffer
M-x eglot-codelens-mode

;; Execute CodeLens at current line
M-x eglot-codelens-execute-at-line
```

### Customization

```elisp
;; Delay before updating CodeLens after changes (default: 0.5s)
(setq eglot-codelens-update-delay 0.3)
```

## License

GPLv3
