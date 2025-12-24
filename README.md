# eglot-codelens.el (WIP)

CodeLens support for Eglot, displaying LSP CodeLens information as overlays above code.

## Screenshot

![Screenshot](https://raw.githubusercontent.com/zsxh/eglot-codelens/refs/heads/imgs/screenshot.webp)

## Features

- `textDocument/codeLens` and `codeLens/resolve` support
- Display CodeLens information as overlays above code
- Automatic CodeLens resolution in visible viewport
- Rate-limited resolve queue to prevent server overload
- Smart overlay caching for improved performance
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
;; Delay before updating CodeLens after document changes (default: 0.5s)
(setq eglot-codelens-update-delay 0.3)

;; Delay before refreshing visible CodeLens after window changes (default: 0.25s)
;; Applies to scroll events and window configuration changes
(setq eglot-codelens-visible-refresh-delay 0.25)

;; Delay between processing each CodeLens resolve request (default: 0.25s)
;; Controls the rate at which pending resolve requests are processed
;; to avoid overwhelming the LSP server
(setq eglot-codelens-resolve-delay 0.25)
```

## License

GPLv3
