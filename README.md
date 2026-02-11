# eglot-codelens.el

CodeLens support for Eglot, displaying LSP CodeLens information as overlays above code.

## Screenshot

![Screenshot](https://github.com/user-attachments/assets/4880154f-6077-49be-9a1a-83148ecc9a20)

https://github.com/user-attachments/assets/01d225ce-3ce4-4c4a-a962-b48834247b7e

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

- Emacs 30+
- Eglot 1.17+
- [nerd-icons](https://github.com/rainstormstudio/nerd-icons.el) (optional, for VS Code icon conversion)

## Installation

### use-package (Emacs 30+)

```elisp
(use-package eglot-codelens
  :vc (:url "https://github.com/zsxh/eglot-codelens"
       :rev :newest)
  :hook (eglot-managed-mode . eglot-codelens-mode))
```

### Manual

Download `eglot-codelens.el` and add it to your `load-path` or install it via `package-vc-install`:

```elisp
(unless (package-installed-p 'eglot-codelens)
  (package-vc-install
   '(eglot-codelens :url "https://github.com/zsxh/eglot-codelens")))
(require 'eglot-codelens)
(add-hook 'eglot-managed-mode-hook #'eglot-codelens-mode)
```

## Usage

### Enabling the Mode

You can enable `eglot-codelens-mode` automatically via hook:

```elisp
(add-hook 'eglot-managed-mode-hook #'eglot-codelens-mode)
```

Or toggle it manually with `M-x eglot-codelens-mode`.

### Interactive Commands

| Command                          | Description                                         |
|----------------------------------|-----------------------------------------------------|
| `eglot-codelens-execute-at-line` | Execute CodeLens at current line                    |
| `eglot-codelens-execute-dwim`    | Execute CodeLens at or above current line (DWIM)    |

### Customization

```elisp
;; Delay before updating CodeLens after document changes (default: 0.5s)
(setq eglot-codelens-update-delay 0.5)

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
