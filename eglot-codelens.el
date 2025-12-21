;;; eglot-codelens.el --- CodeLens support for Eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Zxsh Chen

;; Version: 0.1
;; Author: Zxsh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/eglot-codelens
;; Keywords: eglot, codelens, tools
;; Package-Requires: ((emacs "26.3") (eglot "1.19"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides CodeLens support for Eglot.
;; It displays CodeLens information as overlays above
;; code and supports user interaction through mouse clicks and keyboard.
;;
;; Usage:
;; Simply enable eglot-codelens-mode in buffers managed by Eglot. The mode
;; will be automatically enabled when Eglot connects to a server that supports
;; CodeLens functionality.
;;
;;   M-x eglot-codelens-mode
;;
;; Or add it to your init file to enable it automatically in all Eglot buffers:
;;
;;   (add-hook 'eglot-managed-mode-hook #'eglot-codelens-mode)
;;
;; The package integrates with Eglot's extension mechanism and doesn't
;; interfere with Eglot's core functionality.

;;; Code:

(require 'eglot)
(require 'cl-lib)

(require 'nerd-icons nil t)

;;; Customization Options

(defgroup eglot-codelens nil
  "CodeLens support for Eglot."
  :group 'eglot
  :prefix "eglot-codelens-")

(defcustom eglot-codelens-update-delay 0.5
  "Delay in seconds before updating CodeLens after document changes."
  :type 'float
  :group 'eglot-codelens)

;;; Faces

(defface eglot-codelens-face
  '((t :inherit font-lock-comment-face :height 0.9))
  "Face for displaying CodeLens information."
  :group 'eglot-codelens)

(defface eglot-codelens-mouse-face
  '((t :inherit highlight :box (:line-width 1 :color "gray")))
  "Face for CodeLens when mouse is over them."
  :group 'eglot-codelens)

;;; Core Data Structures and Cache Management

(defvar-local eglot-codelens--cache nil
  "Cache for raw LSP CodeLens objects in current buffer.
Each element is a plist representing a CodeLens object:
(:range ((start . ((line . 10) (character . 0)))
         (end . ((line . 10) (character . 20))))
 :command ((title . \"3 references\") ...)
 :data (...))")

(defvar-local eglot-codelens--version nil
  "Document version for cached CodeLens.")


(defvar-local eglot-codelens--update-timer nil
  "Timer for delayed CodeLens updates.")

;;; LSP Protocol Handlers

(defun eglot-codelens--fetch-codelens (cb)
  "Fetch CodeLens for current buffer with Callback CB."
  (let* ((method :textDocument/codeLens)
         (params (list :textDocument (eglot--TextDocumentIdentifier))))
    (eglot--async-request
     (eglot--current-server-or-lose) method params
     :success-fn cb
     :hint method)))

(defun eglot-codelens--resolve-codelens (codelens)
  "Resolve CODELENS details."
  (when-let* ((server (eglot-current-server)))
    (if (eglot-server-capable :codeLensProvider :resolveProvider)
        (eglot--request server :codeLens/resolve codelens)
      (message "Server does not support CodeLens resolve"))))

;;; UI Display System

(defun eglot-codelens--build-display-string (ov codelens line-start index total-codelens)
  "Build display string for CODELENS overlay OV at LINE-START with INDEX and TOTAL-CODELENS."
  (let* ((is-first (= index 0))
         (is-last (= index (1- total-codelens)))
         (indentation (if is-first
                          (save-excursion
                            (goto-char line-start)
                            (make-string (current-indentation) ? ))
                        ""))
         (separator (if is-last "\n" "|")))
    (concat
     indentation
     (propertize (eglot-codelens--format-text codelens)
                 'face 'eglot-codelens-face
                 'mouse-face 'eglot-codelens-mouse-face
                 'help-echo (eglot-codelens--help-text codelens)
                 'keymap (let ((map (make-sparse-keymap)))
                           (define-key map [mouse-1]
                             (lambda () (interactive)
                               (eglot-codelens-execute-or-resolve ov)))
                           map))
     separator)))

(defun eglot-codelens--make-overlay (line-start codelens index total-codelens)
  "Create overlay for CODELENS at LINE-START with INDEX and TOTAL-CODELENS count."
  (let ((ov (make-overlay line-start line-start)))
    ;; Priority increases: 0, 1, 2... matching LSP return order
    (overlay-put ov 'priority index)

    ;; Add identification for cleanup and store data
    (overlay-put ov 'eglot-codelens codelens)

    ;; Set display string
    (overlay-put ov 'before-string
                 (eglot-codelens--build-display-string
                  ov codelens line-start index total-codelens))
    ov))

(defun eglot-codelens--codicons-to-nerd-icons (title)
  "Convert VS Code icon placeholders in TITLE to nerd icons.
VS Code icon placeholder syntax: $(icon-name)
Converts to nerd icons using nerd-icons-codicon.
If the icon is not recognized, returns the original placeholder."
  (if (featurep 'nerd-icons)
      (replace-regexp-in-string
       "\\$(\\([^)]+\\))"
       (lambda (match)
         (let* ((icon-name (match-string 1 match))
                (nerd-icon-name (replace-regexp-in-string "-" "_" icon-name))
                (icon-code (format "nf-cod-%s" nerd-icon-name)))
           (condition-case err
               (nerd-icons-codicon icon-code)
             (error
              ;; If nerd-icons-codicon fails, return the original placeholder
              match))))
       title
       nil
       t)
    title))

(defun eglot-codelens--format-text (codelens)
  "Format display text for CODELENS."
  (if-let* ((command (plist-get codelens :command))
            (title (plist-get command :title)))
      (eglot-codelens--codicons-to-nerd-icons title)
    "Resolve CodeLens"))

(defun eglot-codelens--help-text (codelens)
  "Generate help text for CODELENS."
  (if (plist-get codelens :command)
      "Click to execute this CodeLens command"
    "Click to resolve CodeLens..."))

(defun eglot-codelens--cleanup-overlays ()
  "Clean up all CodeLens overlays in current buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'eglot-codelens)
      (delete-overlay ov))))

(defun eglot-codelens--update-single-overlay (new-codelens ov)
  "Update overlay OV with NEW-CODELENS data."
  ;; Calculate total CodeLens on the same line - only check overlays at this line position
  (let* ((line-start (overlay-start ov))
         (line-end (line-end-position (line-number-at-pos line-start)))
         (codelens-on-line (cl-loop for ov in (overlays-in line-start line-end)
                                    when (and (overlay-get ov 'eglot-codelens)
                                              (= (overlay-start ov) line-start))
                                    collect (overlay-get ov 'eglot-codelens)))
         (total-on-line (length codelens-on-line)))
    ;; Update the display string using the common function
    (overlay-put ov 'before-string
                 (eglot-codelens--build-display-string
                  ov
                  new-codelens
                  line-start
                  (overlay-get ov 'priority)
                  total-on-line))))

(defun eglot-codelens--render-codelens (codelens-list)
  "Render CODELENS-LIST in current buffer."
  (eglot-codelens--cleanup-overlays)

  (when codelens-list
    (with-silent-modifications
      (save-excursion
        ;; Group CodeLens by line
        (let ((line-groups (make-hash-table :test 'equal)))
          ;; First pass: group CodeLens by line number
          (cl-loop for codelens across codelens-list
                   for range = (plist-get codelens :range)
                   for line = (1+ (plist-get (plist-get range :start) :line))
                   do (push codelens (gethash line line-groups)))

          ;; Second pass: render each line's CodeLens
          (maphash (lambda (line codelens-on-line)
                     (let* ((sorted-codelens (nreverse codelens-on-line))
                            (line-start (progn
                                          (goto-char (point-min))
                                          (forward-line (1- line))
                                          (line-beginning-position)))
                            (total-on-line (length sorted-codelens)))
                       (cl-loop for codelens in sorted-codelens
                                for index from 0
                                do (eglot-codelens--make-overlay line-start codelens index total-on-line))))
                   line-groups))))))

;;; Interaction Handling
(defun eglot-codelens-execute-or-resolve (ov)
  "Execute CodeLens command or resolve CodeLens at overlay OV."
  (if-let* ((codelens (overlay-get ov 'eglot-codelens))
            (command (plist-get codelens :command)))
      ;; Execute resolved command
      (eglot-execute (eglot--current-server-or-lose) command)
    ;; Resolve command and update overlay
    (eglot-codelens--resolve-and-update codelens ov)))

(defun eglot-codelens--resolve-and-update (codelens ov)
  "Resolve CODELENS and then update overlay OV."
  (if-let* ((resolved (eglot-codelens--resolve-codelens codelens)))
      (progn
        ;; Update cache with resolved codelens
        (cl-remf codelens :data)
        (plist-put codelens :command (plist-get resolved :command))
        ;; Update only the specific overlay
        (eglot-codelens--update-single-overlay codelens ov))
    (message "Failed to resolve CodeLens command")))

(defun eglot-codelens-execute-at-line ()
  "Execute CodeLens at current line.
If there's only one CodeLens, execute it directly.
If there are multiple, show a selection menu for user to choose."
  (interactive)
  (if-let* ((ovs (cl-loop for ov in (overlays-in (line-beginning-position) (line-end-position))
                          when (overlay-get ov 'eglot-codelens)
                          collect ov)))
      (if (= (length ovs) 1)
          ;; Only one CodeLens, execute directly
          (eglot-codelens-execute-or-resolve (car ovs))
        ;; Multiple CodeLens, sort by priority and show selection menu
        (let* ((sorted-ovs (sort ovs (lambda (a b)
                                      (< (overlay-get a 'priority)
                                         (overlay-get b 'priority)))))
               (choices (cl-loop for ov in sorted-ovs
                                for priority = (overlay-get ov 'priority)
                                for codelens = (overlay-get ov 'eglot-codelens)
                                collect (cons (format "[%d] %s" priority (eglot-codelens--format-text codelens)) ov)))
               (vertico-sort-function nil) ;; No sorting if using vertico
               (selected-ov (cdr (assoc (completing-read "Select CodeLens: " choices) choices))))
          (when selected-ov
            (eglot-codelens-execute-or-resolve selected-ov))))
    (message "No CodeLens found at this line.")))

;;; Eglot Integration

(defun eglot-codelens--server-supports-codelens ()
  "Check if current server supports CodeLens."
  (eglot-server-capable :codeLensProvider))

(defun eglot-codelens--setup-buffer ()
  "Setup CodeLens for current buffer."
  (when eglot-codelens-mode
    ;; Initialize buffer-local variables
    (setq eglot-codelens--cache nil
          eglot-codelens--version nil)
    ;; Add Eglot document change hook
    (add-hook 'eglot--document-changed-hook #'eglot-codelens--on-document-change nil t)))

(defun eglot-codelens--cleanup-buffer ()
  "Cleanup CodeLens for current buffer."
  ;; Cancel any pending update timer
  (when eglot-codelens--update-timer
    (cancel-timer eglot-codelens--update-timer)
    (setq eglot-codelens--update-timer nil))

  ;; Remove all overlays
  (eglot-codelens--cleanup-overlays)

  ;; Clear cache and version
  (setq eglot-codelens--cache nil
        eglot-codelens--version nil)

  ;; Remove Eglot document change hook
  (remove-hook 'eglot--document-changed-hook #'eglot-codelens--on-document-change t))

(defun eglot-codelens--on-document-change (&rest _args)
  "Handle document changes via Eglot's document-changed hook."
  (when eglot-codelens-mode
    ;; If there's already a timer, just reset its time instead of canceling and recreating
    (if (timerp eglot-codelens--update-timer)
        ;; Reset existing timer's time
        (timer-set-idle-time eglot-codelens--update-timer
                             (time-add (current-idle-time)
                                       (seconds-to-time eglot-codelens-update-delay)))
      ;; Create new timer if none exists
      (setq eglot-codelens--update-timer
            (run-with-idle-timer eglot-codelens-update-delay nil
                                 (lambda (buf)
                                   (when (timerp eglot-codelens--update-timer)
                                     (cancel-timer eglot-codelens--update-timer))
                                   (setq eglot-codelens--update-timer nil)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (eglot-codelens--update-buffer))))
                                 (current-buffer))))))

(defun eglot-codelens--update-buffer ()
  "Update CodeLens display in current buffer."
  (let* ((docver eglot--versioned-identifier)
         (buf (current-buffer)))
    (eglot-codelens--fetch-codelens
     (lambda (codelens-list)
       (eglot--when-live-buffer buf
         (when (eq docver eglot--versioned-identifier)
           (setq eglot-codelens--cache codelens-list
                 eglot-codelens--version docver)
           (eglot-codelens--render-codelens codelens-list)))))))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode eglot-codelens-mode
  "Minor mode for displaying LSP CodeLens in Eglot."
  :group 'eglot-codelens
  (cond
   (eglot-codelens-mode
    (if (and eglot--managed-mode (eglot-codelens--server-supports-codelens))
        (progn
          (eglot-codelens--setup-buffer)
          (eglot-codelens--update-buffer))
      (eglot-codelens--cleanup-buffer)))
   (t
    (eglot-codelens--cleanup-buffer))))

(provide 'eglot-codelens)

;;; eglot-codelens.el ends here
