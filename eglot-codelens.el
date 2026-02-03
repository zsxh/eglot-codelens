;;; eglot-codelens.el --- CodeLens support for Eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Zxsh Chen

;; Version: 0.4.1
;; Author: Zxsh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/eglot-codelens
;; Keywords: eglot, codelens, tools
;; Package-Requires: ((emacs "30.1") (compat "30.1.0.1") (eglot "1.17.30") (jsonrpc "1.0.24"))

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
;; Simply enable eglot-codelens-mode in buffers managed by Eglot.  The mode
;; will be automatically enabled when Eglot connects to a server that supports
;; CodeLens functionality.
;;
;;   M-x eglot-codelens-mode
;;
;; Or add it to your init file to enable it automatically in all Eglot buffers:
;;
;;   (add-hook 'eglot-managed-mode-hook #'eglot-codelens-mode)
;;
;; NOTE: This extension relies on some eglot--internal symbols
;; (eglot--docver, eglot--TextDocumentIdentifier, etc).


;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'eglot)

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

(defcustom eglot-codelens-visible-refresh-delay 0.25
  "Delay in seconds before refreshing visible CodeLens after window changes.
This applies to scroll events and window configuration changes."
  :type 'float
  :group 'eglot-codelens)

(defcustom eglot-codelens-resolve-delay 0.25
  "Delay in seconds between processing each CodeLens resolve request.
This controls the rate at which pending resolve requests are processed
to avoid overwhelming the LSP server."
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
  "Cache for CodeLens objects grouped by line in current buffer.
A hash table with line numbers as keys.
Each value is a SORTED list of CODELENS-OVERLAY-CELL where CODELENS-OVERLAY-CELL is (CODELENS . OVERLAY).")

(defvar-local eglot-codelens--prev-line-count nil
  "Previous document line count for calculating offset during line changes.
Used to compute delta when reusing overlays across document edits.")

(defvar-local eglot-codelens--version nil
  "Document version for cached CodeLens.")

(defvar-local eglot-codelens--recent-changes nil
  "Recent buffer changes as collected by `eglot--before-change'.")

(defvar-local eglot-codelens--update-timer nil
  "Timer for delayed CodeLens updates.")

(defvar-local eglot-codelens--refresh-timer nil
  "Timer for delayed CodeLens refresh on window visible changes.")

(defvar-local eglot-codelens--pending-lines nil
  "List of line numbers with pending CodeLens to be rendered.
This is used to track which lines need processing during partial updates.")

(defvar-local eglot-codelens--resolve-queue nil
  "Queue of pending CodeLens resolve requests.
Each element is (DOCVER . CODELENS-CELL) where CODELENS-CELL is (CODELENS . OVERLAY).
This is used to batch resolve requests with debouncing.")

(defvar-local eglot-codelens--resolve-timer nil
  "Timer for processing CodeLens resolve queue.")

(defvar-local eglot-codelens--overlays nil
  "List of all CodeLens overlays in current buffer.
This is used to efficiently clean up stale overlays without traversing
the entire buffer's overlay list.")

(defun eglot-codelens--docver ()
  "Get the current document version for CodeLens tracking.
Returns `eglot--docver' if available, otherwise falls back to
`eglot--versioned-identifier'."
  (or eglot--docver eglot--versioned-identifier))

(defun eglot-codelens--build-cache (codelens-list)
  "Build cache from CODELENS-LIST.
Returns a hash table with line numbers as keys.
Each value is a sorted list of CODELENS-OVERLAY-CELL where CODELENS-OVERLAY-CELL is (CODELENS . OVERLAY)."
  (when (and codelens-list (vectorp codelens-list) (length> codelens-list 0))
    (let ((line-groups (make-hash-table :test 'eq)))
      ;; Group CodeLens by line number
      (cl-loop for codelens across codelens-list
               when (and (listp codelens) (plist-get codelens :range))
               for range = (plist-get codelens :range)
               for start = (when range (plist-get range :start))
               for line-num = (when start (plist-get start :line))
               when (and (integerp line-num) (>= line-num 0))
               ;; Position in a text document expressed as zero-based line and zero-based character offset.
               do (push (cons codelens nil) (gethash (1+ line-num) line-groups)))

      ;; Reverse each list to maintain LSP order (sorted by index)
      (maphash (lambda (line codelens-on-line)
                 (puthash line (nreverse codelens-on-line) line-groups))
               line-groups)
      line-groups)))

;;; LSP Protocol Handlers

(defun eglot-codelens--resolve-codelens (codelens-cell)
  "Resolve CODELENS-CELL and update its overlay.
Makes an async request to :codeLens/resolve.
CODELENS-CELL is a cons cell (CODELENS . OVERLAY)."
  (when-let* ((codelens (car codelens-cell))
              (ov (cdr codelens-cell))
              (buf (current-buffer))
              (server (eglot-current-server)))
    (when (and server (eglot-server-capable :codeLensProvider :resolveProvider)
               codelens (overlayp ov) (overlay-buffer ov))
      (jsonrpc-async-request
       server :codeLens/resolve codelens
       :success-fn (lambda (resolved)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (eglot-codelens--update-resolved-codelens codelens-cell resolved))))
       :deferred :codeLens/resolve))))

(defun eglot-codelens--resolve-schedule ()
  "Schedule processing of the CodeLens resolve queue.
Starts processing the queue immediately and sets up a timer to pace
subsequent requests at `eglot-codelens-resolve-delay' intervals.
If a timer is already running, does nothing (prevents duplicate scheduling)."
  (unless (and eglot-codelens--resolve-timer
               (timerp eglot-codelens--resolve-timer))
    (eglot-codelens--resolve-process-queue)
    (setq eglot-codelens--resolve-timer
          (run-with-timer
           eglot-codelens-resolve-delay nil
           (lambda (buf)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when eglot-codelens--resolve-timer
                   (cancel-timer eglot-codelens--resolve-timer))
                 (setq eglot-codelens--resolve-timer nil)
                 (when eglot-codelens--resolve-queue
                   (eglot-codelens--resolve-schedule)))))
           (current-buffer)))))

(defun eglot-codelens--resolve-process-queue ()
  "Process one item from the resolve queue.
Each item is (DOCVER . CODELENS-CELL) where CODELENS-CELL is (CODELENS . OVERLAY)."
  (when eglot-codelens--resolve-queue
    ;; Remove outdated items
    (setq eglot-codelens--resolve-queue
          (cl-delete-if (lambda (e)
                          (not (eq (car e) eglot-codelens--version)))
                        eglot-codelens--resolve-queue))
    ;; Process one item from the queue
    (let ((queue-item (pop eglot-codelens--resolve-queue)))
      (when queue-item
        (eglot-codelens--resolve-codelens (cdr queue-item))))))

(defun eglot-codelens--fetch-codelens ()
  "Fetch CodeLens from server and update display in current buffer.
Makes an async request to :textDocument/codeLens.
Renders only the visible area initially, with the full cache stored
for later visible-area refreshes."
  (when-let* ((server (eglot-current-server))
              (docver (eglot-codelens--docver))
              (buf (current-buffer)))
    (jsonrpc-async-request
     server
     :textDocument/codeLens (list :textDocument (eglot--TextDocumentIdentifier))
     :success-fn (lambda (codelens-list)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (when (and eglot-codelens-mode
                                  (eq docver (eglot-codelens--docver))
                                  (eq (window-buffer (selected-window)) buf))
                         ;; Save old cache before updating
                         (let ((old-cache eglot-codelens--cache)
                               (new-cache (eglot-codelens--build-cache codelens-list))
                               (range (eglot-codelens--visible-range))
                               all-lines)

                           (when new-cache
                             (maphash (lambda (line _) (push line all-lines)) new-cache))

                           ;; Initialize pending-lines with all lines from new cache
                           (setq eglot-codelens--cache new-cache
                                 eglot-codelens--version docver
                                 eglot-codelens--pending-lines all-lines)

                           (eglot-codelens--render-codelens
                            new-cache docver all-lines t old-cache range))))))
     :deferred :textDocument/codeLens)))

;;; UI Display System

(defun eglot-codelens--build-display-string (codelens-cell line-start index total-codelens)
  "Build display string for CODELENS-CELL at LINE-START with INDEX and TOTAL-CODELENS.
CODELENS-CELL is a cons cell (CODELENS . OVERLAY)."
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
     (propertize (eglot-codelens--format-text codelens-cell)
                 'face 'eglot-codelens-face
                 'mouse-face 'eglot-codelens-mouse-face
                 'help-echo "Click to execute this CodeLens command"
                 'keymap (let ((map (make-sparse-keymap)))
                           (define-key map [mouse-1]
                             (lambda () (interactive)
                               (eglot-codelens-execute codelens-cell)))
                           map))
     separator)))

(defun eglot-codelens--make-overlay (line-start codelens-cell index total-codelens docver)
  "Create overlay for CODELENS-CELL at LINE-START with INDEX and TOTAL-CODELENS count.
CODELENS-CELL is a cons cell (CODELENS . OVERLAY).
DOCVER is the document version for tracking overlay validity.
Returns the created overlay."
  (let* ((ov (make-overlay line-start line-start)))

    ;; Priority increases: 0, 1, 2... matching LSP return order
    (overlay-put ov 'priority index)

    ;; Add identification for cleanup and store data
    (overlay-put ov 'eglot-codelens t)

    ;; Document version for which this overlay displays content.
    ;; Used to verify the overlay's display data is still current with the document.
    (overlay-put ov 'eglot-codelens-docver docver)

    ;; Cache version tracking whether this overlay is still referenced in the current cache.
    ;; Used for cleanup - overlays not referenced in the new cache iteration are deleted.
    (overlay-put ov 'eglot-codelens-usever docver)

    (when-let* ((codelens (car codelens-cell))
                (command (plist-get codelens :command)))
      (overlay-put ov 'eglot-codelens-command command))

    ;; Set display string
    (overlay-put ov 'before-string
                 (eglot-codelens--build-display-string
                  codelens-cell line-start index total-codelens))

    ;; Register overlay in tracking list
    (push ov eglot-codelens--overlays)
    ov))

(declare-function nerd-icons-codicon "nerd-icons" (icon-name &rest _args))

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
           (condition-case _
               (nerd-icons-codicon icon-code)
             (error
              ;; If nerd-icons-codicon fails, return the original placeholder
              match))))
       title
       nil
       t)
    title))

(defun eglot-codelens--format-text (codelens-cell)
  "Format display text for CODELENS-CELL.
CODELENS-CELL is a cons cell (CODELENS . OVERLAY)."
  (let* ((codelens (car codelens-cell))
         (ov (cdr codelens-cell))
         (command (or (plist-get codelens :command)
                      (and ov (overlayp ov) (overlay-buffer ov)
                           (overlay-get ov 'eglot-codelens-command))))
         (title (when (listp command) (plist-get command :title))))
    (if title
        (eglot-codelens--codicons-to-nerd-icons title)
      "Loading...")))

(defun eglot-codelens--cleanup-overlays ()
  "Clean up all CodeLens overlays in current buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'eglot-codelens)
      (delete-overlay ov))))

(defun eglot-codelens--update-resolved-codelens (codelens-cell resolved)
  "Update overlay in CODELENS-CELL with RESOLVED CodeLens data.
CODELENS-CELL is a cons cell (CODELENS . OVERLAY).
RESOLVED is the resolved CodeLens data from the server."
  (let* ((ov (cdr codelens-cell))
         (command (plist-get resolved :command)))
    (when (and eglot-codelens-mode
               ov (overlayp ov) (overlay-buffer ov)
               (eq (overlay-get ov 'eglot-codelens-docver) eglot-codelens--version))
      ;; Update cache with resolved codelens
      (setcar codelens-cell resolved)

      ;; Update the display string
      (let* ((line-start (overlay-start ov))
             (line (line-number-at-pos line-start t))
             ;; Find line group in cache (hashtable lookup)
             (sorted-codelens (gethash line eglot-codelens--cache))
             (total-on-line (if sorted-codelens
                                (length sorted-codelens)
                              1)))
        (overlay-put ov 'eglot-codelens-command command)
        (overlay-put ov 'before-string
                     (eglot-codelens--build-display-string
                      codelens-cell
                      line-start
                      (overlay-get ov 'priority)
                      total-on-line))))))

(defun eglot-codelens--line-delta ()
  "Calculate the line count delta and update the previous line count cache.

Returns the difference between the current buffer line count and the
previous line count (`eglot-codelens--prev-line-count').  A positive
value indicates lines were inserted, negative indicates lines were
deleted, and zero means no change.

As a side effect, updates `eglot-codelens--prev-line-count' to the
current line count for the next iteration.

This delta is used during overlay reuse to adjust line lookups when
the file has changed.  Overlays before the cursor position are assumed
to be unaffected by edits, while overlays at or after the cursor need
their line numbers adjusted by the delta."
  (let* ((line-count (line-number-at-pos (point-max) t))
         (prev-line-count eglot-codelens--prev-line-count)
         (line-delta (if (and prev-line-count (integerp prev-line-count))
                         (- line-count prev-line-count)
                       0)))
    ;; Update prev-line-count for next iteration
    (setq eglot-codelens--prev-line-count line-count)
    line-delta))

(defun eglot-codelens--render-codelens
    (new-cache docver pending-lines file-changed-p &optional old-cache range)
  "Render CodeLens overlays, reusing overlays from OLD-CACHE when possible.

Arguments:
  NEW-CACHE     - Hash table (line -> sorted list of CODELENS-OVERLAY-CELL)
                  containing the CodeLens data to render.
  DOCVER        - Document version for tracking overlay validity.
  PENDING-LINES - List of line numbers to process.
  FILE-CHANGED-P - Non-nil if the file content has changed, triggering full
                  cleanup and delta calculation for line adjustments.

Optional arguments:
  OLD-CACHE     - Previous cache for overlay reuse.  When provided, existing
                  overlays are reused where possible to minimize flicker.
  RANGE         - Cons cell (BEG-LINE . END-LINE) specifying the line number
                  range to render.  When provided, only CodeLens within this
                  range are updated.  Existing overlays in the range that
                  already have DOCVER are preserved unchanged.

Overlay Reuse Algorithm:
  1. For lines before the cursor: use direct line lookup in OLD-CACHE.
  2. For lines at/after the cursor: adjust lookup by LINE-DELTA (insertions/deletions).
  3. Overlays are matched by index position within each line.
  4. Outside RANGE: overlays are reused without updating display content.
  5. When FILE-CHANGED-P: clean up overlays not referenced in NEW-CACHE.

This function also schedules CodeLens resolve requests for CodeLens
without a :command property, adding them to `eglot-codelens--resolve-queue'."
  (with-silent-modifications
    (save-excursion
      (let* (lines-processed
             resolve-queue)
        ;; Step 1: Process pending lines
        (when (and new-cache pending-lines)
          (let* ((lines-to-process (sort pending-lines #'<))
                 (current-line 1)
                 (line-delta (if file-changed-p (eglot-codelens--line-delta) 0))
                 (beg-line (eglot-codelens--change-begin-line)))
            (goto-char (point-min))
            (dolist (line lines-to-process)
              (forward-line (- line current-line))
              (setq current-line line)
              (let* ((line-start (point))
                     (new-sorted (gethash line new-cache)))
                (when new-sorted
                  (let* ((in-range-p (or (not range)
                                         (and (>= line (car range))
                                              (<= line (cdr range)))))
                         (total-on-line (length new-sorted))
                         ;; Find corresponding line group in old cache
                         ;; Before cursor: assume no change, use line directly
                         ;; At/After cursor: adjust by delta (lines inserted/deleted)
                         (lookup-line (when old-cache
                                        (if (and (/= line-delta 0)
                                                 (> line beg-line))
                                            (- line line-delta)
                                          line)))
                         (old-sorted (when old-cache
                                       (gethash lookup-line old-cache))))

                    ;; Process each CodeLens by index
                    (cl-loop for new-cell in new-sorted
                             for codelens = (car new-cell)
                             for new-ov = (cdr new-cell)
                             for index from 0
                             for old-cell = (when old-sorted
                                              (nth index old-sorted))
                             for old-ov = (when old-cell
                                            (cdr old-cell))
                             do
                             ;; 1) Handle overlays
                             (cond
                              ;; Within range: update/create overlay with new codelens data
                              (in-range-p
                               (cond
                                ;; update existing overlay from new-cache
                                ((and new-ov (overlayp new-ov) (overlay-buffer new-ov))
                                 (unless (eq (overlay-get new-ov 'eglot-codelens-docver) docver)
                                   (overlay-put new-ov 'before-string
                                                (eglot-codelens--build-display-string
                                                 new-cell
                                                 line-start
                                                 index
                                                 total-on-line))
                                   (overlay-put new-ov 'eglot-codelens-docver docver)
                                   (move-overlay new-ov line-start line-start)))

                                ;; reuse and update existing overlay from old-cache
                                ((and old-ov (overlayp old-ov) (overlay-buffer old-ov))
                                 (setcdr new-cell old-ov)
                                 (overlay-put old-ov 'before-string
                                              (eglot-codelens--build-display-string
                                               new-cell
                                               line-start
                                               index
                                               total-on-line))
                                 (overlay-put old-ov 'eglot-codelens-docver docver)
                                 (overlay-put old-ov 'eglot-codelens-usever docver)
                                 (move-overlay old-ov line-start line-start))

                                ;; create new overlay
                                (t
                                 (let ((new-ov (eglot-codelens--make-overlay
                                                line-start new-cell index total-on-line docver)))
                                   (setcdr new-cell new-ov)))))

                              ;; Outside range: reuse overlay from old-cache, only update usever
                              ((and old-ov (overlayp old-ov) (overlay-buffer old-ov))
                               (setcdr new-cell old-ov)
                               (overlay-put old-ov 'eglot-codelens-usever docver)))

                             ;; 2) Check if codelens needs to be resolved
                             (when (and in-range-p
                                        (not (plist-get codelens :command)))
                               (push (cons docver new-cell) resolve-queue)))

                    ;; Track this line as processed if in range
                    (when in-range-p
                      (push line lines-processed))))))))

        ;; Step 2: Delete overlays with old usever (entire buffer)
        ;; Use tracked overlay list for efficient iteration
        (when file-changed-p
          (let ((retained nil))
            (dolist (ov eglot-codelens--overlays)
              (if (and (overlayp ov) (overlay-buffer ov)
                       (eq (overlay-get ov 'eglot-codelens-usever) docver))
                  (push ov retained)
                (delete-overlay ov)))
            (setq eglot-codelens--overlays retained)))

        ;; Step 3: Update pending-lines cache (remove processed lines)
        (when lines-processed
          (setq eglot-codelens--pending-lines
                (cl-loop for line in eglot-codelens--pending-lines
                         unless (memq line lines-processed)
                         collect line)))

        ;; Step 4: Add resolve-queue items to global queue and schedule
        (when resolve-queue
          (setq eglot-codelens--resolve-queue
                (append eglot-codelens--resolve-queue (nreverse resolve-queue)))
          (eglot-codelens--resolve-schedule))))))

;;; Interaction Handling
(defun eglot-codelens-execute (codelens-cell)
  "Execute CodeLens command from CODELENS-CELL.
If the command is already available (from codelens or overlay), execute it directly.
If the command needs resolving, trigger the resolve process.
CODELENS-CELL is a cons cell (CODELENS . OVERLAY)."
  (let* ((codelens (car codelens-cell))
         (ov (cdr codelens-cell))
         (codelens-command (plist-get codelens :command))
         (overlay-command (when (and ov (overlayp ov) (overlay-buffer ov))
                            (overlay-get ov 'eglot-codelens-command)))
         (command (or codelens-command overlay-command))
         (server (eglot-current-server)))
    (cond
     ;; Execute resolved command
     (command
      (eglot-execute (eglot--current-server-or-lose) command))
     ;; Try to resolve if server supports resolve provider
     ((and server (eglot-server-capable :codeLensProvider :resolveProvider))
      (eglot-codelens--resolve-codelens codelens-cell)
      (message "Resolving CodeLens command..."))
     ;; No command available and server doesn't support resolve
     (t
      (message "CodeLens command not available")))))

;;;###autoload
(defun eglot-codelens-execute-at-line ()
  "Execute CodeLens at current line.
If there's only one CodeLens, execute it directly.
If there are multiple, show a selection menu for user to choose."
  (interactive)
  (let* ((line (line-number-at-pos (point) t))
         ;; Get line group from cache (hashtable lookup)
         (sorted-codelens (gethash line eglot-codelens--cache)))
    (if sorted-codelens
        (if (= (length sorted-codelens) 1)
            ;; Only one CodeLens, execute it directly from cache
            (eglot-codelens-execute (car sorted-codelens))
          ;; Multiple CodeLens, show selection menu using cached sorted list
          (let* ((choices (cl-loop for codelens-cell in sorted-codelens
                                   for index from 0
                                   collect (cons
                                            (format "[%d] %s" index
                                                    (eglot-codelens--format-text codelens-cell))
                                            codelens-cell)))
                 (vertico-sort-function nil) ;; No sorting if using vertico
                 (selected-cell (cdr (assoc
                                      (completing-read (format "CodeLens (L%d): " line) choices)
                                      choices))))
            (when selected-cell
              (eglot-codelens-execute selected-cell))))
      (message (format "No CodeLens found at line %d." line)))))

;;; Eglot Integration

(defun eglot-codelens--collect-recent-changes (_beg _end _pre-change-length)
  "Collect recent changes."
  (when eglot-codelens-mode
    (setq eglot-codelens--recent-changes eglot--recent-changes)))

(defun eglot-codelens--change-begin-line ()
  "Get the beginning line number of recent buffer changes.

Analyzes `eglot-codelens--recent-changes' (collected from Eglot's change tracking)
to find the first line affected by edits.  Returns the minimum line number from
all change ranges, or 1 if there are no recent changes or if the change tracking
was interrupted by Emacs messup.

This is used in overlay reuse to determine where line adjustments are needed:
overlays before this line use direct lookup, while overlays at or after this line
need delta adjustment."
  (if (and eglot-codelens--recent-changes
           (not (eq :emacs-messup eglot-codelens--recent-changes)))
      (let ((beg-line most-positive-fixnum))
        (cl-loop for (beg _end _len _text) in eglot-codelens--recent-changes
                 for line = (plist-get beg :line)
                 do (when (< line beg-line)
                      (setq beg-line line)))
        beg-line)
    1))

(defun eglot-codelens--schedule-visible-refresh (&rest _args)
  "Handle window scroll/resize to refresh visible CodeLens with debouncing."
  (when eglot-codelens-mode
    ;; If there's already a timer, just reset its time
    (if (timerp eglot-codelens--refresh-timer)
        ;; Reset existing timer's time
        (timer-set-idle-time eglot-codelens--refresh-timer eglot-codelens-visible-refresh-delay)
      ;; Create new timer if none exists
      (setq eglot-codelens--refresh-timer
            (run-with-idle-timer
             eglot-codelens-visible-refresh-delay nil
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (timerp eglot-codelens--refresh-timer)
                     (cancel-timer eglot-codelens--refresh-timer))
                   (setq eglot-codelens--refresh-timer nil)
                   (when (eq (window-buffer (selected-window)) buf)
                     (eglot-codelens--refresh-visible-area)))))
             (current-buffer))))))

(defun eglot-codelens--on-document-change (&rest _args)
  "Handle document changes via Eglot's document-changed hook."
  (when eglot-codelens-mode
    ;; If there's already a timer, just reset its time instead of canceling and recreating
    (if (timerp eglot-codelens--update-timer)
        ;; Reset existing timer's time
        (timer-set-idle-time eglot-codelens--update-timer eglot-codelens-update-delay)
      ;; Create new timer if none exists
      (setq eglot-codelens--update-timer
            (run-with-idle-timer
             eglot-codelens-update-delay nil
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (timerp eglot-codelens--update-timer)
                     (cancel-timer eglot-codelens--update-timer))
                   (setq eglot-codelens--update-timer nil)
                   (eglot-codelens--fetch-codelens))))
             (current-buffer))))))

(defun eglot-codelens--visible-range (&optional extend-lines)
  "Calculate the visible line range for current buffer's window, with optional extension.

EXTEND-LINES, when a positive integer, extends the range by that many
lines in both directions (useful for pre-fetching).

Return a cons cell (BEG-LINE . END-LINE) where both are 1-based line numbers.
END-LINE may exceed the buffer's actual line count."
  (when-let* ((w (car (get-buffer-window-list)))
              (beg (window-start w))
              (end (window-end w t))
              (beg-line (line-number-at-pos beg t))
              (end-line (line-number-at-pos end t)))
    (if (and extend-lines (integerp extend-lines))
        (cons (max 1 (- beg-line extend-lines))
              ;; Overflow is safe here,
              ;; gethash handles non-existent keys gracefully.
              (+ end-line extend-lines))
      (cons beg-line end-line))))

(defun eglot-codelens--refresh-visible-area ()
  "Refresh CodeLens overlays in visible window area using existing cache.
This function efficiently updates only the visible portion of the buffer
without re-fetching CodeLens from the server."
  (when-let* ((_ (and eglot-codelens-mode
                      eglot-codelens--cache
                      (eq eglot-codelens--version (eglot-codelens--docver))))
              (range (eglot-codelens--visible-range))
              (docver eglot-codelens--version)
              (beg-line (car range))
              (end-line (cdr range))
              ;; Filter pending-lines to only those within visible range
              (pending-lines (when (and eglot-codelens--pending-lines
                                        (length> eglot-codelens--pending-lines 0))
                               (cl-loop for line in eglot-codelens--pending-lines
                                        when (and (>= line beg-line) (<= line end-line))
                                        collect line))))
    ;; Use existing cache - no new data, just refresh visible area
    (eglot-codelens--render-codelens
     eglot-codelens--cache docver pending-lines nil nil range)))

(defun eglot-codelens--setup-buffer ()
  "Setup CodeLens for current buffer."
  (when eglot-codelens-mode
    ;; Initialize buffer-local variables
    (setq eglot-codelens--cache nil
          eglot-codelens--prev-line-count nil
          eglot-codelens--version nil
          eglot-codelens--overlays nil)
    (add-hook 'after-change-functions #'eglot-codelens--collect-recent-changes 10 t)
    ;; Add Eglot document change hook
    (if (boundp 'eglot--send-changes-hook)
        (add-hook 'eglot--send-changes-hook #'eglot-codelens--on-document-change nil t)
      (add-hook 'eglot--document-changed-hook #'eglot-codelens--on-document-change nil t))
    ;; Add window scroll hook for visible area refresh
    (add-hook 'window-scroll-functions #'eglot-codelens--schedule-visible-refresh nil t)
    ;; Add window configuration change hook
    (add-hook 'window-configuration-change-hook #'eglot-codelens--schedule-visible-refresh nil t)))

(defun eglot-codelens--cleanup-buffer ()
  "Cleanup CodeLens for current buffer."
  ;; Cancel any pending update timer
  (when eglot-codelens--update-timer
    (cancel-timer eglot-codelens--update-timer)
    (setq eglot-codelens--update-timer nil))
  ;; Cancel any pending refresh timer
  (when eglot-codelens--refresh-timer
    (cancel-timer eglot-codelens--refresh-timer)
    (setq eglot-codelens--refresh-timer nil))
  ;; Cancel any pending resolve timer
  (when eglot-codelens--resolve-timer
    (cancel-timer eglot-codelens--resolve-timer)
    (setq eglot-codelens--resolve-timer nil))

  ;; Remove all overlays
  (eglot-codelens--cleanup-overlays)

  ;; Clear cache, version, queues, and overlay tracking
  (when (hash-table-p eglot-codelens--cache)
    (clrhash eglot-codelens--cache))
  (setq eglot-codelens--cache nil
        eglot-codelens--prev-line-count nil
        eglot-codelens--version nil
        eglot-codelens--pending-lines nil
        eglot-codelens--resolve-queue nil
        eglot-codelens--overlays nil
        eglot-codelens--recent-changes nil)

  ;; Remove hooks
  (remove-hook 'after-change-functions #'eglot-codelens--collect-recent-changes t)
  (if (boundp 'eglot--send-changes-hook)
      (remove-hook 'eglot--send-changes-hook #'eglot-codelens--on-document-change t)
    (remove-hook 'eglot--document-changed-hook #'eglot-codelens--on-document-change t))
  (remove-hook 'window-scroll-functions #'eglot-codelens--schedule-visible-refresh t)
  (remove-hook 'window-configuration-change-hook #'eglot-codelens--schedule-visible-refresh t))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode eglot-codelens-mode
  "Minor mode for displaying LSP CodeLens in Eglot."
  :group 'eglot-codelens
  (cond
   (eglot-codelens-mode
    (if (and (eglot-managed-p)
             (eglot-current-server)
             (eglot-server-capable :codeLensProvider))
        (progn
          (eglot-codelens--setup-buffer)
          (eglot-codelens--fetch-codelens))
      (eglot-codelens--cleanup-buffer)))
   (t
    (eglot-codelens--cleanup-buffer))))

(provide 'eglot-codelens)

;;; eglot-codelens.el ends here
