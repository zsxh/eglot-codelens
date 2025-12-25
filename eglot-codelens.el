;;; eglot-codelens.el --- CodeLens support for Eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Zxsh Chen

;; Version: 0.2
;; Author: Zxsh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/eglot-codelens
;; Keywords: eglot, codelens, tools
;; Package-Requires: ((emacs "29.1") (eglot "1.19"))

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

(defvar-local eglot-codelens--version nil
  "Document version for cached CodeLens.")

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

(defun eglot-codelens--build-cache (codelens-list)
  "Build cache from CODELENS-LIST.
Returns a hash table with line numbers as keys.
Each value is a sorted list of CODELENS-OVERLAY-CELL where CODELENS-OVERLAY-CELL is (CODELENS . OVERLAY)."
  (when codelens-list
    (let ((line-groups (make-hash-table :test 'eq)))
      ;; Group CodeLens by line number
      (cl-loop for codelens across codelens-list
               for range = (plist-get codelens :range)
               ;; Position in a text document expressed as zero-based line and zero-based character offset.
               for line = (1+ (plist-get (plist-get range :start) :line))
               do (push (cons codelens nil) (gethash line line-groups)))

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
      (eglot--async-request
       server :codeLens/resolve codelens
       :success-fn (lambda (resolved)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (eglot-codelens--update-resolved-codelens codelens-cell resolved))))
       :hint :codeLens/resolve))))

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
    (cl-delete-if (lambda (e)
                    (not (eq (car e) eglot-codelens--version)))
                  eglot-codelens--resolve-queue)
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
              (docver eglot--versioned-identifier)
              (buf (current-buffer)))
    (eglot--async-request
     server
     :textDocument/codeLens (list :textDocument (eglot--TextDocumentIdentifier))
     :success-fn (lambda (codelens-list)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (when (and eglot-codelens-mode
                                  (eq docver eglot--versioned-identifier))
                         ;; Save old cache before updating
                         (let ((old-cache eglot-codelens--cache)
                               (new-cache (eglot-codelens--build-cache codelens-list))
                               (range (eglot-codelens--visible-range))
                               all-lines)
                           ;; Initialize pending-lines with all lines from new cache
                           (when new-cache
                             (maphash (lambda (line _) (push line all-lines)) new-cache))
                           ;; Reuse existing overlays for optimal performance
                           ;; Pass pending-lines to track which lines need processing
                           (eglot-codelens--render-codelens
                            new-cache docver all-lines old-cache range)
                           (setq eglot-codelens--cache new-cache
                                 eglot-codelens--version docver
                                 eglot-codelens--pending-lines all-lines))))))
     :hint :textDocument/codeLens)))

;;; UI Display System

(defun eglot-codelens--build-display-string (codelens-cell line-start index total-codelens)
  "Build display string for CODELENS-CELL at LINE-START with INDEX and TOTAL-CODELENS.
CODELENS-CELL is a cons cell (CODELENS . OVERLAY)."
  (let* ((codelens (car codelens-cell))
         (is-first (= index 0))
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

(defun eglot-codelens--format-text (codelens-cell)
  "Format display text for CODELENS-CELL.
CODELENS-CELL is a cons cell (CODELENS . OVERLAY)."
  (let* ((codelens (car codelens-cell))
         (ov (cdr codelens-cell))
         (command (or (plist-get codelens :command)
                      (when (and ov (overlayp ov) (overlay-buffer ov))
                        (overlay-get ov 'eglot-codelens-command))))
         (title (when command (plist-get command :title))))
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
  (let* ((codelens (car codelens-cell))
         (ov (cdr codelens-cell))
         (command (plist-get resolved :command)))
    (when (and eglot-codelens-mode
               ov (overlayp ov) (overlay-buffer ov)
               (eq (overlay-get ov 'eglot-codelens-docver) eglot-codelens--version))
      ;; Update cache with resolved codelens
      (cl-remf codelens :data)
      (plist-put codelens :command command)

      ;; Update the display string
      (let* ((line-start (overlay-start ov))
             (line (line-number-at-pos line-start))
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

(defun eglot-codelens--render-codelens (new-cache docver pending-lines &optional old-cache range)
  "Render CodeLens overlays by reusing overlays from OLD-CACHE.
NEW-CACHE is a hash table of the new cache to render (line -> sorted list of cells).
DOCVER is the document version for tracking overlay validity.
PENDING-LINES is a list of line numbers to process.

Optional argument OLD-CACHE is the previous cache for overlay reuse.
When provided, existing overlays are reused where possible.

Optional argument RANGE is a cons cell (BEG-LINE . END-LINE) specifying the line
number range to render. When provided, only CodeLens within this line range
are updated. Existing overlays in the range that already have DOCVER
are preserved unchanged.

This function efficiently updates overlays.
Overlays are matched by index position within each line."
  (with-silent-modifications
    (save-excursion
      ;; Step 1: Process pending lines
      (let ((lines-to-process pending-lines)
            (lines-processed nil)
            (resolve-queue nil))
        (dolist (line lines-to-process)
          (let* ((new-sorted (gethash line new-cache)))
            (when new-sorted
              (let* ((line-start (progn
                                   (goto-char (point-min))
                                   (forward-line (1- line))
                                   (line-beginning-position)))
                     (in-range-p (or (not range)
                                     (and (>= line (car range))
                                          (<= line (cdr range)))))
                     (total-on-line (length new-sorted))
                     ;; Find corresponding line group in old cache
                     (old-sorted (when old-cache
                                   (gethash line old-cache))))

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
                               (overlay-put new-ov 'eglot-codelens-docver docver)))

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
                             (overlay-put old-ov 'eglot-codelens-usever docver))

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
                  (push line lines-processed))))))

        ;; Step 2: Delete overlays with old usever (entire buffer)
        (when old-cache
          (dolist (ov (overlays-in (point-min) (point-max)))
            (when (and (overlay-get ov 'eglot-codelens)
                       (not (eq (overlay-get ov 'eglot-codelens-usever) docver)))
              (delete-overlay ov))))

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
  (let* ((line (line-number-at-pos))
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

(defun eglot-codelens--setup-buffer ()
  "Setup CodeLens for current buffer."
  (when eglot-codelens-mode
    ;; Initialize buffer-local variables
    (setq eglot-codelens--cache nil
          eglot-codelens--version nil)
    ;; Add Eglot document change hook
    (add-hook 'eglot--document-changed-hook #'eglot-codelens--on-document-change nil t)
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

  ;; Clear cache, version, and queues
  (setq eglot-codelens--cache nil
        eglot-codelens--version nil
        eglot-codelens--pending-lines nil
        eglot-codelens--resolve-queue nil)

  ;; Remove Eglot document change hook
  (remove-hook 'eglot--document-changed-hook #'eglot-codelens--on-document-change t)
  ;; Remove window scroll hook
  (remove-hook 'window-scroll-functions #'eglot-codelens--schedule-visible-refresh t)
  ;; Remove window configuration change hook
  (remove-hook 'window-configuration-change-hook #'eglot-codelens--schedule-visible-refresh t))

(defun eglot-codelens--schedule-visible-refresh (&rest _args)
  "Handle window scroll/resize to refresh visible CodeLens with debouncing."
  (when eglot-codelens-mode
    ;; If there's already a timer, just reset its time
    (if (timerp eglot-codelens--refresh-timer)
        ;; Reset existing timer's time
        (timer-set-time eglot-codelens--refresh-timer eglot-codelens-visible-refresh-delay)
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
                   (when (eq (current-buffer) (window-buffer (selected-window)))
                     (eglot-codelens--refresh-visible-area)))))
             (current-buffer))))))

(defun eglot-codelens--on-document-change (&rest _args)
  "Handle document changes via Eglot's document-changed hook."
  (when eglot-codelens-mode
    ;; If there's already a timer, just reset its time instead of canceling and recreating
    (if (timerp eglot-codelens--update-timer)
        ;; Reset existing timer's time
        (timer-set-time eglot-codelens--update-timer eglot-codelens-update-delay)
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
  "Calculate the visible range with optional extension.
EXTEND-LINES is the number of lines to extend beyond the visible area
both before and after. If nil or not provided, returns the exact visible range.
Returns a cons cell (BEG-LINE . END-LINE) representing line numbers."
  (let* ((beg (window-start))
         (end (window-end nil t))
         (beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end)))
    (if (and extend-lines (integerp extend-lines) (> extend-lines 0))
        (cons (max 1 (- beg-line extend-lines))
              (min (line-number-at-pos (point-max))
                   (+ end-line extend-lines)))
      (cons beg-line end-line))))

(defun eglot-codelens--refresh-visible-area ()
  "Refresh CodeLens overlays in visible window area using existing cache.
This function efficiently updates only the visible portion of the buffer
without re-fetching CodeLens from the server."
  (when (and eglot-codelens-mode
             eglot-codelens--cache
             (eq eglot-codelens--version eglot--versioned-identifier))
    (let* ((docver eglot-codelens--version)
           (range (eglot-codelens--visible-range))
           (beg-line (car range))
           (end-line (cdr range))
           ;; Filter pending-lines to only those within visible range
           (pending-lines (cl-loop for line in eglot-codelens--pending-lines
                                   when (and (>= line beg-line) (<= line end-line))
                                   collect line)))
      ;; Use existing cache - no new data, just refresh visible area
      (eglot-codelens--render-codelens
       eglot-codelens--cache docver pending-lines nil range))))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode eglot-codelens-mode
  "Minor mode for displaying LSP CodeLens in Eglot."
  :group 'eglot-codelens
  (cond
   (eglot-codelens-mode
    (if (and eglot--managed-mode (eglot-server-capable :codeLensProvider))
        (progn
          (eglot-codelens--setup-buffer)
          (eglot-codelens--fetch-codelens))
      (eglot-codelens--cleanup-buffer)))
   (t
    (eglot-codelens--cleanup-buffer))))

(provide 'eglot-codelens)

;;; eglot-codelens.el ends here
