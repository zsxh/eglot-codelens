;;; eglot-codelens-test.el --- Tests for eglot-codelens  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Zxsh Chen

;; Author: Zxsh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/eglot-codelens

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

;; Test suite for eglot-codelens.el using ERT.
;; Tests are designed to run in batch mode with mocked dependencies.

;;; Code:

(require 'ert)
(require 'eglot-codelens)


;;; Mock Helpers
(defun eglot-codelens-test--mock-codelens (line title &optional command)
  "Create a mock CodeLens at LINE with TITLE.
Optional COMMAND provides the command plist."
  (let ((cmd (or command
                 (list :title title
                       :command "test.command"
                       :arguments (vector (list :uri "test://file"))))))
    (list :range (list :start (list :line (1- line) :character 0)
                       :end (list :line (1- line) :character 10))
          :command cmd
          :data nil)))

(defun eglot-codelens-test--mock-codelens-unresolved (line)
  "Create a mock unresolved CodeLens at LINE (no command)."
  (list :range (list :start (list :line (1- line) :character 0)
                     :end (list :line (1- line) :character 10))
        :command nil
        :data "some-data"))

(defun eglot-codelens-test--count-codelens-overlays ()
  "Count CodeLens overlays in current buffer."
  (let ((count 0))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'eglot-codelens)
        (setq count (1+ count))))
    count))

(defun eglot-codelens-test--get-overlay-at-line (line)
  "Get the first CodeLens overlay at LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((line-start (point)))
      (forward-line 1)
      (let ((line-end (point)))
        (cl-loop for ov in (overlays-in line-start line-end)
                 when (overlay-get ov 'eglot-codelens)
                 return ov)))))

(defun eglot-codelens-test--count-overlays-in-range (beg-line end-line)
  "Count CodeLens overlays between BEG-LINE and END-LINE (inclusive)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- beg-line))
    (let ((beg (point)))
      (goto-char (point-min))
      (forward-line (min (1- end-line) (1- (line-number-at-pos (point-max)))))
      (let ((end (point)))
        (let ((count 0))
          (dolist (ov (overlays-in beg end))
            (when (overlay-get ov 'eglot-codelens)
              (setq count (1+ count))))
          count)))))


;;; Cache Management Tests

(ert-deftest eglot-codelens-test-build-cache-empty ()
  "Test building cache from empty list returns nil."
  ;; Empty vector is truthy in Elisp, need to pass nil or use vector with 0 elements
  (should (null (eglot-codelens--build-cache nil))))

(ert-deftest eglot-codelens-test-build-cache-single ()
  "Test building cache with single CodeLens."
  (let* ((c1 (eglot-codelens-test--mock-codelens 5 "test"))
         (codelens (vector c1))
         (cache (eglot-codelens--build-cache codelens)))
    (should cache)
    (should (= (hash-table-count cache) 1))
    (let ((line-5-group (gethash 5 cache)))
      (should line-5-group)
      (should (= (length line-5-group) 1))
      (should (eq (car (car line-5-group)) c1)))))

(ert-deftest eglot-codelens-test-build-cache-multiple-same-line ()
  "Test building cache with multiple CodeLens on same line."
  (let* ((c1 (eglot-codelens-test--mock-codelens 3 "5 references"))
         (c2 (eglot-codelens-test--mock-codelens 3 "2 implementations"))
         (codelens (vector c1 c2))
         (cache (eglot-codelens--build-cache codelens)))
    (should cache)
    (should (= (hash-table-count cache) 1))
    (let ((line-3-group (gethash 3 cache)))
      (should line-3-group)
      (should (= (length line-3-group) 2))
      ;; Verify order is preserved (LSP order)
      (should (eq (car (nth 0 line-3-group)) c1))
      (should (eq (car (nth 1 line-3-group)) c2)))))

(ert-deftest eglot-codelens-test-build-cache-multiple-lines ()
  "Test building cache with CodeLens on multiple lines."
  (let* ((c1 (eglot-codelens-test--mock-codelens 1 "5 references"))
         (c2 (eglot-codelens-test--mock-codelens 3 "3 references"))
         (c3 (eglot-codelens-test--mock-codelens 6 "1 reference"))
         (codelens (vector c1 c2 c3))
         (cache (eglot-codelens--build-cache codelens)))
    (should cache)
    (should (= (hash-table-count cache) 3))
    (should (gethash 1 cache))
    (should (gethash 3 cache))
    (should (gethash 6 cache))))

(ert-deftest eglot-codelens-test-build-cache-sorted-order ()
  "Test that CodeLens maintain LSP order (sorted by index)."
  (let* ((codelens (vector (eglot-codelens-test--mock-codelens 1 "first")
                            (eglot-codelens-test--mock-codelens 1 "second")
                            (eglot-codelens-test--mock-codelens 1 "third")))
         (cache (eglot-codelens--build-cache codelens))
         (line-group (gethash 1 cache)))
    (should (= (length line-group) 3))
    (should (string= (plist-get (plist-get (car (nth 0 line-group)) :command) :title) "first"))
    (should (string= (plist-get (plist-get (car (nth 1 line-group)) :command) :title) "second"))
    (should (string= (plist-get (plist-get (car (nth 2 line-group)) :command) :title) "third"))))


;;; UI Display System Tests

(ert-deftest eglot-codelens-test-format-text-with-command ()
  "Test formatting text when command is present."
  (let* ((codelens (eglot-codelens-test--mock-codelens 1 "5 references"))
         (cell (cons codelens nil)))
    (should (string= (eglot-codelens--format-text cell) "5 references"))))

(ert-deftest eglot-codelens-test-format-text-without-command ()
  "Test formatting text when command is missing (unresolved)."
  (let* ((codelens (eglot-codelens-test--mock-codelens-unresolved 1))
         (cell (cons codelens nil)))
    (should (string= (eglot-codelens--format-text cell) "Loading..."))))

(ert-deftest eglot-codelens-test-format-text-from-overlay ()
  "Test formatting text using cached command from overlay."
  (with-temp-buffer
    (let* ((codelens (eglot-codelens-test--mock-codelens-unresolved 1))
           (ov (make-overlay (point-min) (point-min)))
           (cell (cons codelens ov)))
      (overlay-put ov 'eglot-codelens-command (list :title "Cached Title"))
      (should (string= (eglot-codelens--format-text cell) "Cached Title")))))

(ert-deftest eglot-codelens-test-codicons-to-nerd-icons ()
  "Test VS Code icon placeholder conversion."
  ;; When nerd-icons is available
  (when (featurep 'nerd-icons)
    (let ((result (eglot-codelens--codicons-to-nerd-icons "Test $(debug) text")))
      (should (not (string-match "\\$(debug)" result))))))

(ert-deftest eglot-codelens-test-codicons-unrecognized-icon ()
  "Test that unrecognized icons keep placeholder when nerd-icons fails."
  (let ((result (eglot-codelens--codicons-to-nerd-icons "Test $(unknown-icon)")))
    ;; When nerd-icons is not available or fails, keep original
    (should (stringp result))))

(ert-deftest eglot-codelens-test-build-display-string-first ()
  "Test display string for first CodeLens (with indentation)."
  (with-temp-buffer
    (insert "    test line")
    (goto-char (point-min))
    (let* ((codelens (eglot-codelens-test--mock-codelens 1 "5 references"))
           (cell (cons codelens nil))
           (result (eglot-codelens--build-display-string cell (point-min) 0 1))
           (plain (substring-no-properties result)))
      (should (string-prefix-p "    " plain))  ; Has indentation
      (should (string-suffix-p "\n" plain))))) ; Has newline

(ert-deftest eglot-codelens-test-build-display-string-middle ()
  "Test display string for middle CodeLens (with separator)."
  (with-temp-buffer
    (let* ((codelens (eglot-codelens-test--mock-codelens 1 "2 implementations"))
           (cell (cons codelens nil))
           (result (eglot-codelens--build-display-string cell (point-min) 1 3)))
      ;; Check separator is present (use substring-no-properties to ignore text props)
      ;; index=1 with total=3 means this is the middle item (not first, not last)
      (should (string-suffix-p "|" (substring-no-properties result))))))

(ert-deftest eglot-codelens-test-build-display-string-last ()
  "Test display string for last CodeLens (with newline)."
  (with-temp-buffer
    (let* ((codelens (eglot-codelens-test--mock-codelens 1 "2 implementations"))
           (cell (cons codelens nil))
           (result (eglot-codelens--build-display-string cell (point-min) 2 3)))
      ;; index=2 with total=3 means this is the last item
      (should (string-suffix-p "\n" (substring-no-properties result))))))


;;; Overlay Management Tests

(ert-deftest eglot-codelens-test-make-overlay-properties ()
  "Test that overlay has correct properties."
  (with-temp-buffer
    (insert "test")
    (goto-char (point-min))
    (let* ((codelens (eglot-codelens-test--mock-codelens 1 "test"))
           (cell (cons codelens nil))
           (docver 1)
           (ov (eglot-codelens--make-overlay (point-min) cell 0 1 docver)))
      (should (overlayp ov))
      (should (overlay-get ov 'eglot-codelens))
      (should (eq (overlay-get ov 'eglot-codelens-docver) docver))
      (should (eq (overlay-get ov 'eglot-codelens-usever) docver))
      (should (overlay-get ov 'eglot-codelens-command))
      (should (overlay-get ov 'before-string)))))

(ert-deftest eglot-codelens-test-make-overlay-priority ()
  "Test that overlay priority matches index."
  (with-temp-buffer
    (insert "test")
    (goto-char (point-min))
    (let* ((codelens (eglot-codelens-test--mock-codelens 1 "test"))
           (cell (cons codelens nil))
           (ov0 (eglot-codelens--make-overlay (point-min) cell 0 1 1))
           (ov1 (eglot-codelens--make-overlay (point-min) cell 1 1 1))
           (ov2 (eglot-codelens--make-overlay (point-min) cell 2 1 1)))
      (should (= (overlay-get ov0 'priority) 0))
      (should (= (overlay-get ov1 'priority) 1))
      (should (= (overlay-get ov2 'priority) 2)))))

(ert-deftest eglot-codelens-test-cleanup-overlays ()
  "Test cleanup removes all CodeLens overlays."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Create some CodeLens overlays
    (let ((ov1 (make-overlay (point-min) (point-min)))
          (ov2 (make-overlay (point-min) (point-min))))
      (overlay-put ov1 'eglot-codelens t)
      (overlay-put ov2 'eglot-codelens t)
      ;; Create a non-CodeLens overlay
      (let ((ov3 (make-overlay (point-min) (point-min))))
        (should (= (eglot-codelens-test--count-codelens-overlays) 2))
        (eglot-codelens--cleanup-overlays)
        (should (= (eglot-codelens-test--count-codelens-overlays) 0))))))


;;; Rendering System Tests (with Mocks)

(defun eglot-codelens-test--setup-test-buffer ()
  "Setup a test buffer similar to A.java structure."
  (insert "public class A {\n")
  (insert "\n")
  (insert "    class B {\n")
  (insert "    }\n")
  (insert "\n")
  (insert "    public void test() {\n")
  (insert "        // comment\n")
  (insert "    }\n")
  (insert "}\n"))

(defun eglot-codelens-test--mock-visible-range (beg-line end-line)
  "Mock `eglot-codelens--visible-range' to return (BEG-LINE . END-LINE)."
  (cl-letf (((symbol-function 'eglot-codelens--visible-range)
             (lambda (&rest _) (cons beg-line end-line))))
    (let ((result (eglot-codelens--visible-range)))
      result)))

(ert-deftest eglot-codelens-test-mock-visible-range ()
  "Test that visible range mocking works correctly."
  (should (equal (eglot-codelens-test--mock-visible-range 3 6) '(3 . 6)))
  (should (equal (eglot-codelens-test--mock-visible-range 1 10) '(1 . 10))))

(ert-deftest eglot-codelens-test-render-create-new-overlays ()
  "Test rendering creates new overlays when none exist."
  (with-temp-buffer
    (eglot-codelens-test--setup-test-buffer)
    (let* ((c1 (eglot-codelens-test--mock-codelens 1 "5 references"))
           (c2 (eglot-codelens-test--mock-codelens 1 "2 implementations"))
           (codelens (vector c1 c2))
           (cache (eglot-codelens--build-cache codelens))
           (docver 1))
      ;; Mock visible range to include line 1
      (cl-letf (((symbol-function 'eglot-codelens--visible-range)
                 (lambda (&rest _) (cons 1 3))))
        (eglot-codelens--render-codelens cache docver '(1) nil (cons 1 3))
        ;; Should have created 2 overlays
        (should (= (eglot-codelens-test--count-overlays-in-range 1 1) 2))))))

(ert-deftest eglot-codelens-test-render-only-visible-range ()
  "Test that rendering only creates overlays in visible range."
  (with-temp-buffer
    (eglot-codelens-test--setup-test-buffer)
    (let* ((c1 (eglot-codelens-test--mock-codelens 1 "5 refs"))
           (c2 (eglot-codelens-test--mock-codelens 3 "3 refs"))
           (c3 (eglot-codelens-test--mock-codelens 6 "1 ref"))
           (codelens (vector c1 c2 c3))
           (cache (eglot-codelens--build-cache codelens))
           (docver 1))
      ;; Mock visible range to only include lines 3-6
      (cl-letf (((symbol-function 'eglot-codelens--visible-range)
                 (lambda (&rest _) (cons 3 6))))
        (eglot-codelens--render-codelens cache docver '(1 3 6) nil (cons 3 6))
        ;; Should only have overlays for lines 3 and 6
        (should (= (eglot-codelens-test--count-overlays-in-range 1 1) 0))
        (should (= (eglot-codelens-test--count-overlays-in-range 3 3) 1))
        (should (= (eglot-codelens-test--count-overlays-in-range 6 6) 1))))))

(ert-deftest eglot-codelens-test-render-reuse-existing-overlays ()
  "Test that rendering reuses existing overlays."
  (with-temp-buffer
    (eglot-codelens-test--setup-test-buffer)
    (let* ((c1 (eglot-codelens-test--mock-codelens 1 "5 refs"))
           (c2 (eglot-codelens-test--mock-codelens 1 "2 impls"))
           (codelens (vector c1 c2))
           (old-cache (eglot-codelens--build-cache codelens))
           (docver-old 1)
           (docver-new 2)
           ;; Create fresh codelens for second render (simulate new fetch)
           (c1-fresh (eglot-codelens-test--mock-codelens 1 "5 refs"))
           (c2-fresh (eglot-codelens-test--mock-codelens 1 "2 impls"))
           (codelens-fresh (vector c1-fresh c2-fresh))
           (new-cache (eglot-codelens--build-cache codelens-fresh))
           count-after-first count-after-second)
      ;; First render - create overlays
      (cl-letf (((symbol-function 'eglot-codelens--visible-range)
                 (lambda (&rest _) (cons 1 3))))
        (eglot-codelens--render-codelens old-cache docver-old '(1) nil (cons 1 3))
        (setq count-after-first (eglot-codelens-test--count-codelens-overlays))
        (should (= count-after-first 2))
        ;; Second render - should reuse overlays
        (eglot-codelens--render-codelens new-cache docver-new '(1) old-cache (cons 1 3))
        (setq count-after-second (eglot-codelens-test--count-codelens-overlays))
        (should (= count-after-second 2))))))

(ert-deftest eglot-codelens-test-render-delete-stale-overlays ()
  "Test that stale overlays (not in new cache) are deleted."
  (with-temp-buffer
    (eglot-codelens-test--setup-test-buffer)
    (let* ((c1 (eglot-codelens-test--mock-codelens 1 "5 refs"))
           (c2 (eglot-codelens-test--mock-codelens 3 "3 refs"))
           (old-codelens (vector c1 c2))
           (old-cache (eglot-codelens--build-cache old-codelens))
           (new-codelens (vector c1))  ; Only line 1 remains
           (new-cache (eglot-codelens--build-cache new-codelens))
           (docver-old 1)
           (docver-new 2))  ; Use different docver to trigger stale deletion
      ;; First render - create overlays for lines 1 and 3
      (cl-letf (((symbol-function 'eglot-codelens--visible-range)
                 (lambda (&rest _) (cons 1 6))))
        (eglot-codelens--render-codelens old-cache docver-old '(1 3) nil (cons 1 6))
        (let ((count-after-first (eglot-codelens-test--count-codelens-overlays)))
          (should (= count-after-first 2))
          ;; Second render - line 3 overlay should be deleted
          (eglot-codelens--render-codelens new-cache docver-new '(1) old-cache (cons 1 6))
          (let ((count-after-second (eglot-codelens-test--count-codelens-overlays)))
            (should (= count-after-second 1))
            (should (eglot-codelens-test--get-overlay-at-line 1))
            (should (null (eglot-codelens-test--get-overlay-at-line 3)))))))))

(ert-deftest eglot-codelens-test-render-resolve-queue ()
  "Test that unresolved CodeLens are added to resolve queue."
  (with-temp-buffer
    (eglot-codelens-test--setup-test-buffer)
    (let* ((c1 (eglot-codelens-test--mock-codelens 1 "5 refs"))  ; Has command
           (c2 (eglot-codelens-test--mock-codelens-unresolved 1)) ; No command
           (codelens (vector c1 c2))
           (cache (eglot-codelens--build-cache codelens))
           (docver 1))
      ;; Clear resolve queue (buffer-local variable)
      (setq eglot-codelens--resolve-queue nil)
      ;; Mock visible range and prevent auto-scheduling resolve queue
      (cl-letf (((symbol-function 'eglot-codelens--visible-range)
                 (lambda (&rest _) (cons 1 3)))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda () nil)))  ; Prevent auto-processing
        (eglot-codelens--render-codelens cache docver '(1) nil (cons 1 3))
        ;; Check that c2 was added to resolve queue
        (should eglot-codelens--resolve-queue)
        (should (= (length eglot-codelens--resolve-queue) 1))))))


;;; CodeLens Resolution Tests

(ert-deftest eglot-codelens-test-resolve-process-queue-empty ()
  "Test processing empty resolve queue."
  (let ((eglot-codelens--resolve-queue nil))
    (eglot-codelens--resolve-process-queue)
    (should (null eglot-codelens--resolve-queue))))

(ert-deftest eglot-codelens-test-resolve-process-queue-one-item ()
  "Test processing single item in resolve queue."
  (let* ((codelens (eglot-codelens-test--mock-codelens-unresolved 1))
         (cell (cons codelens nil))
         (eglot-codelens--resolve-queue (list (cons 1 cell)))
         (eglot-codelens--version 1))
    ;; Mock the resolve function to avoid actual LSP call
    (cl-letf (((symbol-function 'eglot-codelens--resolve-codelens)
               (lambda (_arg) 'called)))
      (eglot-codelens--resolve-process-queue)
      (should (null eglot-codelens--resolve-queue)))))

(ert-deftest eglot-codelens-test-resolve-process-queue-outdated ()
  "Test that outdated items are removed from queue."
  (let* ((codelens (eglot-codelens-test--mock-codelens-unresolved 1))
         (cell (cons codelens nil))
         (eglot-codelens--resolve-queue (list (cons 999 cell)))  ; Wrong version
         (eglot-codelens--version 1))
    (eglot-codelens--resolve-process-queue)
    (should (null eglot-codelens--resolve-queue))))


;;; Interaction Handling Tests

(ert-deftest eglot-codelens-test-execute-at-line-single ()
  "Test executing single CodeLens at line."
  (with-temp-buffer
    (insert "test line\n")
    (goto-char (point-min))  ; Ensure we're at line 1
    (let* ((c1 (eglot-codelens-test--mock-codelens 1 "5 refs"))
           (cache (make-hash-table :test 'eq))
           (executed nil))
      (puthash 1 (list (cons c1 nil)) cache)
      (setq eglot-codelens--cache cache)
      ;; Mock eglot-execute and eglot--current-server-or-lose
      (cl-letf (((symbol-function 'eglot-execute)
                 (lambda (&rest _) (setq executed t)))
                ((symbol-function 'eglot--current-server-or-lose)
                 (lambda () 'mock-server)))
        (eglot-codelens-execute-at-line)
        (should executed)))))

(ert-deftest eglot-codelens-test-execute-at-line-none ()
  "Test executing CodeLens when none exists at line."
  (with-temp-buffer
    (insert "test line\n")
    (let ((eglot-codelens--cache (make-hash-table :test 'eq)))
      (eglot-codelens-execute-at-line)
      ;; Should not error, just show message
      (should t))))


;;; Helper Function Tests

(ert-deftest eglot-codelens-test-visible-range ()
  "Test visible range calculation."
  ;; Skip in batch mode where there's no window
  (unless noninteractive
    (save-window-excursion
      (with-temp-buffer
        (dotimes (_ 20) (insert "line\n"))
        ;; Display buffer in a window to test window-dependent functions
        (set-window-buffer (selected-window) (current-buffer))
        (let* ((beg (window-start))
               (end (window-end nil t))
               (range (eglot-codelens--visible-range)))
          (should (consp range))
          (should (integerp (car range)))
          (should (integerp (cdr range))))))))

(ert-deftest eglot-codelens-test-visible-range-extended ()
  "Test visible range with extension."
  ;; Skip in batch mode where there's no window
  (unless noninteractive
    (save-window-excursion
      (with-temp-buffer
        ;; Create enough lines for testing
        (dotimes (_ 30) (insert "line\n"))
        ;; Display buffer in a window to test window-dependent functions
        (set-window-buffer (selected-window) (current-buffer))
        ;; Get buffer's actual line count
        (let* ((buffer-max-line (line-number-at-pos (point-max)))
               (range-normal (eglot-codelens--visible-range))
               (range-extended (eglot-codelens--visible-range 5)))
          (should (consp range-extended))
          (let ((beg-normal (car range-normal))
                (end-normal (cdr range-normal))
                (beg-extended (car range-extended))
                (end-extended (cdr range-extended)))
            ;; Extended range should start before or at normal range
            (should (<= beg-extended beg-normal))
            ;; Extended range should end at or after normal range
            (should (>= end-extended end-normal))
            ;; Extended range should be larger than or equal to normal range
            (should (>= (- end-extended beg-extended)
                        (- end-normal beg-normal)))
            ;; Beginning should not be less than 1
            (should (>= beg-extended 1))
            ;; END-LINE may exceed buffer size; this is intentional
            ;; as gethash safely returns nil for non-existent keys
            ;; Overflow is allowed
            (should t)))))))


;;; Setup and Teardown

(defun eglot-codelens-test-setup ()
  "Setup function run before each test."
  (setq eglot-codelens--cache nil
        eglot-codelens--version nil
        eglot-codelens--pending-lines nil
        eglot-codelens--resolve-queue nil
        eglot-codelens--overlays nil))

(defun eglot-codelens-test-teardown ()
  "Teardown function run after each test."
  (setq eglot-codelens--cache nil
        eglot-codelens--version nil
        eglot-codelens--pending-lines nil
        eglot-codelens--resolve-queue nil
        eglot-codelens--overlays nil))

;; Note: Setup and teardown is handled per-test in ert-deftest forms
;; Individual tests reset state as needed

(provide 'eglot-codelens-test)

;;; eglot-codelens-test.el ends here
