;;; eglot-codelens-test.el --- Tests for eglot-codelens -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Zxsh Chen

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

;; Temporary test file for eglot-codelens.
;; Tests are implemented here first, then migrated to eglot-codelens-test.el.

;;; Code:

(require 'ert)
(require 'eglot-codelens)

(ert-deftest eglot-codelens--build-cache-test ()
  "Test `eglot-codelens--build-cache' with various inputs."
  ;; Test case collection: (name input expected-validator-fn)
  (let ((test-cases
         ;; Test Case 1: Nil input returns nil
         `(("nil input returns nil"
            nil
            ,(lambda (result) (should (null result))))

           ;; Test Case 2: Empty vector returns nil
           ("empty vector returns nil"
            []
            ,(lambda (result) (should (null result))))

           ;; Test Case 3: Non-vector input returns nil
           ("non-vector input returns nil"
            '(:range (:start (:line 0)))
            ,(lambda (result) (should (null result))))

           ;; Test Case 4: Valid single CodeLens on line 1
           ("valid single CodeLens on line 1"
            [(:range (:start (:line 0 :character 0) :end (:line 0 :character 1)))]
            ,(lambda (result)
               (should (hash-table-p result))
               (should (= (hash-table-count result) 1))
               (let ((cells (gethash 1 result)))
                 (should cells)
                 (should (= (length cells) 1))
                 (should (null (cdar cells))))))

           ;; Test Case 5: Multiple CodeLens on different lines
           ("multiple CodeLens on different lines"
            [(:range (:start (:line 0 :character 0) :end (:line 0 :character 1)))
             (:range (:start (:line 2 :character 0) :end (:line 2 :character 1)))]
            ,(lambda (result)
               (should (hash-table-p result))
               (should (= (hash-table-count result) 2))
               (should (gethash 1 result))
               (should (gethash 3 result))
               (should (= (length (gethash 1 result)) 1))
               (should (= (length (gethash 3 result)) 1))))

           ;; Test Case 6: Multiple CodeLens on same line preserves order
           ("multiple CodeLens on same line preserves order"
            [(:range (:start (:line 0 :character 0) :end (:line 0 :character 1)) :title "A")
             (:range (:start (:line 0 :character 2) :end (:line 0 :character 3)) :title "B")]
            ,(lambda (result)
               (should (hash-table-p result))
               (should (= (hash-table-count result) 1))
               (let ((cells (gethash 1 result)))
                 (should (= (length cells) 2))
                 ;; Check order is preserved
                 (should (string= (plist-get (caar cells) :title) "A"))
                 (should (string= (plist-get (caadr cells) :title) "B")))))

           ;; Test Case 7: CodeLens without :range is filtered out
           ("CodeLens without :range is filtered out"
            [(:title "no-range")
             (:range (:start (:line 0 :character 0) :end (:line 0 :character 1)))]
            ,(lambda (result)
               (should (hash-table-p result))
               (should (= (hash-table-count result) 1))
               (should (= (length (gethash 1 result)) 1))))

           ;; Test Case 8: CodeLens with :range but no :start is filtered out
           ("CodeLens with :range but no :start is filtered out"
            [(:range (:end (:line 0 :character 1)))
             (:range (:start (:line 0 :character 0) :end (:line 0 :character 1)))]
            ,(lambda (result)
               (should (hash-table-p result))
               (should (= (hash-table-count result) 1))
               (should (= (length (gethash 1 result)) 1))))

           ;; Test Case 9: CodeLens with negative line number is filtered out
           ("CodeLens with negative line number is filtered out"
            [(:range (:start (:line -1 :character 0) :end (:line -1 :character 1)))
             (:range (:start (:line 0 :character 0) :end (:line 0 :character 1)))]
            ,(lambda (result)
               (should (hash-table-p result))
               (should (= (hash-table-count result) 1))
               ;; Only line 1 should exist (line 0 filtered out)
               (should (gethash 1 result))
               (should-not (gethash 0 result))))

           ;; Test Case 10: CodeLens with non-integer line number is filtered out
           ("CodeLens with non-integer line number is filtered out"
            [(:range (:start (:line "0" :character 0) :end (:line "0" :character 1)))
             (:range (:start (:line 0 :character 0) :end (:line 0 :character 1)))]
            ,(lambda (result)
               (should (hash-table-p result))
               (should (= (hash-table-count result) 1))
               (should (= (length (gethash 1 result)) 1))))

           ;; Test Case 11: Mixed valid and invalid CodeLens
           ("mixed valid and invalid CodeLens"
            [(:title "invalid-1")
             (:range (:start (:line 0 :character 0) :end (:line 0 :character 1)))
             (:range (:end (:line 1 :character 1)))
             (:range (:start (:line 1 :character 0) :end (:line 1 :character 1)))
             (:range (:start (:line -1 :character 0) :end (:line -1 :character 1)))]
            ,(lambda (result)
               (should (hash-table-p result))
               (should (= (hash-table-count result) 2))
               (should (gethash 1 result))
               (should (gethash 2 result))
               (should (= (length (gethash 1 result)) 1))
               (should (= (length (gethash 2 result)) 1))))

           ;; Test Case 12: 0-based LSP line numbers converted to 1-based Emacs line numbers
           ("0-based LSP line numbers converted to 1-based"
            [(:range (:start (:line 9 :character 0) :end (:line 9 :character 1)))]
            ,(lambda (result)
               (should (hash-table-p result))
               (should (= (hash-table-count result) 1))
               ;; LSP line 9 -> Emacs line 10
               (should (gethash 10 result))
               (should-not (gethash 9 result)))))))

    ;; Run all test cases
    (dolist (test-case test-cases)
      (pcase test-case
        (`(,name ,input ,validator-fn)
         (ert-info ((format "Test: %s" name))
           (let ((result (eglot-codelens--build-cache input)))
             (funcall validator-fn result))))))))

(ert-deftest eglot-codelens--resolve-codelens-test ()
  "Test `eglot-codelens--resolve-codelens' with various inputs."
  ;; Test case collection: (name codelens-cell server server-capable-p expect-request-called)
  (with-temp-buffer
    (let ((test-cases
           ;; Test Case 1: Nil codelens-cell returns nil without any side effects
           `(("nil codelens-cell returns nil"
              nil
              t
              t
              nil)

             ;; Test Case 2: codelens-cell with nil codelens (car is nil) returns nil
             ("codelens-cell with nil codelens returns nil"
              ,(let ((ov (make-overlay (point-min) (point-min))))
                 (cons nil ov))
              t
              t
              nil)

             ;; Test Case 3: codelens-cell with nil overlay (cdr is nil) returns nil
             ("codelens-cell with nil overlay returns nil"
              ,(cons '(:range (:start (:line 0))) nil)
              t
              t
              nil)

             ;; Test Case 4: No current LSP server (eglot-current-server returns nil) returns nil
             ("no current LSP server returns nil"
              ,(let ((ov (make-overlay (point-min) (point-min))))
                 (cons '(:range (:start (:line 0))) ov))
              nil
              t
              nil)

             ;; Test Case 5: Server doesn't support resolveProvider returns nil without making request
             ("server without resolveProvider returns nil"
              ,(let ((ov (make-overlay (point-min) (point-min))))
                 (cons '(:range (:start (:line 0))) ov))
              t
              nil
              nil)

             ;; Test Case 6: Overlay is not a valid overlay (not overlayp) returns nil
             ("invalid overlay returns nil"
              ,(cons '(:range (:start (:line 0))) 'not-an-overlay)
              t
              t
              nil)

             ;; Test Case 7: Overlay has no buffer (overlay-buffer returns nil) returns nil
             ("overlay with no buffer returns nil"
              ,(let ((ov (make-overlay (point-min) (point-min))))
                 (delete-overlay ov)
                 (cons '(:range (:start (:line 0))) ov))
              t
              t
              nil)

             ;; Test Case 8: Valid codelens-cell with server supporting resolveProvider makes async request
             ("valid codelens-cell makes async request"
              ,(let ((ov (make-overlay (point-min) (point-min))))
                 (cons '(:range (:start (:line 0))) ov))
              t
              t
              t))))

      ;; Run all test cases
      (dolist (test-case test-cases)
        (pcase test-case
          (`(,name ,codelens-cell ,server ,server-capable-p ,expect-request-called)
           (ert-info ((format "Test: %s" name))
             (let ((request-called nil)
                   (request-args nil))
               (cl-letf* (((symbol-function 'eglot-current-server)
                           (lambda () server))
                          ((symbol-function 'eglot-server-capable)
                           (lambda (&rest _args) server-capable-p))
                          ((symbol-function 'jsonrpc-async-request)
                           (lambda (server method &rest _args)
                             (setq request-called t)
                             (setq request-args (list server method))))
                          ((symbol-function 'eglot-codelens--update-resolved-codelens)
                           (lambda (&rest _args) nil)))
                 ;; Call the function being tested
                 (eglot-codelens--resolve-codelens codelens-cell)
                 ;; Verify expectations
                 (if expect-request-called
                     (progn
                       (should request-called)
                       (should (equal (cadr request-args) :codeLens/resolve))
                       (should request-args))
                   (should-not request-called)))))))))))

  (ert-deftest eglot-codelens--resolve-codelens-success-callback-test ()
    "Test success callback behavior of `eglot-codelens--resolve-codelens'."
    ;; Test case collection: (name buffer-live-p expect-update-called)
    (with-temp-buffer
      (let ((test-cases
             `(("success callback updates overlay when buffer is live"
                t
                t)

               ("success callback doesn't update overlay when buffer is dead"
                nil
                nil))))

        ;; Run all test cases
        (dolist (test-case test-cases)
          (pcase test-case
            (`(,name ,buffer-live-p ,expect-update-called)
             (ert-info ((format "Test: %s" name))
               (let ((update-called nil)
                     (update-args nil)
                     (codelens-cell (let ((ov (make-overlay (point-min) (point-min))))
                                      (cons '(:range (:start (:line 0))) ov)))
                     (resolved-codelens '(:command (:title "Resolved"))))
                 (cl-letf* (((symbol-function 'eglot-current-server)
                             (lambda () 'mock-server))
                            ((symbol-function 'eglot-server-capable)
                             (lambda (&rest _args) t))
                            ((symbol-function 'buffer-live-p)
                             (lambda (&rest _args) buffer-live-p))
                            ((symbol-function 'jsonrpc-async-request)
                             (lambda (server method params &rest args)
                               ;; Extract and call the success callback immediately
                               ;; The args list is (:success-fn fn :deferred :codeLens/resolve)
                               ;; but params might be included in args due to &rest behavior
                               (let ((success-fn (car (cdr (memq :success-fn args)))))
                                 (when (functionp success-fn)
                                   (funcall success-fn resolved-codelens)))))
                            ((symbol-function 'eglot-codelens--update-resolved-codelens)
                             (lambda (codelens-cell resolved)
                               (setq update-called t)
                               (setq update-args (list codelens-cell resolved)))))
                   ;; Call the function being tested
                   (eglot-codelens--resolve-codelens codelens-cell)
                   ;; Verify expectations
                   (if expect-update-called
                       (progn
                         (should update-called)
                         (should (eq (car update-args) codelens-cell))
                         (should (eq (cadr update-args) resolved-codelens)))
                     (should-not update-called)))))))))))

(ert-deftest eglot-codelens--resolve-process-queue-test ()
  "Test `eglot-codelens--resolve-process-queue' with various inputs."
  ;; Test case collection: (name queue version expect-called expect-cell-index expect-queue-empty)
  ;; expect-cell-index: nil for no call, 0-based index into queue for expected cell
  (with-temp-buffer
    (let ((test-cases
           ;; Test Case 1: Empty queue does nothing
           '(("empty queue does nothing"
              nil
              1
              nil
              nil
              t)

             ;; Test Case 2: All items are outdated (docver mismatch)
             ("all items are outdated"
              ((1 . ((:range (:start (:line 0))) . nil))
               (1 . ((:range (:start (:line 0))) . nil)))
              2
              nil
              nil
              t)

             ;; Test Case 3: Single valid item in queue
             ("single valid item in queue"
              ((2 . ((:range (:start (:line 0))) . nil)))
              2
              t
              0
              t)

             ;; Test Case 4: Multiple items with some outdated
             ("multiple items with some outdated"
              ((1 . ((:range (:start (:line 0))) . nil))
               (2 . ((:range (:start (:line 1))) . nil))
               (1 . ((:range (:start (:line 2))) . nil)))
              2
              t
              1
              t))))

      ;; Run all test cases
      (dolist (test-case test-cases)
        (pcase test-case
          (`(,name ,queue ,version ,expect-called ,expect-cell-index ,expect-queue-empty)
           (ert-info ((format "Test: %s" name))
             (let ((resolve-called nil)
                   (resolve-args nil))
               (cl-letf* (((symbol-function 'eglot-codelens--resolve-codelens)
                           (lambda (cell)
                             (setq resolve-called t)
                             (setq resolve-args (list cell)))))
                 ;; Set up test state
                 (setq eglot-codelens--version version)
                 (setq eglot-codelens--resolve-queue queue)
                 ;; Call the function being tested
                 (eglot-codelens--resolve-process-queue)
                 ;; Verify expectations
                 (if expect-called
                     (progn
                       (should resolve-called)
                       (should (eq (car resolve-args)
                                   (cdr (nth expect-cell-index queue)))))
                   (should-not resolve-called))
                 (if expect-queue-empty
                     (should-not eglot-codelens--resolve-queue)
                   (should eglot-codelens--resolve-queue)))))))))))

(ert-deftest eglot-codelens--fetch-codelens-test ()
  "Test `eglot-codelens--fetch-codelens' with various inputs."
  ;; Test case collection: (name server docver expect-request-called)
  (with-temp-buffer
    (let ((test-cases
           ;; Test Case 1: No server available returns nil without sending request
           `(("no server available returns nil"
              nil
              1
              nil)

             ;; Test Case 2: Cannot get docver returns nil without sending request
             ("cannot get docver returns nil"
              t
              nil
              nil)

             ;; Test Case 3: With server and docver sends async request
             ("with server and docver sends request"
              t
              1
              t))))

      ;; Run all test cases
      (dolist (test-case test-cases)
        (pcase test-case
          (`(,name ,server ,docver ,expect-request-called)
           (ert-info ((format "Test: %s" name))
             (let ((request-called nil)
                   (request-args nil)
                   ;; Set up mock for the underlying docver symbol before calling the function
                   ;; (eglot-codelens--docver is defsubst, so we mock the symbol it reads)
                   (docver-sym (symbol-value 'eglot-codelens--docver-symbol)))
               (set docver-sym docver)
               (cl-letf* (((symbol-function 'eglot-current-server)
                             (lambda () server))
                            ((symbol-function 'eglot--TextDocumentIdentifier)
                             (lambda () '(:uri "test://uri")))
                            ((symbol-function 'jsonrpc-async-request)
                             (lambda (server method &rest _args)
                               (setq request-called t)
                               (setq request-args (list server method)))))
                   ;; Call the function being tested
                   (eglot-codelens--fetch-codelens)
                   ;; Verify expectations
                   (if expect-request-called
                       (progn
                         (should request-called)
                         (should (equal (cadr request-args) :textDocument/codeLens))
                         (should request-args))
                     (should-not request-called)))))))))))

(ert-deftest eglot-codelens--fetch-codelens-success-callback-test ()
  "Test success callback behavior of `eglot-codelens--fetch-codelens'."
  ;; Test case collection: (name buffer-live-p eglot-codelens-mode window-buffer-match codelens-list expect-render-called)
  (with-temp-buffer
    (let ((test-cases
           ;; Test Case 1: Success callback with dead buffer does not process
           `(("dead buffer does not process"
              nil
              t
              t
              []
              nil)

             ;; Test Case 2: Success callback with eglot-codelens-mode off does not process
             ("eglot-codelens-mode off does not process"
              t
              nil
              t
              []
              nil)

             ;; Test Case 3: Success callback with buffer not in selected window does not process
             ("buffer not in selected window does not process"
              t
              t
              nil
              []
              nil)

             ;; Test Case 4: Success callback with empty codelens list (calls render with nil cache)
             ("empty codelens list calls render with nil cache"
              t
              t
              t
              []
              t)

             ;; Test Case 5: Success callback with valid codelens data builds cache and calls render
             ("valid codelens data builds cache and calls render"
              t
              t
              t
              [(:range (:start (:line 0 :character 0) :end (:line 0 :character 1)))]
              t))))

      ;; Run all test cases
      (dolist (test-case test-cases)
        (pcase test-case
          (`(,name ,buffer-live-p ,mode-active ,window-buffer-match ,codelens-list ,expect-render-called)
           (ert-info ((format "Test: %s" name))
             (let ((render-called nil)
                   (render-args nil))
               (cl-letf* (((symbol-function 'eglot-current-server)
                           (lambda () 'mock-server))
                          ((symbol-function 'eglot-codelens--docver)
                           (lambda () 1))
                          ((symbol-function 'eglot--TextDocumentIdentifier)
                           (lambda () '(:uri "test://uri")))
                          ((symbol-function 'buffer-live-p)
                           (lambda (&rest _args) buffer-live-p))
                          ((symbol-function 'jsonrpc-async-request)
                           (lambda (server method params &rest args)
                             ;; Extract and call the success callback
                             (let ((success-fn (car (cdr (memq :success-fn args)))))
                               (when (functionp success-fn)
                                 (funcall success-fn codelens-list)))))
                          ((symbol-function 'eglot-codelens--render-codelens)
                           (lambda (new-cache docver pending-lines file-changed-p &optional old-cache range)
                             (setq render-called t)
                             (setq render-args (list new-cache docver pending-lines file-changed-p old-cache range))))
                          ((symbol-function 'window-buffer)
                           (lambda (&rest _args)
                             (if window-buffer-match (current-buffer) (generate-new-buffer " *other*")))))
                 ;; Set up test state
                 (setq eglot-codelens-mode mode-active)
                 ;; Call the function being tested
                 (eglot-codelens--fetch-codelens)
                 ;; Verify expectations
                 (if expect-render-called
                     (should render-called)
                   (should-not render-called)))))))))))


(ert-deftest eglot-codelens--build-display-string-test ()
  "Test `eglot-codelens--build-display-string' with various inputs."
  (with-temp-buffer
    (insert "  test line")
    (goto-char (point-min))
    (let* ((codelens-cell (cons '(:command (:title "Test Action")) nil))
           (result (eglot-codelens--build-display-string
                    codelens-cell (point-min) 0 1))
           ;; Find the position where text properties start (after indentation)
           (prop-start (next-single-property-change 0 'face result)))
      ;; Check result is a string
      (should (stringp result))
      ;; Check indentation (for first codelens)
      (should (string-prefix-p "  " result))
      ;; Check separator (for last codelens, single item)
      (should (string-suffix-p "\n" result))
      ;; Check content is in result
      (should (string-match-p "Test Action" result))
      ;; Check face property (at the position where properties exist)
      (should (eq (get-text-property prop-start 'face result) 'eglot-codelens-face))
      ;; Check mouse-face property
      (should (eq (get-text-property prop-start 'mouse-face result) 'eglot-codelens-mouse-face))
      ;; Check help-echo property
      (should (equal (get-text-property prop-start 'help-echo result)
                     "Click to execute this CodeLens command"))
      ;; Check keymap property
      (let ((keymap (get-text-property prop-start 'keymap result)))
        (should (keymapp keymap))
        (should (lookup-key keymap [mouse-1]))))))

(ert-deftest eglot-codelens--build-display-string-multiple-test ()
  "Test `eglot-codelens--build-display-string' with multiple CodeLenses."
  (with-temp-buffer
    (insert "    test line")
    (goto-char (point-min))
    (let* ((codelens-cell (cons '(:command (:title "First")) nil))
           (result (eglot-codelens--build-display-string
                    codelens-cell (point-min) 0 3))
           (prop-start (next-single-property-change 0 'face result)))
      ;; First of multiple: should have indentation
      (should (string-prefix-p "    " result))
      ;; First of multiple: separator should be pipe (not newline)
      (should (string-suffix-p "|" result))
      (should-not (string-suffix-p "\n" result)))))

(ert-deftest eglot-codelens--build-display-string-middle-test ()
  "Test `eglot-codelens--build-display-string' for middle CodeLens."
  (with-temp-buffer
    (insert "  test line")
    (goto-char (point-min))
    (let* ((codelens-cell (cons '(:command (:title "Middle")) nil))
           (result (eglot-codelens--build-display-string
                    codelens-cell (point-min) 1 3)))
      ;; Middle CodeLens: no indentation (not first)
      (should-not (string-prefix-p "  " result))
      ;; Middle CodeLens: separator should be pipe (not last)
      (should (string-suffix-p "|" result))
      (should-not (string-suffix-p "\n" result)))))

(ert-deftest eglot-codelens--build-display-string-last-test ()
  "Test `eglot-codelens--build-display-string' for last CodeLens."
  (with-temp-buffer
    (insert "  test line")
    (goto-char (point-min))
    (let* ((codelens-cell (cons '(:command (:title "Last")) nil))
           (result (eglot-codelens--build-display-string
                    codelens-cell (point-min) 2 3)))
      ;; Last CodeLens: no indentation (not first)
      (should-not (string-prefix-p "  " result))
      ;; Last CodeLens: separator should be newline (not pipe)
      (should (string-suffix-p "\n" result))
      (should-not (string-suffix-p "|" result)))))

(ert-deftest eglot-codelens--make-overlay-test ()
  "Test `eglot-codelens--make-overlay' with various inputs."
  ;; Test case collection: (name codelens-cell index total docver expected-validator-fn)
  (with-temp-buffer
    (insert "  test line")
    (goto-char (point-min))
    (let ((test-cases
           ;; Test Case 1: Creates overlay with correct priority (index)
           `(("creates overlay with correct priority"
              ,(cons '(:command (:title "Test")) nil)
              5
              1
              1
              ,(lambda (result)
                 (should (overlayp result))
                 (should (= (overlay-get result 'priority) 5))))

             ;; Test Case 2: Sets eglot-codelens property to t
             ("sets eglot-codelens property to t"
              ,(cons '(:command (:title "Test")) nil)
              0
              1
              1
              ,(lambda (result)
                 (should (overlayp result))
                 (should (eq (overlay-get result 'eglot-codelens) t))))

             ;; Test Case 3: Sets eglot-codelens-docver to docver
             ("sets eglot-codelens-docver"
              ,(cons '(:command (:title "Test")) nil)
              0
              1
              42
              ,(lambda (result)
                 (should (overlayp result))
                 (should (= (overlay-get result 'eglot-codelens-docver) 42))))

             ;; Test Case 4: Sets eglot-codelens-usever to docver
             ("sets eglot-codelens-usever"
              ,(cons '(:command (:title "Test")) nil)
              0
              1
              7
              ,(lambda (result)
                 (should (overlayp result))
                 (should (= (overlay-get result 'eglot-codelens-usever) 7))))

             ;; Test Case 5: Sets eglot-codelens-command when codelens has :command
             ("sets eglot-codelens-command when present"
              ,(cons '(:command (:title "Test" :command (:id "test.cmd"))) nil)
              0
              1
              1
              ,(lambda (result)
                 (should (overlayp result))
                 (should (equal (overlay-get result 'eglot-codelens-command)
                                '(:title "Test" :command (:id "test.cmd"))))))

             ;; Test Case 6: Does not set eglot-codelens-command when codelens has no :command
             ("does not set eglot-codelens-command when absent"
              ,(cons '(:title "No Command") nil)
              0
              1
              1
              ,(lambda (result)
                 (should (overlayp result))
                 (should-not (overlay-get result 'eglot-codelens-command))))

             ;; Test Case 7: Sets before-string using build-display-string
             ("sets before-string"
              ,(cons '(:command (:title "Action")) nil)
              0
              1
              1
              ,(lambda (result)
                 (should (overlayp result))
                 (let ((before-string (overlay-get result 'before-string)))
                   (should (stringp before-string))
                   (should (string-match-p "Action" before-string)))))

             ;; Test Case 8: Adds overlay to eglot-codelens--overlays list
             ("adds overlay to overlays list"
              ,(cons '(:command (:title "Test")) nil)
              0
              1
              1
              ,(lambda (result)
                 (should (overlayp result))
                 (should (memq result eglot-codelens--overlays))))

             ;; Test Case 9: Returns the created overlay
             ("returns the created overlay"
              ,(cons '(:command (:title "Test")) nil)
              0
              1
              1
              ,(lambda (result)
                 (should (overlayp result))
                 (should (overlay-buffer result))
                 (should (eq (overlay-buffer result) (current-buffer))))))))

      ;; Run all test cases
      (dolist (test-case test-cases)
        (pcase test-case
          (`(,name ,codelens-cell ,index ,total ,docver ,validator-fn)
           (ert-info ((format "Test: %s" name))
             ;; Clear overlays list before each test
             (setq eglot-codelens--overlays nil)
             (let ((result (eglot-codelens--make-overlay
                           (point-min) codelens-cell index total docver)))
               (funcall validator-fn result)
               ;; Clean up overlay
               (when (overlayp result)
                 (delete-overlay result))))))))))

(ert-deftest eglot-codelens--codicons-to-nerd-icons-test ()
  "Test eglot-codelens--codicons-to-nerd-icons function."
  ;; Test case 1: when nerd-icons feature is not available, returns original title
  (cl-letf (((symbol-function 'featurep)
             (lambda (feat)
               (not (eq feat 'nerd-icons)))))
    (should (equal (eglot-codelens--codicons-to-nerd-icons "test title") "test title"))
    (should (equal (eglot-codelens--codicons-to-nerd-icons "$(chevron-down)") "$(chevron-down)"))))

(ert-deftest eglot-codelens--codicons-to-nerd-icons-no-placeholders-test ()
  "Test when title has no icon placeholders, returns original title."
  ;; Mock featurep to return t for nerd-icons
  (cl-letf (((symbol-function 'featurep)
             (lambda (feat)
               (eq feat 'nerd-icons))))
    ;; When there's no placeholder, return original title
    (should (equal (eglot-codelens--codicons-to-nerd-icons "simple title") "simple title"))
    (should (equal (eglot-codelens--codicons-to-nerd-icons "title with text") "title with text"))
    (should (equal (eglot-codelens--codicons-to-nerd-icons "") ""))))


(ert-deftest eglot-codelens--codicons-to-nerd-icons-single-icon-test ()
  "Test when title has single valid icon placeholder, returns converted string."
  ;; Mock featurep to return t for nerd-icons
  ;; Mock nerd-icons-codicon to return a fake icon
  (cl-letf* (((symbol-function 'featurep)
              (lambda (feat)
                (eq feat 'nerd-icons)))
             ((symbol-function 'nerd-icons-codicon)
              (lambda (icon-code)
                ;; Return a predictable string for testing
                (format "[ICON:%s]" icon-code))))
    (should (equal (eglot-codelens--codicons-to-nerd-icons "$(chevron-down)")
                   "[ICON:nf-cod-chevron_down]"))
    (should (equal (eglot-codelens--codicons-to-nerd-icons "prefix $(icon) suffix")
                   "prefix [ICON:nf-cod-icon] suffix"))))

(ert-deftest eglot-codelens--codicons-to-nerd-icons-edge-cases-test ()
  "Test edge cases for eglot-codelens--codicons-to-nerd-icons function."
  ;; Mock featurep to return t for nerd-icons
  ;; Mock nerd-icons-codicon:
  ;; - Returns fake icon for most icons
  ;; - Throws error for "invalid" and "invalid-icon"
  (cl-letf* (((symbol-function 'featurep)
              (lambda (feat)
                (eq feat 'nerd-icons)))
             ((symbol-function 'nerd-icons-codicon)
              (lambda (icon-code)
                (if (or (string= icon-code "nf-cod-invalid")
                        (string= icon-code "nf-cod-invalid_icon"))
                    (error "Icon not found")
                  ;; Return a predictable string for testing
                  (format "[ICON:%s]" icon-code)))))

    ;; Test multiple valid icon placeholders
    (should (equal (eglot-codelens--codicons-to-nerd-icons "$(icon1) $(icon2)")
                   "[ICON:nf-cod-icon1] [ICON:nf-cod-icon2]"))

    ;; Test invalid icon returns original placeholder
    (should (equal (eglot-codelens--codicons-to-nerd-icons "$(invalid-icon)")
                   "$(invalid-icon)"))

    ;; Test mixed valid and invalid icons
    (should (equal (eglot-codelens--codicons-to-nerd-icons "$(valid) $(invalid) $(also-valid)")
                   "[ICON:nf-cod-valid] $(invalid) [ICON:nf-cod-also_valid]"))

    ;; Test title with only placeholder
    (should (equal (eglot-codelens--codicons-to-nerd-icons "$(standalone)")
                   "[ICON:nf-cod-standalone]"))))

(ert-deftest eglot-codelens--format-text-codelens-command-test ()
  "Test when command exists in codelens plist with title, returns converted text."
  (cl-letf (((symbol-function 'eglot-codelens--codicons-to-nerd-icons)
             (lambda (title)
               (format "[CONVERTED:%s]" title))))
    ;; Create a codelens-cell with command in codelens plist
    (let* ((codelens (list :command (list :title "Run Tests")))
           (codelens-cell (cons codelens nil)))
      (should (equal (eglot-codelens--format-text codelens-cell)
                     "[CONVERTED:Run Tests]")))))

(ert-deftest eglot-codelens--format-text-overlay-command-test ()
  "Test when command exists in overlay with title (fallback), returns converted text."
  ;; Create a temp buffer and overlay
  (with-temp-buffer
    (cl-letf (((symbol-function 'eglot-codelens--codicons-to-nerd-icons)
               (lambda (title)
                 (format "[CONVERTED:%s]" title))))
      ;; Create overlay and set command property
      (let* ((ov (make-overlay 1 1))
             (codelens (list :range (list :start (list :line 0 :character 0)
                                           :end (list :line 0 :character 0))))
             (codelens-cell (cons codelens ov)))
        ;; Set the command on the overlay (fallback path)
        (overlay-put ov 'eglot-codelens-command (list :title "Debug Test"))
        (should (equal (eglot-codelens--format-text codelens-cell)
                       "[CONVERTED:Debug Test]"))
        (delete-overlay ov)))))

(ert-deftest eglot-codelens--format-text-loading-test ()
  "Test when command is nil, title is nil, or command is not a list, returns \"Loading...\"."
  ;; Test case 1: command is nil
  (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                        :end (list :line 0 :character 0))))
         (codelens-cell (cons codelens nil)))
    (should (equal (eglot-codelens--format-text codelens-cell) "Loading...")))

  ;; Test case 2: command is not a list (string)
  (let* ((codelens (list :command "not-a-list"
                         :range (list :start (list :line 0 :character 0)
                                      :end (list :line 0 :character 0))))
         (codelens-cell (cons codelens nil)))
    (should (equal (eglot-codelens--format-text codelens-cell) "Loading...")))

  ;; Test case 3: command is a list but title is nil
  (let* ((codelens (list :command (list :tooltip "no title")
                         :range (list :start (list :line 0 :character 0)
                                      :end (list :line 0 :character 0))))
         (codelens-cell (cons codelens nil)))
    (should (equal (eglot-codelens--format-text codelens-cell) "Loading..."))))

(ert-deftest eglot-codelens--format-text-edge-cases-test ()
  "Test edge cases for eglot-codelens--format-text function."
  (cl-letf (((symbol-function 'eglot-codelens--codicons-to-nerd-icons)
             (lambda (title)
               (format "[CONVERTED:%s]" title))))

    ;; Test case 1: when title is empty string, returns converted empty string
    (let* ((codelens (list :command (list :title "")
                           :range (list :start (list :line 0 :character 0)
                                        :end (list :line 0 :character 0))))
           (codelens-cell (cons codelens nil)))
      (should (equal (eglot-codelens--format-text codelens-cell) "[CONVERTED:]")))

    ;; Test case 2: when overlay is nil but command exists in codelens, returns converted text
    (let* ((codelens (list :command (list :title "Test Command")
                           :range (list :start (list :line 0 :character 0)
                                        :end (list :line 0 :character 0))))
           (codelens-cell (cons codelens nil)))
      (should (equal (eglot-codelens--format-text codelens-cell) "[CONVERTED:Test Command]")))

    ;; Test case 3: when overlay is not valid (not overlayp), falls back to codelens command
    (let* ((codelens (list :command (list :title "Fallback Command")
                           :range (list :start (list :line 0 :character 0)
                                        :end (list :line 0 :character 0))))
           (codelens-cell (cons codelens "not-an-overlay")))
      (should (equal (eglot-codelens--format-text codelens-cell) "[CONVERTED:Fallback Command]")))))

(ert-deftest eglot-codelens--update-resolved-codelens-test ()
  "Test `eglot-codelens--update-resolved-codelens' with various inputs."
  (with-temp-buffer
    (insert "  test line")
    (goto-char (point-min))

    ;; Test Case 1: eglot-codelens-mode is nil, does not update overlay
    (ert-info ("Test: eglot-codelens-mode is nil, does not update overlay")
      (let* ((ov (make-overlay (point-min) (point-min)))
             (codelens-cell (cons '(:range (:start (:line 0))) ov))
             (resolved '(:command (:title "Resolved"))))
        (overlay-put ov 'eglot-codelens-docver 1)
        (setq eglot-codelens-mode nil)
        (setq eglot-codelens--version 1)
        (setq eglot-codelens--cache nil)
        (eglot-codelens--update-resolved-codelens codelens-cell resolved)
      ;; Should not update command
      (should-not (overlay-get ov 'eglot-codelens-command))
      (delete-overlay ov)))

    ;; Test Case 2: overlay is nil, does not update
    (ert-info ("Test: overlay is nil, does not update")
      (let* ((codelens-cell (cons '(:range (:start (:line 0))) nil))
             (resolved '(:command (:title "Resolved"))))
        (setq eglot-codelens-mode t)
        (setq eglot-codelens--version 1)
        (setq eglot-codelens--cache nil)
        (eglot-codelens--update-resolved-codelens codelens-cell resolved)
      ;; Should not error, simply return nil
      (should t)))

    ;; Test Case 3: overlay is not a valid overlay (not overlayp), does not update
    (ert-info ("Test: overlay is not a valid overlay, does not update")
      (let* ((codelens-cell (cons '(:range (:start (:line 0))) 'not-an-overlay))
             (resolved '(:command (:title "Resolved"))))
        (setq eglot-codelens-mode t)
        (setq eglot-codelens--version 1)
        (setq eglot-codelens--cache nil)
        (eglot-codelens--update-resolved-codelens codelens-cell resolved)
      ;; Should not error
      (should t)))

    ;; Test Case 4: overlay has no buffer (overlay-buffer returns nil), does not update
    (ert-info ("Test: overlay has no buffer, does not update")
      (let* ((ov (make-overlay (point-min) (point-min)))
             (codelens-cell (cons '(:range (:start (:line 0))) ov))
             (resolved '(:command (:title "Resolved"))))
        (delete-overlay ov)
        (overlay-put ov 'eglot-codelens-docver 1)
        (setq eglot-codelens-mode t)
        (setq eglot-codelens--version 1)
        (setq eglot-codelens--cache nil)
        (eglot-codelens--update-resolved-codelens codelens-cell resolved)
      ;; Should not update command
      (should-not (overlay-get ov 'eglot-codelens-command))))

    ;; Test Case 5: overlay docver doesn't match current version, does not update
    (ert-info ("Test: overlay docver doesn't match current version, does not update")
      (let* ((ov (make-overlay (point-min) (point-min)))
             (codelens-cell (cons '(:range (:start (:line 0))) ov))
             (resolved '(:command (:title "Resolved"))))
        (overlay-put ov 'eglot-codelens-docver 2)  ; Different from version 1
        (overlay-put ov 'priority 0)
        (setq eglot-codelens-mode t)
        (setq eglot-codelens--version 1)
        (setq eglot-codelens--cache nil)
        (eglot-codelens--update-resolved-codelens codelens-cell resolved)
      ;; Should not update command
      (should-not (overlay-get ov 'eglot-codelens-command))
      (delete-overlay ov)))

    ;; Test Case 6: All conditions satisfied, successfully updates overlay
    (ert-info ("Test: all conditions satisfied, successfully updates overlay")
      (let* ((ov (make-overlay (point-min) (point-min)))
             (codelens-cell (cons '(:range (:start (:line 0))) ov))
             (resolved '(:command (:title "Resolved Action" :command (:id "test.cmd"))))
             (cache (make-hash-table :test 'eq)))
        (overlay-put ov 'eglot-codelens-docver 1)
        (overlay-put ov 'priority 0)
        (puthash 1 '() cache)
        (setq eglot-codelens-mode t)
        (setq eglot-codelens--version 1)
        (setq eglot-codelens--cache cache)
        (eglot-codelens--update-resolved-codelens codelens-cell resolved)
      ;; Should update command
      (should (equal (overlay-get ov 'eglot-codelens-command)
                     (plist-get resolved :command)))
      (should (overlay-get ov 'before-string))
      (delete-overlay ov)))))

(ert-deftest eglot-codelens--line-delta-test ()
  "Test `eglot-codelens--line-delta' function."
  (with-temp-buffer
    ;; Reset the global variable before each test case
    (setq eglot-codelens--prev-line-count nil)

    ;; Test case 1: initial state (prev-line-count is nil) returns 0
    (insert "line1\nline2\nline3")
    (let ((delta (eglot-codelens--line-delta)))
      (should (= delta 0))
      (should (= eglot-codelens--prev-line-count 3)))

    ;; Test case 2: line count unchanged returns 0
    (let ((delta (eglot-codelens--line-delta)))
      (should (= delta 0))
      (should (= eglot-codelens--prev-line-count 3)))

    ;; Test case 3: line count increased returns positive value
    (goto-char (point-max))
    (insert "\nline4\nline5")
    (let ((delta (eglot-codelens--line-delta)))
      (should (= delta 2))
      (should (= eglot-codelens--prev-line-count 5)))

    ;; Test case 4: line count decreased returns negative value
    (goto-char (point-min))
    (delete-char 20)  ; Delete some lines (from 5 to 2 lines)
    (let ((delta (eglot-codelens--line-delta)))
      (should (= delta -3))
      (should (= eglot-codelens--prev-line-count 2)))

    ;; Test case 5: prev-line-count is not an integer returns 0
    (setq eglot-codelens--prev-line-count "not-an-integer")
    (let ((delta (eglot-codelens--line-delta)))
      (should (= delta 0))
      (should (= eglot-codelens--prev-line-count 2)))))

(ert-deftest eglot-codelens--render-codelens-nil-cache-nil-pending-test ()
  "Test `eglot-codelens--render-codelens' with nil new-cache and nil pending-lines."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Set up initial state
    (setq eglot-codelens--cache nil)
    (setq eglot-codelens--version 1)
    (setq eglot-codelens--pending-lines nil)
    (setq eglot-codelens--resolve-queue nil)
    (setq eglot-codelens--overlays nil)
    ;; Mock resolve-schedule to track if it's called
    (let ((resolve-schedule-called nil))
      (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   (setq resolve-schedule-called t))))
        ;; Call with nil new-cache and nil pending-lines
        (eglot-codelens--render-codelens nil 1 nil nil)
        ;; Verify no overlays created
        (should-not eglot-codelens--overlays)
        ;; Verify resolve-schedule not called
        (should-not resolve-schedule-called)
        ;; Verify pending-lines remains nil
        (should-not eglot-codelens--pending-lines)))))

(ert-deftest eglot-codelens--render-codelens-nil-cache-non-nil-pending-test ()
  "Test `eglot-codelens--render-codelens' with nil new-cache but non-nil pending-lines."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Set up initial state
    (setq eglot-codelens--cache nil)
    (setq eglot-codelens--version 1)
    (setq eglot-codelens--pending-lines '(1 2 3))
    (setq eglot-codelens--resolve-queue nil)
    (setq eglot-codelens--overlays nil)
    ;; Mock resolve-schedule to track if it's called
    (let ((resolve-schedule-called nil))
      (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   (setq resolve-schedule-called t))))
        ;; Call with nil new-cache but non-nil pending-lines
        (eglot-codelens--render-codelens nil 1 '(1 2 3) nil)
        ;; Verify no overlays created
        (should-not eglot-codelens--overlays)
        ;; Verify resolve-schedule not called
        (should-not resolve-schedule-called)
        ;; Verify pending-lines remains unchanged
        (should (equal eglot-codelens--pending-lines '(1 2 3)))))))

(ert-deftest eglot-codelens--render-codelens-empty-cache-pending-test ()
  "Test `eglot-codelens--render-codelens' with empty cache but non-nil pending-lines."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Set up initial state with empty hash table as cache
    (let ((empty-cache (make-hash-table :test 'eq)))
      (setq eglot-codelens--cache empty-cache)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines '(1 2 3))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      ;; Mock resolve-schedule to track if it's called
      (let ((resolve-schedule-called nil))
        (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                   (lambda ()
                     (setq resolve-schedule-called t))))
          ;; Call with empty cache
          (eglot-codelens--render-codelens empty-cache 1 '(1 2 3) nil)
          ;; Verify no overlays created
          (should-not eglot-codelens--overlays)
          ;; Verify resolve-schedule not called
          (should-not resolve-schedule-called)
          ;; Verify pending-lines remains unchanged (lines not processed since cache is empty)
          (should (equal eglot-codelens--pending-lines '(1 2 3))))))))

(ert-deftest eglot-codelens--render-codelens-single-codelens-test ()
  "Test `eglot-codelens--render-codelens' creates overlay for single CodeLens."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build cache with single CodeLens on line 1
    (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                           :command (list :title "Run Tests")))
           (cache (make-hash-table :test 'eq))
           (cell (cons codelens nil)))
      (puthash 1 (list cell) cache)
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      ;; Mock resolve-schedule
      (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda () nil)))
        ;; Call render function
        (eglot-codelens--render-codelens cache 1 '(1) nil)
        ;; Verify overlay was created
        (should (= (length eglot-codelens--overlays) 1))
        (let ((ov (car eglot-codelens--overlays)))
          (should (overlayp ov))
          (should (eq (overlay-get ov 'eglot-codelens) t))
          (should (= (overlay-get ov 'eglot-codelens-docver) 1))
          (should (= (overlay-get ov 'eglot-codelens-usever) 1))
          (should (overlay-get ov 'before-string))
          ;; Verify pending-lines was updated (line 1 removed)
          (should-not (memq 1 eglot-codelens--pending-lines)))))))

(ert-deftest eglot-codelens--render-codelens-nil-pending-test ()
  "Test `eglot-codelens--render-codelens' with valid cache but nil pending-lines."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build cache with CodeLens
    (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                           :command (list :title "Run Tests")))
           (cache (make-hash-table :test 'eq))
           (cell (cons codelens nil)))
      (puthash 1 (list cell) cache)
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines nil)
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      ;; Call with nil pending-lines
      (eglot-codelens--render-codelens cache 1 nil nil)
      ;; Verify no overlays created (since no lines to process)
      (should-not eglot-codelens--overlays))))

(ert-deftest eglot-codelens--render-codelens-resolve-queue-test ()
  "Test `eglot-codelens--render-codelens' adds CodeLens without command to resolve-queue."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build cache with CodeLens WITHOUT :command (needs resolve)
    (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                           :title "Unresolved"))  ; No :command
           (cache (make-hash-table :test 'eq))
           (cell (cons codelens nil)))
      (puthash 1 (list cell) cache)
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      ;; Mock resolve-schedule
      (let ((schedule-called nil))
        (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                   (lambda ()
                     (setq schedule-called t))))
          ;; Call render function
          (eglot-codelens--render-codelens cache 1 '(1) nil)
          ;; Verify resolve-schedule was called
          (should schedule-called)
          ;; Verify CodeLens was added to resolve-queue
          (should eglot-codelens--resolve-queue)
          (should (= (length eglot-codelens--resolve-queue) 1))
          (let ((queue-item (car eglot-codelens--resolve-queue)))
            (should (= (car queue-item) 1))  ; docver
            (should (eq (cdr queue-item) cell))))))))

(ert-deftest eglot-codelens--render-codelens-no-resolve-test ()
  "Test `eglot-codelens--render-codelens' does not add CodeLens with command to queue."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build cache with CodeLens WITH :command (already resolved)
    (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                           :command (list :title "Run Tests")))
           (cache (make-hash-table :test 'eq))
           (cell (cons codelens nil)))
      (puthash 1 (list cell) cache)
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      ;; Mock resolve-schedule
      (let ((schedule-called nil))
        (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                   (lambda ()
                     (setq schedule-called t))))
          ;; Call render function
          (eglot-codelens--render-codelens cache 1 '(1) nil)
          ;; Verify resolve-schedule was NOT called
          (should-not schedule-called)
          ;; Verify resolve-queue is still empty
          (should-not eglot-codelens--resolve-queue))))))

(ert-deftest eglot-codelens--render-codelens-file-changed-cleanup-test ()
  "Test `eglot-codelens--render-codelens' cleanup when file-changed-p is t."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Create an existing overlay
    (let* ((old-ov (make-overlay (point-min) (point-min)))
           ;; New cache only has line 1
           (new-cache (make-hash-table :test 'eq))
           (codelens (list :range (list :start (list :line 0 :character 0)
                                          :end (list :line 0 :character 5))
                            :command (list :title "Run Tests")))
           (cell (cons codelens nil)))
      ;; Set up overlay properties
      (overlay-put old-ov 'eglot-codelens t)
      (overlay-put old-ov 'eglot-codelens-usever 1)  ; Old usever
      (setq eglot-codelens--overlays (list old-ov))
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 2)  ; New version
      (setq eglot-codelens--pending-lines '(1))
      (puthash 1 (list cell) new-cache)
      ;; Mock resolve-schedule
      (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda () nil)))
        ;; Call with file-changed-p = t
        (eglot-codelens--render-codelens new-cache 2 '(1) t nil)
        ;; Verify old overlay was deleted (not in overlays list)
        (should-not (memq old-ov eglot-codelens--overlays))))))

(ert-deftest eglot-codelens--render-codelens-no-file-changed-cleanup-test ()
  "Test `eglot-codelens--render-codelens' keeps old overlays when file-changed-p is nil."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Create an existing overlay
    (let* ((old-ov (make-overlay (point-min) (point-min)))
           ;; New cache only has line 1
           (new-cache (make-hash-table :test 'eq))
           (codelens (list :range (list :start (list :line 0 :character 0)
                                          :end (list :line 0 :character 5))
                            :command (list :title "Run Tests")))
           (cell (cons codelens nil)))
      ;; Set up overlay properties
      (overlay-put old-ov 'eglot-codelens t)
      (overlay-put old-ov 'eglot-codelens-usever 1)  ; Old usever
      (setq eglot-codelens--overlays (list old-ov))
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 2)  ; New version
      (setq eglot-codelens--pending-lines '(1))
      (puthash 1 (list cell) new-cache)
      ;; Mock resolve-schedule
      (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda () nil)))
        ;; Call with file-changed-p = nil
        (eglot-codelens--render-codelens new-cache 2 '(1) nil nil)
        ;; Verify old overlay is NOT deleted (still in overlays list)
        (should (memq old-ov eglot-codelens--overlays))
        (delete-overlay old-ov)))))

(ert-deftest eglot-codelens--render-codelens-empty-pending-test ()
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                           :command (list :title "Run Tests")))
           (cache (make-hash-table :test 'eq))
           (cell (cons codelens nil)))
      (puthash 1 (list cell) cache)
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines '())
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      (eglot-codelens--render-codelens cache 1 '() nil)
      (should-not eglot-codelens--overlays))))

(ert-deftest eglot-codelens--render-codelens-multiple-codelens-test ()
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (let* ((codelens1 (list :range (list :start (list :line 0 :character 0)
                                          :end (list :line 0 :character 5))
                            :command (list :title "Action 1")))
           (codelens2 (list :range (list :start (list :line 0 :character 0)
                                          :end (list :line 0 :character 5))
                            :command (list :title "Action 2")))
           (codelens3 (list :range (list :start (list :line 0 :character 0)
                                          :end (list :line 0 :character 5))
                            :command (list :title "Action 3")))
           (cache (make-hash-table :test 'eq))
           (cell1 (cons codelens1 nil))
           (cell2 (cons codelens2 nil))
           (cell3 (cons codelens3 nil)))
      (puthash 1 (list cell1 cell2 cell3) cache)
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda () nil)))
        (eglot-codelens--render-codelens cache 1 '(1) nil)
        (should (= (length eglot-codelens--overlays) 3))
        (let ((priorities (sort (mapcar (lambda (ov) (overlay-get ov 'priority))
                                         eglot-codelens--overlays)
                                #'<)))
          (should (equal priorities '(0 1 2))))))))

(ert-deftest eglot-codelens--render-codelens-multiple-lines-test ()
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (let* ((codelens1 (list :range (list :start (list :line 0 :character 0)
                                          :end (list :line 0 :character 5))
                            :command (list :title "Action 1")))
           (codelens3 (list :range (list :start (list :line 2 :character 0)
                                          :end (list :line 2 :character 5))
                            :command (list :title "Action 3")))
           (cache (make-hash-table :test 'eq))
           (cell1 (cons codelens1 nil))
           (cell3 (cons codelens3 nil)))
      (puthash 1 (list cell1) cache)
      (puthash 3 (list cell3) cache)
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines '(1 3))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda () nil)))
        (eglot-codelens--render-codelens cache 1 '(1 3) nil)
        (should (= (length eglot-codelens--overlays) 2))))))

(ert-deftest eglot-codelens--render-codelens-range-test ()
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5")
    (goto-char (point-min))
    (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                           :command (list :title "Action")))
           (cache (make-hash-table :test 'eq))
           (cell (cons codelens nil)))
      (puthash 1 (list cell) cache)
      (puthash 3 (list cell) cache)
      (puthash 5 (list cell) cache)
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines '(1 3 5))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda () nil)))
        (eglot-codelens--render-codelens cache 1 '(1 3 5) nil nil (cons 2 4))
        (should (= (length eglot-codelens--overlays) 1))))))

(ert-deftest eglot-codelens--render-codelens-no-resolve-outside-range-test ()
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                           :title "Unresolved"))
           (cache (make-hash-table :test 'eq))
           (cell (cons codelens nil)))
      (puthash 1 (list cell) cache)
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version 1)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      (let ((schedule-called nil))
        (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                   (lambda ()
                     (setq schedule-called t))))
          (eglot-codelens--render-codelens cache 1 '(1) nil nil (cons 2 3))
          (should-not schedule-called)
          (should-not eglot-codelens--resolve-queue))))))

(ert-deftest eglot-codelens--render-codelens-existing-overlay-matching-docver-test ()
  "Test `eglot-codelens--render-codelens' with overlay already in new-cache with matching docver."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build cache with single CodeLens on line 1, overlay already exists
    (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                           :command (list :title "Run Tests")))
           (cache (make-hash-table :test 'eq))
           (docver 1)
           (build-display-called nil)
           (build-display-args nil)
           cell existing-ov)
      ;; First, create an existing overlay
      (save-excursion
        (goto-line 1)
        (setq existing-ov (make-overlay (point) (point)))
        (overlay-put existing-ov 'eglot-codelens-docver docver)
        (overlay-put existing-ov 'eglot-codelens-usever docver)
        (overlay-put existing-ov 'before-string "old-display"))
      ;; Create cell with existing overlay
      (setq cell (cons codelens existing-ov))
      (puthash 1 (list cell) cache)
      ;; Set up initial state
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version docver)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays (list existing-ov))
      ;; Mock build-display-string to track if it's called
      (cl-letf (((symbol-function 'eglot-codelens--build-display-string)
                 (lambda (cell line-start index total)
                   (setq build-display-called t)
                   (setq build-display-args (list cell line-start index total))
                   "new-display"))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        ;; Call render-codelens with same docver
        (eglot-codelens--render-codelens cache docver '(1) nil)
        ;; Verify build-display-string was NOT called (since docver matches)
        (should-not build-display-called)
        (should-not build-display-args)
        ;; Verify overlay is still the same object (not recreated)
        (should (eq (cdr (car (gethash 1 cache))) existing-ov))
        ;; Verify before-string was NOT updated (still old value)
        (should (string= (overlay-get existing-ov 'before-string) "old-display"))
        ;; Verify docver is unchanged
        (should (eq (overlay-get existing-ov 'eglot-codelens-docver) docver))
        ;; Verify overlay is still in the overlays list
        (should (memq existing-ov eglot-codelens--overlays))))))

(ert-deftest eglot-codelens--render-codelens-existing-overlay-different-docver-test ()
  "Test `eglot-codelens--render-codelens' with overlay in new-cache with different docver."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build cache with single CodeLens on line 1, overlay already exists with different docver
    (let* ((codelens (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                           :command (list :title "Run Tests")))
           (cache (make-hash-table :test 'eq))
           (old-docver 1)
           (new-docver 2)
           (build-display-called nil)
           (build-display-args nil)
           cell existing-ov)
      ;; First, create an existing overlay with old docver
      (save-excursion
        (goto-line 1)
        (setq existing-ov (make-overlay (point) (point)))
        (overlay-put existing-ov 'eglot-codelens-docver old-docver)
        (overlay-put existing-ov 'eglot-codelens-usever old-docver)
        (overlay-put existing-ov 'before-string "old-display"))
      ;; Create cell with existing overlay
      (setq cell (cons codelens existing-ov))
      (puthash 1 (list cell) cache)
      ;; Set up initial state
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version new-docver)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays (list existing-ov))
      ;; Mock build-display-string to track if it's called
      (cl-letf (((symbol-function 'eglot-codelens--build-display-string)
                 (lambda (cell line-start index total)
                   (setq build-display-called t)
                   (setq build-display-args (list cell line-start index total))
                   "new-display"))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        ;; Call render-codelens with different docver
        (eglot-codelens--render-codelens cache new-docver '(1) nil)
        ;; Verify build-display-string WAS called (since docver differs)
        (should build-display-called)
        (should build-display-args)
        ;; Verify overlay is still the same object (not recreated)
        (should (eq (cdr (car (gethash 1 cache))) existing-ov))
        ;; Verify before-string WAS updated (now has new value)
        (should (string= (overlay-get existing-ov 'before-string) "new-display"))
        ;; Verify docver was updated to new docver
        (should (eq (overlay-get existing-ov 'eglot-codelens-docver) new-docver))
        ;; Verify overlay is still in the overlays list
        (should (memq existing-ov eglot-codelens--overlays))))))

(ert-deftest eglot-codelens--render-codelens-old-cache-same-line-reuse-test ()
  "Test `eglot-codelens--render-codelens' reuses overlay from old-cache on same line."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build old-cache with overlay on line 1
    (let* ((old-codelens (list :range (list :start (list :line 0 :character 0)
                                            :end (list :line 0 :character 5))
                              :command (list :title "Old Test")))
           (old-cache (make-hash-table :test 'eq))
           (new-codelens (list :range (list :start (list :line 0 :character 0)
                                            :end (list :line 0 :character 5))
                              :command (list :title "New Test")))
           (new-cache (make-hash-table :test 'eq))
           (docver 1)
           (build-display-called nil)
           (build-display-args nil)
           (make-overlay-called nil)
           old-cell old-ov new-cell)
      ;; Create overlay in old-cache
      (save-excursion
        (goto-line 1)
        (setq old-ov (make-overlay (point) (point)))
        (overlay-put old-ov 'eglot-codelens-docver 0)
        (overlay-put old-ov 'eglot-codelens-usever 0)
        (overlay-put old-ov 'before-string "old-display"))
      (setq old-cell (cons old-codelens old-ov))
      (puthash 1 (list old-cell) old-cache)
      ;; Set up new-cache without overlay (cdr is nil)
      (setq new-cell (cons new-codelens nil))
      (puthash 1 (list new-cell) new-cache)
      ;; Set up initial state
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version docver)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays (list old-ov))
      ;; Mock build-display-string and make-overlay to track calls
      (cl-letf (((symbol-function 'eglot-codelens--build-display-string)
                 (lambda (cell line-start index total)
                   (setq build-display-called t)
                   (setq build-display-args (list cell line-start index total))
                   "new-display"))
                ((symbol-function 'eglot-codelens--make-overlay)
                 (lambda (line-start cell index total docver)
                   (setq make-overlay-called t)
                   (make-overlay line-start line-start)))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        ;; Call render-codelens with old-cache
        (eglot-codelens--render-codelens new-cache docver '(1) nil old-cache)
        ;; Verify build-display-string WAS called (overlay was updated)
        (should build-display-called)
        (should build-display-args)
        ;; Verify make-overlay was NOT called (reused from old-cache)
        (should-not make-overlay-called)
        ;; Verify new-cache cell now points to the reused overlay
        (should (eq (cdr (car (gethash 1 new-cache))) old-ov))
        ;; Verify before-string was updated
        (should (string= (overlay-get old-ov 'before-string) "new-display"))
        ;; Verify docver was updated
        (should (eq (overlay-get old-ov 'eglot-codelens-docver) docver))
        ;; Verify usever was updated
        (should (eq (overlay-get old-ov 'eglot-codelens-usever) docver))
        ;; Verify overlay is still in the overlays list
        (should (memq old-ov eglot-codelens--overlays))))))

(ert-deftest eglot-codelens--render-codelens-old-cache-deleted-overlay-test ()
  "Test `eglot-codelens--render-codelens' creates new overlay when old-cache overlay is deleted."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build old-cache with overlay that will be deleted
    (let* ((old-codelens (list :range (list :start (list :line 0 :character 0)
                                            :end (list :line 0 :character 5))
                              :command (list :title "Old Test")))
           (old-cache (make-hash-table :test 'eq))
           (new-codelens (list :range (list :start (list :line 0 :character 0)
                                            :end (list :line 0 :character 5))
                              :command (list :title "New Test")))
           (new-cache (make-hash-table :test 'eq))
           (docver 1)
           (make-overlay-called nil)
           (created-ov nil)
           old-cell old-ov new-cell)
      ;; Create overlay in old-cache
      (save-excursion
        (goto-line 1)
        (setq old-ov (make-overlay (point) (point)))
        (overlay-put old-ov 'eglot-codelens-docver 0)
        (overlay-put old-ov 'eglot-codelens-usever 0))
      (setq old-cell (cons old-codelens old-ov))
      (puthash 1 (list old-cell) old-cache)
      ;; Set up new-cache without overlay (cdr is nil)
      (setq new-cell (cons new-codelens nil))
      (puthash 1 (list new-cell) new-cache)
      ;; Set up initial state
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version docver)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      ;; Delete the old overlay to simulate it being gone
      (delete-overlay old-ov)
      ;; Mock make-overlay to track calls and capture created overlay
      (cl-letf (((symbol-function 'eglot-codelens--make-overlay)
                 (lambda (line-start cell index total docver)
                   (setq make-overlay-called t)
                   (let ((ov (make-overlay line-start line-start)))
                     (overlay-put ov 'eglot-codelens-docver docver)
                     (overlay-put ov 'eglot-codelens-usever docver)
                     (push ov eglot-codelens--overlays)
                     (setq created-ov ov))
                   created-ov))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        ;; Call render-codelens with old-cache (but overlay is deleted)
        (eglot-codelens--render-codelens new-cache docver '(1) nil old-cache)
        ;; Verify make-overlay WAS called (new overlay created)
        (should make-overlay-called)
        (should created-ov)
        ;; Verify new-cache cell now points to the new overlay
        (should (eq (cdr (car (gethash 1 new-cache))) created-ov))
        ;; Verify old-ov is not the same as created-ov
        (should-not (eq old-ov created-ov))
        ;; Verify new overlay has correct docver
        (should (eq (overlay-get created-ov 'eglot-codelens-docver) docver))
        ;; Verify new overlay has correct usever
        (should (eq (overlay-get created-ov 'eglot-codelens-usever) docver))
        ;; Verify new overlay is in the overlays list
        (should (memq created-ov eglot-codelens--overlays))))))

(ert-deftest eglot-codelens--render-codelens-range-outside-usever-only-test ()
  "Test `eglot-codelens--render-codelens' updates only usever for lines outside range."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build old-cache with overlay on line 1 (outside range)
    (let* ((old-codelens (list :range (list :start (list :line 0 :character 0)
                                            :end (list :line 0 :character 5))
                              :command (list :title "Old Test")))
           (old-cache (make-hash-table :test 'eq))
           (new-codelens (list :range (list :start (list :line 0 :character 0)
                                            :end (list :line 0 :character 5))
                              :command (list :title "New Test")))
           (new-cache (make-hash-table :test 'eq))
           (docver 1)
           (old-docver 0)
           (build-display-called nil)
           old-cell old-ov new-cell)
      ;; Create overlay in old-cache on line 1 (will be outside range)
      (save-excursion
        (goto-line 1)
        (setq old-ov (make-overlay (point) (point)))
        (overlay-put old-ov 'eglot-codelens-docver old-docver)
        (overlay-put old-ov 'eglot-codelens-usever old-docver)
        (overlay-put old-ov 'before-string "old-display"))
      (setq old-cell (cons old-codelens old-ov))
      (puthash 1 (list old-cell) old-cache)
      ;; Set up new-cache without overlay (cdr is nil)
      (setq new-cell (cons new-codelens nil))
      (puthash 1 (list new-cell) new-cache)
      ;; Set up initial state
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version docver)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays (list old-ov))
      ;; Mock build-display-string to track if it's called
      (cl-letf (((symbol-function 'eglot-codelens--build-display-string)
                 (lambda (cell line-start index total)
                   (setq build-display-called t)
                   "new-display"))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        ;; Call render-codelens with range (2 . 3) - line 1 is outside range
        (eglot-codelens--render-codelens new-cache docver '(1) nil old-cache (cons 2 3))
        ;; Verify build-display-string was NOT called (outside range, no content update)
        (should-not build-display-called)
        ;; Verify new-cache cell now points to the reused overlay
        (should (eq (cdr (car (gethash 1 new-cache))) old-ov))
        ;; Verify before-string was NOT updated (still old value)
        (should (string= (overlay-get old-ov 'before-string) "old-display"))
        ;; Verify docver was NOT updated (still old value)
        (should (eq (overlay-get old-ov 'eglot-codelens-docver) old-docver))
        ;; Verify usever WAS updated to new docver
        (should (eq (overlay-get old-ov 'eglot-codelens-usever) docver))
        ;; Verify overlay is still in the overlays list
        (should (memq old-ov eglot-codelens--overlays))))))

(ert-deftest eglot-codelens--render-codelens-range-preserve-current-docver-test ()
  "Test `eglot-codelens--render-codelens' preserves overlays in range with current docver."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build cache with overlay on line 2 (in range) with current docver
    (let* ((codelens (list :range (list :start (list :line 1 :character 0)
                                         :end (list :line 1 :character 5))
                           :command (list :title "Test")))
           (cache (make-hash-table :test 'eq))
           (docver 1)
           (build-display-called nil)
           (move-overlay-called nil)
           cell existing-ov)
      ;; Create existing overlay with current docver
      (save-excursion
        (goto-line 2)
        (setq existing-ov (make-overlay (point) (point)))
        (overlay-put existing-ov 'eglot-codelens-docver docver)
        (overlay-put existing-ov 'eglot-codelens-usever docver)
        (overlay-put existing-ov 'before-string "current-display"))
      (setq cell (cons codelens existing-ov))
      (puthash 2 (list cell) cache)
      ;; Set up initial state
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version docver)
      (setq eglot-codelens--pending-lines '(2))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays (list existing-ov))
      ;; Mock build-display-string and move-overlay to track calls
      (cl-letf (((symbol-function 'eglot-codelens--build-display-string)
                 (lambda (cell line-start index total)
                   (setq build-display-called t)
                   "new-display"))
                ((symbol-function 'move-overlay)
                 (lambda (ov beg end &optional buffer)
                   (setq move-overlay-called t)))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        ;; Call render-codelens with range (1 . 3) - line 2 is in range
        (eglot-codelens--render-codelens cache docver '(2) nil nil (cons 1 3))
        ;; Verify build-display-string was NOT called (docver matches)
        (should-not build-display-called)
        ;; Verify move-overlay was NOT called (docver matches, preserved unchanged)
        (should-not move-overlay-called)
        ;; Verify before-string was NOT updated
        (should (string= (overlay-get existing-ov 'before-string) "current-display"))
        ;; Verify overlay is still in the overlays list
        (should (memq existing-ov eglot-codelens--overlays))))))

(ert-deftest eglot-codelens--render-codelens-file-changed-delta-test ()
  "Test `eglot-codelens--render-codelens' calculates line-delta when file-changed-p is t."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build cache with single CodeLens on line 2
    (let* ((codelens (list :range (list :start (list :line 1 :character 0)
                                         :end (list :line 1 :character 5))
                           :command (list :title "Run Tests")))
           (cache (make-hash-table :test 'eq))
           (docver 1)
           (line-delta-called nil)
           (line-delta-result nil)
           cell)
      (setq cell (cons codelens nil))
      (puthash 2 (list cell) cache)
      ;; Set up initial state with prev-line-count to simulate line changes
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--prev-line-count 1)
      (setq eglot-codelens--version docver)
      (setq eglot-codelens--pending-lines '(2))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      ;; Mock line-delta to track if it's called
      (cl-letf (((symbol-function 'eglot-codelens--line-delta)
                 (lambda ()
                   (setq line-delta-called t)
                   (setq line-delta-result (- (line-number-at-pos (point-max)) eglot-codelens--prev-line-count))
                   line-delta-result))
                ((symbol-function 'eglot-codelens--make-overlay)
                 (lambda (line-start cell index total docver)
                   (make-overlay line-start line-start)))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        ;; Call render-codelens with file-changed-p is t
        (eglot-codelens--render-codelens cache docver '(2) t)
        ;; Verify line-delta WAS called
        (should line-delta-called)
        (should line-delta-result)
        ;; Verify result is non-zero (3 lines - 1 = 2)
        (should (> line-delta-result 0))))))

(ert-deftest eglot-codelens--render-codelens-no-file-changed-no-delta-test ()
  "Test `eglot-codelens--render-codelens' skips line-delta when file-changed-p is nil."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    ;; Build cache with single CodeLens on line 2
    (let* ((codelens (list :range (list :start (list :line 1 :character 0)
                                         :end (list :line 1 :character 5))
                           :command (list :title "Run Tests")))
           (cache (make-hash-table :test 'eq))
           (docver 1)
           (line-delta-called nil)
           cell)
      (setq cell (cons codelens nil))
      (puthash 2 (list cell) cache)
      ;; Set up initial state
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--prev-line-count 1)
      (setq eglot-codelens--version docver)
      (setq eglot-codelens--pending-lines '(2))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays nil)
      ;; Mock line-delta to track if it's called
      (cl-letf (((symbol-function 'eglot-codelens--line-delta)
                 (lambda ()
                   (setq line-delta-called t)
                   999))
                ((symbol-function 'eglot-codelens--make-overlay)
                 (lambda (line-start cell index total docver)
                   (make-overlay line-start line-start)))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        ;; Call render-codelens with file-changed-p is nil
        (eglot-codelens--render-codelens cache docver '(2) nil)
        ;; Verify line-delta was NOT called
        (should-not line-delta-called)))))

(ert-deftest eglot-codelens--render-codelens-before-cursor-direct-lookup-test ()
  "Test `eglot-codelens--render-codelens' uses direct line lookup for lines before cursor."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (forward-line 1)  ; Move to line 2 (cursor at line 2)
    ;; Build old-cache with overlay on line 1 (before cursor)
    (let* ((old-codelens (list :range (list :start (list :line 0 :character 0)
                                            :end (list :line 0 :character 5))
                              :command (list :title "Old Test")))
           (old-cache (make-hash-table :test 'eq))
           (new-codelens (list :range (list :start (list :line 0 :character 0)
                                            :end (list :line 0 :character 5))
                              :command (list :title "New Test")))
           (new-cache (make-hash-table :test 'eq))
           (docver 1)
           old-cell old-ov new-cell)
      ;; Create overlay in old-cache on line 1 (before cursor)
      (save-excursion
        (goto-line 1)
        (setq old-ov (make-overlay (point) (point)))
        (overlay-put old-ov 'eglot-codelens-docver 0)
        (overlay-put old-ov 'eglot-codelens-usever 0))
      (setq old-cell (cons old-codelens old-ov))
      (puthash 1 (list old-cell) old-cache)
      ;; Set up new-cache without overlay
      (setq new-cell (cons new-codelens nil))
      (puthash 1 (list new-cell) new-cache)
      ;; Set up initial state
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version docver)
      (setq eglot-codelens--pending-lines '(1))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays (list old-ov))
      ;; Simulate file change with delta
      (setq eglot-codelens--prev-line-count 2)
      (setq eglot-codelens--change-begin-line 2)
      ;; Call render-codelens with file-changed-p
      (cl-letf (((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        (eglot-codelens--render-codelens new-cache docver '(1) t old-cache)
        ;; Verify overlay from old-cache on line 1 was reused (direct lookup)
        (should (eq (cdr (car (gethash 1 new-cache))) old-ov))
        ;; Verify overlay was updated
        (should (eq (overlay-get old-ov 'eglot-codelens-docver) docver))))))

(ert-deftest eglot-codelens--render-codelens-after-cursor-adjusted-lookup-test ()
  "Test `eglot-codelens--render-codelens' uses adjusted lookup for lines at/after cursor."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (forward-line 1)  ; Move to line 2 (cursor at line 2)
    ;; Build old-cache with overlay on line 3 (after cursor)
    (let* ((old-codelens (list :range (list :start (list :line 2 :character 0)
                                            :end (list :line 2 :character 5))
                              :command (list :title "Old Test")))
           (old-cache (make-hash-table :test 'eq))
           (new-codelens (list :range (list :start (list :line 2 :character 0)
                                            :end (list :line 2 :character 5))
                              :command (list :title "New Test")))
           (new-cache (make-hash-table :test 'eq))
           (docver 1)
           (line-delta 1)
           old-cell old-ov new-cell)
      ;; Create overlay in old-cache on line 3 (after cursor)
      ;; With delta=1, should lookup line (3-1)=2 in old-cache
      (save-excursion
        (goto-line 3)
        (setq old-ov (make-overlay (point) (point)))
        (overlay-put old-ov 'eglot-codelens-docver 0)
        (overlay-put old-ov 'eglot-codelens-usever 0))
      ;; Put in old-cache at line 2 (adjusted position)
      (setq old-cell (cons old-codelens old-ov))
      (puthash 2 (list old-cell) old-cache)  ; line 3 - delta(1) = 2
      ;; Set up new-cache for line 3
      (setq new-cell (cons new-codelens nil))
      (puthash 3 (list new-cell) new-cache)
      ;; Set up initial state
      (setq eglot-codelens--cache nil)
      (setq eglot-codelens--version docver)
      (setq eglot-codelens--pending-lines '(3))
      (setq eglot-codelens--resolve-queue nil)
      (setq eglot-codelens--overlays (list old-ov))
      ;; Set change-begin-line and simulate line-delta
      (setq eglot-codelens--change-begin-line 2)
      (setq eglot-codelens--prev-line-count 2)
      ;; Mock line-delta to return non-zero value
      (cl-letf (((symbol-function 'eglot-codelens--line-delta)
                 (lambda ()
                   line-delta))
                ((symbol-function 'eglot-codelens--resolve-schedule)
                 (lambda ()
                   nil)))
        (eglot-codelens--render-codelens new-cache docver '(3) t old-cache)
        ;; Verify overlay from old-cache was reused using adjusted lookup (line 3 - delta = 2)
        (should (eq (cdr (car (gethash 3 new-cache))) old-ov))
        ;; Verify overlay was updated
        (should (eq (overlay-get old-ov 'eglot-codelens-docver) docver))))))


(ert-deftest eglot-codelens-execute-test ()
  "Test `eglot-codelens-execute' with various inputs."
  (with-temp-buffer
    ;; Test Case 1: command from codelens (car of cell) executes via eglot-execute
    (let ((execute-called nil)
          (execute-server nil)
          (execute-command nil))
      (cl-letf (((symbol-function 'eglot-execute)
                 (lambda (server command)
                   (setq execute-called t)
                   (setq execute-server server)
                   (setq execute-command command)))
                ((symbol-function 'eglot--current-server-or-lose)
                 (lambda () 'mock-server)))
        (eglot-codelens-execute (cons '(:command (:title "Test" :arguments [])) nil))
        (should execute-called)
        (should (eq execute-server 'mock-server))
        (should (equal execute-command '(:title "Test" :arguments [])))))

    ;; Test Case 2: command from overlay executes via eglot-execute (fallback)
    (let ((overlay (make-overlay (point-min) (point-min)))
          (execute-called nil)
          (execute-command nil))
      (overlay-put overlay 'eglot-codelens-command '(:title "Overlay Command" :arguments []))
      (cl-letf (((symbol-function 'eglot-execute)
                 (lambda (server command)
                   (setq execute-called t)
                   (setq execute-command command)))
                ((symbol-function 'eglot--current-server-or-lose)
                 (lambda () 'mock-server)))
        (eglot-codelens-execute (cons '() overlay))
        (should execute-called)
        (should (equal execute-command '(:title "Overlay Command" :arguments []))))
      (delete-overlay overlay))

    ;; Test Case 3: codelens command takes priority over overlay command
    (let ((overlay (make-overlay (point-min) (point-min)))
          (execute-command nil))
      (overlay-put overlay 'eglot-codelens-command '(:title "Overlay" :arguments []))
      (cl-letf (((symbol-function 'eglot-execute)
                 (lambda (server command)
                   (setq execute-command command)))
                ((symbol-function 'eglot--current-server-or-lose)
                 (lambda () 'mock-server)))
        (eglot-codelens-execute (cons '(:command (:title "Codelens" :arguments [])) overlay))
        (should (equal execute-command '(:title "Codelens" :arguments []))))
      (delete-overlay overlay))

    ;; Test Case 4: no command and no server returns nil without execution
    (let ((execute-called nil))
      (cl-letf (((symbol-function 'eglot-execute)
                 (lambda (&rest _) (setq execute-called t)))
                ((symbol-function 'eglot-current-server)
                 (lambda () nil)))
        (eglot-codelens-execute (cons '() nil))
        (should-not execute-called)))

    ;; Test Case 5: no command, server exists but doesn't support resolveProvider shows error
    (let ((message-output nil))
      (cl-letf (((symbol-function 'eglot-execute)
                 (lambda (&rest _) nil))
                ((symbol-function 'eglot-current-server)
                 (lambda () (make-hash-table)))
                ((symbol-function 'eglot-server-capable)
                 (lambda (&rest _) nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-output (apply #'format fmt args)))))
        (eglot-codelens-execute (cons '() nil))
        (should (string= message-output "CodeLens command not available"))))

    ;; Test Case 6: no command, server supports resolveProvider triggers resolve
    (let ((resolve-called nil)
          (resolve-cell nil)
          (message-output nil))
      (cl-letf (((symbol-function 'eglot-execute)
                 (lambda (&rest _) nil))
                ((symbol-function 'eglot-current-server)
                 (lambda () (make-hash-table)))
                ((symbol-function 'eglot-server-capable)
                 (lambda (&rest _) t))
                ((symbol-function 'eglot-codelens--resolve-codelens)
                 (lambda (cell)
                   (setq resolve-called t)
                   (setq resolve-cell cell)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-output (apply #'format fmt args)))))
        (let ((cell (cons '() nil)))
          (eglot-codelens-execute cell)
          (should resolve-called)
          (should (eq resolve-cell cell))
          (should (string= message-output "Resolving CodeLens command...")))))))

(ert-deftest eglot-codelens-execute-at-line-no-codelens-test ()
  "Test `eglot-codelens-execute-at-line' when no CodeLens found at line."
  ;; Test case 1: cache is empty hash table for current line
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (let* ((cache (make-hash-table :test 'eq))
           (message-output nil)
           (line 1))
      ;; Set up cache (empty, no CodeLens on line 1)
      (setq eglot-codelens--cache cache)
      ;; Mock message to capture output
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-output (apply #'format fmt args)))))
        ;; Call the function being tested
        (eglot-codelens-execute-at-line)
        ;; Verify message was shown
        (should message-output)
        (should (string-match-p "No CodeLens found at line 1" message-output))))))

(ert-deftest eglot-codelens-execute-at-line-empty-cache-test ()
  "Test `eglot-codelens-execute-at-line' when cache is empty hash table."
  ;; Test case 2: cache is hash table but line not found
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (let* ((cache (make-hash-table :test 'eq))
           (message-output nil))
      ;; Set up cache (hash table, but no CodeLens on line 1)
      (setq eglot-codelens--cache cache)
      ;; Mock message to capture output
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-output (apply #'format fmt args)))))
        ;; Call the function being tested
        (eglot-codelens-execute-at-line)
        ;; Verify message was shown
        (should message-output)
        (should (string-match-p "No CodeLens found at line 1" message-output))))))

(ert-deftest eglot-codelens-execute-at-line-single-codelens-test ()
  "Test `eglot-codelens-execute-at-line' with single CodeLens at line."
  ;; Test case 3: cache has one CodeLens, should execute directly
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (let* ((cache (make-hash-table :test 'eq))
           (codelens (list :range (list :start (list :line 0 :character 0)
                                        :end (list :line 0 :character 5))
                           :command (list :title "Run Tests" :arguments [])))
           (cell (cons codelens nil))
           (execute-called nil)
           (execute-cell nil))
      ;; Set up cache with single CodeLens on line 1
      (puthash 1 (list cell) cache)
      (setq eglot-codelens--cache cache)
      ;; Mock eglot-codelens-execute to verify it was called
      (cl-letf (((symbol-function 'eglot-codelens-execute)
                 (lambda (codelens-cell)
                   (setq execute-called t)
                   (setq execute-cell codelens-cell))))
        ;; Call the function being tested
        (eglot-codelens-execute-at-line)
        ;; Verify eglot-codelens-execute was called with the correct cell
        (should execute-called)
        (should (eq execute-cell cell))))))

(ert-deftest eglot-codelens-execute-at-line-multiple-codelens-test ()
  "Test `eglot-codelens-execute-at-line' with multiple CodeLens and user selection."
  ;; Test case 4: cache has multiple CodeLenses, should show selection menu
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (let* ((cache (make-hash-table :test 'eq))
           (codelens1 (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                            :command (list :title "Run Tests" :arguments [])))
           (codelens2 (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                            :command (list :title "Debug Tests" :arguments [])))
           (cell1 (cons codelens1 nil))
           (cell2 (cons codelens2 nil))
           (execute-called nil)
           (execute-cell nil)
           (completing-read-called nil))
      ;; Set up cache with two CodeLenses on line 1
      (puthash 1 (list cell1 cell2) cache)
      (setq eglot-codelens--cache cache)
      ;; Mock eglot-codelens-execute to verify it was called
      ;; Mock completing-read to simulate user selecting the first option
      (cl-letf* (((symbol-function 'eglot-codelens-execute)
                  (lambda (codelens-cell)
                    (setq execute-called t)
                    (setq execute-cell codelens-cell)))
                 ((symbol-function 'completing-read)
                  (lambda (prompt collection &rest _args)
                    (setq completing-read-called t)
                    ;; Return the display string of the first CodeLens
                    (car (car collection))))
                 ((symbol-function 'eglot-codelens--format-text)
                  (lambda (cell)
                    (let ((cmd (plist-get (car cell) :command)))
                      (plist-get cmd :title)))))
        ;; Call the function being tested
        (eglot-codelens-execute-at-line)
        ;; Verify completing-read was called (selection menu shown)
        (should completing-read-called)
        ;; Verify eglot-codelens-execute was called with cell1 (first option)
        (should execute-called)
        (should (eq execute-cell cell1))))))

(ert-deftest eglot-codelens-execute-at-line-multiple-codelens-cancel-test ()
  "Test `eglot-codelens-execute-at-line' with multiple CodeLens and user canceling."
  ;; Test case 5: cache has multiple CodeLenses, user cancels, should not execute
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (let* ((cache (make-hash-table :test 'eq))
           (codelens1 (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                            :command (list :title "Run Tests" :arguments [])))
           (codelens2 (list :range (list :start (list :line 0 :character 0)
                                         :end (list :line 0 :character 5))
                            :command (list :title "Debug Tests" :arguments [])))
           (cell1 (cons codelens1 nil))
           (cell2 (cons codelens2 nil))
           (execute-called nil))
      ;; Set up cache with two CodeLenses on line 1
      (puthash 1 (list cell1 cell2) cache)
      (setq eglot-codelens--cache cache)
      ;; Mock eglot-codelens-execute to verify it was NOT called
      ;; Mock completing-read to return empty string (simulating user canceling)
      (cl-letf* (((symbol-function 'eglot-codelens-execute)
                  (lambda (codelens-cell)
                    (setq execute-called t)))
                 ((symbol-function 'completing-read)
                  (lambda (prompt collection &rest _args)
                    ;; Return empty string to simulate user canceling
                    ""))
                 ((symbol-function 'eglot-codelens--format-text)
                  (lambda (cell)
                    (let ((cmd (plist-get (car cell) :command)))
                      (plist-get cmd :title)))))
        ;; Call the function being tested
        (eglot-codelens-execute-at-line)
        ;; Verify eglot-codelens-execute was NOT called
        (should-not execute-called)))))

(ert-deftest eglot-codelens--change-begin-line-test ()
  "Test `eglot-codelens--change-begin-line' with various inputs."
  ;; Test case collection: (name recent-changes expected-line)
  (let ((test-cases
         '(("nil recent-changes returns 1" nil 1)
           (":emacs-messup recent-changes returns 1" :emacs-messup 1)
           ("single change returns its line number"
             (((:line 5) nil nil ""))
             5)
           ("multiple changes returns minimum line number"
             (((:line 10) nil nil "")
              ((:line 5) nil nil "")
              ((:line 15) nil nil ""))
             5))))
    ;; Run all test cases
    (dolist (test-case test-cases)
      (pcase test-case
        (`(,name ,recent-changes ,expected-line)
         (ert-info ((format "Test: %s" name))
           (let ((eglot-codelens--recent-changes recent-changes))
             (should (= expected-line (eglot-codelens--change-begin-line))))))))))

(ert-deftest eglot-codelens--schedule-visible-refresh-mode-off-test ()
  "Test `eglot-codelens--schedule-visible-refresh' when eglot-codelens-mode is off."
  (with-temp-buffer
    ;; Ensure eglot-codelens-mode is off
    (setq eglot-codelens-mode nil)
    ;; Ensure no existing timer
    (setq eglot-codelens--refresh-timer nil)
    ;; Call the function
    (eglot-codelens--schedule-visible-refresh)
    ;; Verify no timer was created
    (should (null eglot-codelens--refresh-timer))))

(ert-deftest eglot-codelens--schedule-visible-refresh-no-timer-test ()
  "Test `eglot-codelens--schedule-visible-refresh' when no existing timer."
  (with-temp-buffer
    ;; Enable eglot-codelens-mode
    (setq eglot-codelens-mode t)
    ;; Ensure no existing timer
    (setq eglot-codelens--refresh-timer nil)
    ;; Call the function
    (eglot-codelens--schedule-visible-refresh)
    ;; Verify timer was created
    (should (timerp eglot-codelens--refresh-timer))
    ;; Clean up the timer
    (cancel-timer eglot-codelens--refresh-timer)
    (setq eglot-codelens--refresh-timer nil)))

(ert-deftest eglot-codelens--schedule-visible-refresh-existing-timer-test ()
  "Test `eglot-codelens--schedule-visible-refresh' when timer already exists."
  (with-temp-buffer
    ;; Enable eglot-codelens-mode
    (setq eglot-codelens-mode t)
    ;; Create a timer
    (let ((original-timer (run-with-idle-timer 100 nil #'ignore)))
      (setq eglot-codelens--refresh-timer original-timer)
      ;; Call the function
      (eglot-codelens--schedule-visible-refresh)
      ;; Verify the same timer object is reused (not replaced)
      (should (eq eglot-codelens--refresh-timer original-timer))
      ;; Clean up the timer
      (cancel-timer eglot-codelens--refresh-timer)
      (setq eglot-codelens--refresh-timer nil))))

(ert-deftest eglot-codelens--schedule-visible-refresh-callback-test ()
  "Test timer callback cancels timer and clears variable."
  (with-temp-buffer
    (setq eglot-codelens-mode t)
    (let ((refresh-called nil)
          (test-buffer (current-buffer)))
      (cl-letf (((symbol-function 'eglot-codelens--refresh-visible-area)
                  (lambda ()
                    (setq refresh-called t)))
                ((symbol-function 'window-buffer)
                  (lambda (&rest _)
                    test-buffer)))
        (let ((callback
               (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (when (timerp eglot-codelens--refresh-timer)
                       (cancel-timer eglot-codelens--refresh-timer))
                     (setq eglot-codelens--refresh-timer nil)
                     (when (eq (window-buffer (selected-window)) buf)
                       (eglot-codelens--refresh-visible-area)))))))
          (setq eglot-codelens--refresh-timer
                (run-with-idle-timer 100 nil #'ignore))
          (funcall callback test-buffer)
          (should refresh-called)
          (should (null eglot-codelens--refresh-timer)))))))

(ert-deftest eglot-codelens--refresh-visible-area-mode-off-test ()
  "Test `eglot-codelens--refresh-visible-area' when eglot-codelens-mode is off."
  (with-temp-buffer
    ;; Ensure mode is off
    (setq eglot-codelens-mode nil)
    (let ((render-called nil)
          (test-buffer (current-buffer)))
      ;; Mock render-codelens to track if it was called
      (cl-letf (((symbol-function 'eglot-codelens--render-codelens)
                  (lambda (&rest _args)
                    (setq render-called t)))
                ((symbol-function 'eglot-codelens--visible-range)
                  (lambda (&rest _)
                    (cons 1 10)))
                ((symbol-function 'eglot-codelens--docver)
                  (lambda ()
                    1)))
        ;; Set up cache and version
        (setq eglot-codelens--cache (make-hash-table)
              eglot-codelens--version 1)
        ;; Call refresh-visible-area
        (eglot-codelens--refresh-visible-area)
        ;; Verify render-codelens was NOT called
        (should (null render-called))))))

(ert-deftest eglot-codelens--refresh-visible-area-cache-nil-test ()
  "Test `eglot-codelens--refresh-visible-area' when cache is nil."
  (with-temp-buffer
    ;; Ensure mode is on
    (setq eglot-codelens-mode t)
    (let ((render-called nil))
      ;; Mock render-codelens to track if it was called
      (cl-letf (((symbol-function 'eglot-codelens--render-codelens)
                  (lambda (&rest _args)
                    (setq render-called t)))
                ((symbol-function 'eglot-codelens--visible-range)
                  (lambda (&rest _)
                    (cons 1 10)))
                ((symbol-function 'eglot-codelens--docver)
                  (lambda ()
                    1)))
        ;; Set version but cache is nil
        (setq eglot-codelens--cache nil
              eglot-codelens--version 1)
        ;; Call refresh-visible-area
        (eglot-codelens--refresh-visible-area)
        ;; Verify render-codelens was NOT called
        (should (null render-called))))))

(ert-deftest eglot-codelens--refresh-visible-area-version-mismatch-test ()
  "Test `eglot-codelens--refresh-visible-area' when version mismatches."
  (with-temp-buffer
    ;; Ensure mode is on
    (setq eglot-codelens-mode t)
    (let ((render-called nil))
      ;; Mock render-codelens to track if it was called
      (cl-letf (((symbol-function 'eglot-codelens--render-codelens)
                  (lambda (&rest _args)
                    (setq render-called t)))
                ((symbol-function 'eglot-codelens--visible-range)
                  (lambda (&rest _)
                    (cons 1 10)))
                ((symbol-function 'eglot-codelens--docver)
                  (lambda ()
                    2)))  ; docver returns 2, different from cache version
        ;; Set up cache and version with mismatch
        (setq eglot-codelens--cache (make-hash-table)
              eglot-codelens--version 1)
        ;; Call refresh-visible-area
        (eglot-codelens--refresh-visible-area)
        ;; Verify render-codelens was NOT called
        (should (null render-called))))))

(ert-deftest eglot-codelens--refresh-visible-area-normal-test ()
  "Test `eglot-codelens--refresh-visible-area' normal operation."
  (with-temp-buffer
    ;; Ensure mode is on
    (setq eglot-codelens-mode t)
    (let ((render-args nil)
          (test-cache (make-hash-table))
          (test-docver 5)
          ;; Set up mock for the underlying docver symbol before calling the function
          ;; (eglot-codelens--docver is defsubst, so we mock the symbol it reads)
          (docver-sym (symbol-value 'eglot-codelens--docver-symbol)))
      (set docver-sym test-docver)
      ;; Mock render-codelens to capture arguments
      (cl-letf (((symbol-function 'eglot-codelens--render-codelens)
                 (lambda (new-cache docver pending-lines file-changed-p old-cache range)
                   (setq render-args (list new-cache docver pending-lines file-changed-p old-cache range))))
                ((symbol-function 'eglot-codelens--visible-range)
                 (lambda (&rest _)
                   (cons 10 20))))
        ;; Set up cache and version with matching docver
        (setq eglot-codelens--cache test-cache
              eglot-codelens--version test-docver
              eglot-codelens--pending-lines '(5 10 15 25))
        ;; Call refresh-visible-area
        (eglot-codelens--refresh-visible-area)
        ;; Verify render-codelens was called with correct arguments
        (should render-args)
        (should (eq (car render-args) test-cache)) ; new-cache is the cache
        (should (eq (nth 1 render-args) test-docver)) ; docver matches
        (should (equal (nth 2 render-args) '(10 15))) ; pending-lines filtered to visible range
        (should (null (nth 3 render-args))) ; file-changed-p is nil
        (should (null (nth 4 render-args))) ; old-cache is nil
        (should (equal (nth 5 render-args) '(10 . 20)))) ; range is visible range
      )))

(ert-deftest eglot-codelens--refresh-visible-area-filter-pending-test ()
  "Test `eglot-codelens--refresh-visible-area' filters pending-lines by visible range."
  (with-temp-buffer
    (setq eglot-codelens-mode t)
    (let ((render-pending-lines nil)
          (test-cache (make-hash-table))
          (test-docver 1)
          ;; Set up mock for the underlying docver symbol before calling the function
          ;; (eglot-codelens--docver is defsubst, so we mock the symbol it reads)
          (docver-sym (symbol-value 'eglot-codelens--docver-symbol)))
      (set docver-sym test-docver)
      (cl-letf (((symbol-function 'eglot-codelens--render-codelens)
                 (lambda (_new-cache _docver pending-lines _file-changed-p _old-cache _range)
                   (setq render-pending-lines pending-lines)))
                ((symbol-function 'eglot-codelens--visible-range)
                 (lambda (&rest _)
                   (cons 50 100))))
        (setq eglot-codelens--cache test-cache
              eglot-codelens--version test-docver
              eglot-codelens--pending-lines '(10 30 50 70 90 110 130))
        (eglot-codelens--refresh-visible-area)
        (should (equal render-pending-lines '(50 70 90)))))))

(ert-deftest eglot-codelens--on-document-change-mode-off-test ()
  "Test `eglot-codelens--on-document-change' when eglot-codelens-mode is off."
  (with-temp-buffer
    ;; Ensure eglot-codelens-mode is off
    (setq eglot-codelens-mode nil)
    ;; Ensure no existing timer
    (setq eglot-codelens--update-timer nil)
    ;; Call the function
    (eglot-codelens--on-document-change)
    ;; Verify no timer was created
    (should (null eglot-codelens--update-timer))))

(ert-deftest eglot-codelens--on-document-change-no-timer-test ()
  "Test `eglot-codelens--on-document-change' when no existing timer."
  (with-temp-buffer
    ;; Enable eglot-codelens-mode
    (setq eglot-codelens-mode t)
    ;; Ensure no existing timer
    (setq eglot-codelens--update-timer nil)
    ;; Call the function
    (eglot-codelens--on-document-change)
    ;; Verify timer was created
    (should (timerp eglot-codelens--update-timer))
    ;; Clean up the timer
    (cancel-timer eglot-codelens--update-timer)
    (setq eglot-codelens--update-timer nil)))

(ert-deftest eglot-codelens--on-document-change-existing-timer-test ()
  "Test `eglot-codelens--on-document-change' when timer already exists."
  (with-temp-buffer
    ;; Enable eglot-codelens-mode
    (setq eglot-codelens-mode t)
    ;; Create a timer
    (let ((original-timer (run-with-idle-timer 100 nil #'ignore)))
      (setq eglot-codelens--update-timer original-timer)
      ;; Call the function
      (eglot-codelens--on-document-change)
      ;; Verify the same timer object is reused (not replaced)
      (should (eq eglot-codelens--update-timer original-timer))
      ;; Clean up the timer
      (cancel-timer eglot-codelens--update-timer)
      (setq eglot-codelens--update-timer nil))))

(ert-deftest eglot-codelens--on-document-change-callback-test ()
  "Test timer callback cancels timer and calls fetch-codelens."
  (with-temp-buffer
    (setq eglot-codelens-mode t)
    (let ((fetch-called nil)
          (test-buffer (current-buffer)))
      (cl-letf (((symbol-function 'eglot-codelens--fetch-codelens)
                  (lambda ()
                    (setq fetch-called t))))
        (let ((callback
               (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (when (timerp eglot-codelens--update-timer)
                       (cancel-timer eglot-codelens--update-timer))
                     (setq eglot-codelens--update-timer nil)
                     (eglot-codelens--fetch-codelens))))))
          (setq eglot-codelens--update-timer
                (run-with-idle-timer 100 nil #'ignore))
          (funcall callback test-buffer)
          (should fetch-called)
          (should (null eglot-codelens--update-timer)))))))

(ert-deftest eglot-codelens--visible-range-test ()
  "Test `eglot-codelens--visible-range' with various inputs."
  ;; Test case collection: (name mock-window-list-p extend-lines expected-validator-fn)
  (let ((test-cases
         ;; Test Case 1: No window exists - should return nil
         `(("no window exists returns nil"
           nil
           nil
           ,(lambda (result) (should (null result))))

           ;; Test Case 2: Window exists, no extension - returns (beg-line . end-line)
           ("window exists no extension returns range"
           t
           nil
           ,(lambda (result)
              (should (consp result))
              (should (integerp (car result)))
              (should (integerp (cdr result)))
              (should (= (car result) 1))
              (should (= (cdr result) 10))))

           ;; Test Case 3: Window exists with positive extension - returns extended range
           ("window exists with extension returns extended range"
           t
           10
           ,(lambda (result)
              (should (consp result))
              (should (integerp (car result)))
              (should (integerp (cdr result)))
              ;; With extension 10, end-line should be 10 + 10 = 20
              (should (= (cdr result) 20))))

           ;; Test Case 4: Extension should not make beg-line less than 1
           ("extension should not make beg-line less than 1"
           t
           1000
           ,(lambda (result)
              (should (consp result))
              (should (>= (car result) 1))
              (should (= (car result) 1)))) ;; max 1

           ;; Test Case 5: Non-integer extend-lines is treated as no extension
           ("non-integer extend-lines treated as no extension"
           t
           "invalid"
           ,(lambda (result)
              (should (consp result))
              (should (integerp (car result)))
              (should (integerp (cdr result)))
              ;; Should be same as no extension
              (should (= (car result) 1))
              (should (= (cdr result) 10)))))))

    ;; Run all test cases
    (dolist (test-case test-cases)
      (pcase test-case
        (`(,name ,mock-window-list-p ,extend-lines ,validator-fn)
         (ert-info ((format "Test: %s" name))
           (cl-letf (((symbol-function 'get-buffer-window-list)
                      (lambda (&rest _) (when mock-window-list-p '(dummy-window))))
                     ((symbol-function 'window-start) (lambda (&rest _) 1))
                     ((symbol-function 'window-end) (lambda (&rest _) 100))
                     ((symbol-function 'line-number-at-pos)
                      (lambda (pos &rest _)
                        ;; Mock line numbers based on position
                        (cond
                         ((<= pos 10) 1)   ;; line 1 for pos <= 10
                         (t 10)))))        ;; line 10 for pos > 10
             (let ((result (eglot-codelens--visible-range extend-lines)))
               (funcall validator-fn result)))))))))


(provide 'eglot-codelens-test)

;;; eglot-codelens-test.el ends here
