;;; literate-calc-mode.el --- Inline results from calc -*- lexical-binding: t -*-

;; Author: Robin Schroer
;; Maintainer: Robin Schroer
;; Version: 0.1
;; Homepage: https://github.com/sulami/literate-calc-mode.el
;; Package-Requires: ((emacs "25.1") (s "1.12.0"))
;; Keywords: calc, languages, tools


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Major/minor mode for literate calculations.

;;; Code:

(require 'calc)
(require 'cl-lib)
(require 'ob)
(require 'org-element)
(require 'rx)
(require 's)
(require 'subr-x)
(require 'thingatpt)

;; TODO semantic highlighting
;; TODO org export

(defgroup literate-calc-mode nil
  "Display inline results from calc."
  :group 'editing
  :prefix "literate-calc-mode-")

(defcustom literate-calc-mode-inhibit-line-functions '(literate-calc-mode-inhibit-in-src-blocks)
  "Hook functions called for each line to test whether to inhibit calculation.

If any of these functions returns non-nil, overlays will not be displayed."
  :group 'literate-calc-mode
  :type 'hook)

(defcustom literate-calc-mode-idle-time 1
  "How long to wait after typing to recalculate results.

A larger value can prevent lagginess in larger buffers by only
recalculating once the buffer contents have settled."
  :group 'literate-calc-mode
  :type '(choice integer
                 float))

(defun literate-calc-mode-inhibit-in-src-blocks ()
  "Return non-nil if point is in a source block."
  (and (derived-mode-p #'org-mode)
       (memq (org-element-type (org-element-context))
             '(inline-src-block src-block))))

(defvar-local literate-calc-minor-mode nil)
(defvar-local literate-calc--scope (list))
(defvar-local literate-calc--idle-timer nil)

(defconst literate-calc--expression
  (rx string-start
      (opt (1+ (or alphanumeric
                   blank
                   (any "-_"))))
      "="
      (1+ (not (any ?=)))
      string-end))

(defconst literate-calc--result
  (rx " => "
      (opt (+ (any alphanumeric blank "-_")) ": ")
      (opt "-")
      (+? anything)
      line-end))

(defmacro literate-calc--without-hooks (&rest body)
  "Run BODY with deactivated edit hooks."
  `(let ((hooks-active (or (equal major-mode #'literate-calc-mode)
                           literate-calc-minor-mode)))
     (when hooks-active
       ;; Temporarily disable the edit hooks while we edit the buffer.
       (literate-calc--exit))
     ,@body
     (when hooks-active
       (literate-calc--setup-hooks))))

(defun literate-calc--format-result (name result)
  "Return the output format for RESULT with the optional NAME.

NAME should be an empty string if RESULT is not bound."
  (if (string-empty-p name)
      (format " => %s" result)
    (format " => %s: %s" name result)))

(defun literate-calc--insert-result (name result)
  "Insert NAME & RESULT at the end of the current line."
  (save-excursion
    (end-of-line)
    (insert (literate-calc--format-result name result))))

(defun literate-calc--create-overlay (name result)
  "Create an overlay for NAME & RESULT on the current line."
  (let* ((o (make-overlay (line-beginning-position)
                          (line-end-position)
                          nil
                          t
                          t)))
    (overlay-put o 'literate-calc t)
    (overlay-put o 'evaporate t)
    (overlay-put o 'after-string
                 (propertize
                  (literate-calc--format-result name result)
                  'face 'font-lock-comment-face
                  'cursor t))))

(defun literate-calc--process-line (line variable-scope &optional insert)
  "Parse LINE using VARIABLE-SCOPE and maybe add a result.

If INSERT is true, insert the result in the buffer, otherwise create
an overlay.

Returns 'nil' if the line is not a calc expression.
Returns 'nil' if the result is not bound to a name.
Returns a list of (NAME RESULT) if the result is bound to a name."
  (save-match-data
    (when (string-match-p literate-calc--expression line)
      (let* ((whole-line (s-split "=" line))
             (var-name (string-trim (car whole-line)))
             (var-value (string-trim (cadr whole-line)))
             (resolved-value (cl-reduce (lambda (s kv)
                                          (let ((k (car kv))
                                                (v (cadr kv)))
                                            (s-replace k (format "(%s)" v) s)))
                                        variable-scope
                                        :initial-value var-value))
             (var-result (if (string-empty-p resolved-value)
                             "0"
                           (format "%s" (calc-eval resolved-value)))))
        (if insert
            (literate-calc--insert-result var-name var-result)
            (literate-calc--create-overlay var-name var-result))
        (unless (string-empty-p var-name)
          (list var-name var-result))))))

;;;###autoload
(defun literate-calc-clear-overlays ()
  "Remove all literate-calc-mode overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min)
                   (point-max)
                   'literate-calc
                   t)
  (setq-local literate-calc--scope (list)))

(defun literate-calc--add-binding (binding)
  "Add BINDING to the buffer-local variable scope.

Bindings are sorted by length descending to prevent substring
shadowing."
  (when binding
    (setq-local literate-calc--scope
                (cl-merge 'list
                          literate-calc--scope
                          (list binding)
                          (lambda (x y)
                            (<= (length (car y))
                                (length (car x))))))))

;;;###autoload
(defun literate-calc-eval-line ()
  "Evaluate the calc expression on the current line."
  (interactive)
  (unless (string-empty-p (buffer-string))
    (remove-overlays (line-beginning-position)
                     (line-end-position)
                     'literate-calc
                     t)
    (let ((binding (literate-calc--process-line (thing-at-point 'line)
                                                literate-calc--scope)))
      (literate-calc--add-binding binding))))

;;;###autoload
(cl-defun literate-calc-eval-buffer (&optional (buffer (current-buffer)))
  "Evaluate all calc expressions in the current buffer in order.

If BUFFER is set, run in it, otherwise in `current-buffer'."
  (interactive)
  (with-current-buffer buffer
    (literate-calc-clear-overlays)
    (unless (string-empty-p (buffer-string))
      (save-excursion
        (goto-char (point-min))
        (let ((buffer-line-count (count-lines (point-min) (point-max)))
              (line-number 1))
          (while (<= line-number buffer-line-count)
            (unless (run-hook-with-args-until-success 'literate-calc-mode-inhibit-line-functions)
              (let ((binding (literate-calc--process-line (thing-at-point 'line)
                                                          literate-calc--scope)))
                (literate-calc--add-binding binding)))
            (setq line-number (1+ line-number))
            (forward-line 1)))))))

;;;###autoload
(defun literate-calc-insert-results ()
  "Insert results into buffer instead of creating overlays."
  (interactive)
  (unless (string-empty-p (buffer-string))
    (literate-calc--without-hooks
     (save-excursion
       (goto-char (point-min))
       (let ((buffer-line-count (count-lines (point-min) (point-max)))
             (line-number 1))
         (while (<= line-number buffer-line-count)
           (unless (run-hook-with-args-until-success 'literate-calc-mode-inhibit-line-functions)
             (let ((binding (literate-calc--process-line (thing-at-point 'line)
                                                         literate-calc--scope
                                                         t)))
               (literate-calc--add-binding binding)))
           (setq line-number (1+ line-number))
           (forward-line 1)))))))

;;;###autoload
(defun literate-calc-remove-results (start end)
  "Remove inserted results from buffer between START and END."
  (interactive "r")
  (unless (string-empty-p (buffer-string))
    (literate-calc--without-hooks
     (save-excursion
       (let* ((start (if (region-active-p)
                         start
                       (point-min)))
              (end (if (region-active-p)
                       end
                     (point-max)))
              ;; NOTE We are shortening the buffer while looping, so
              ;; `end' actually creeps further towards the end with
              ;; every deletion. We can assume that we don't alter the
              ;; number of lines, so we just bound the search on the
              ;; line number instead of the position. Because marking
              ;; a whole line also technically places `point' in the
              ;; next line, we have to walk back one char to make sure
              ;; we don't overreach by one line. This effectively
              ;; removes an empty line off the end, but doesn't affect
              ;; non-empty lines at the end.
              (end-line (line-number-at-pos (- end 1))))
         (goto-char start)
         (while (re-search-forward literate-calc--result
                                   (save-excursion
                                     (goto-char 1)
                                     (line-end-position end-line))
                                   t)
           (replace-match "" nil nil))))
     (setq-local literate-calc--scope (list)))))

(defun literate-calc--async-eval-buffer (_beg _end _pre-change-length)
  "Schedule `literate-calc-eval-buffer' after some idle time.

The exact timeout is determined by `literate-calc-mode-idle-time'."
  (when literate-calc--idle-timer
    (cancel-timer literate-calc--idle-timer))
  (setq literate-calc--idle-timer
        (run-with-idle-timer literate-calc-mode-idle-time
                             nil
                             #'literate-calc-eval-buffer
                             (current-buffer))))

(defun literate-calc--setup-hooks ()
  "Set up after-edit hooks & run first evaluation."
  (add-hook 'after-change-functions #'literate-calc--async-eval-buffer nil t)
  (literate-calc-eval-buffer))

(defun literate-calc--exit ()
  "Clean up hooks & overlays."
  (when literate-calc--idle-timer
    (cancel-timer literate-calc--idle-timer))
  (remove-hook 'after-change-functions #'literate-calc--async-eval-buffer t)
  (literate-calc-clear-overlays))

(defvar literate-calc-font-lock-defaults)
(setq literate-calc-font-lock-defaults
      (let ((identifier-regexp (rx line-start
                                   (group (1+ (and (or letter
                                                       blank))))
                                   "=")))
        `((,identifier-regexp . (1 font-lock-variable-name-face)))))

;;;###autoload
(define-derived-mode literate-calc-mode fundamental-mode
  "Literate-Calc"
  (setq font-lock-defaults '((literate-calc-font-lock-defaults)))
  (literate-calc--setup-hooks)
  (add-hook 'change-major-mode-hook #'literate-calc--exit nil t))

;;;###autoload
(define-minor-mode literate-calc-minor-mode
  "Evaluates calc expressions"
  :lighter "lit-calc"
  (if literate-calc-minor-mode
      (literate-calc--setup-hooks)
    (literate-calc--exit)))

(defun org-babel-expand-body:literate-calc (body
                                            params
                                            &optional
                                            processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr
	              (cl-remove-if-not (lambda (x) (eq (car x) :var))
                                        processed-params))))
    (concat
     (mapconcat
      (lambda (pair)
        (format "%s = %s" (car pair) (cdr pair)))
      vars "\n")
     (when vars "\n")
     body)))

(defun org-babel-execute:literate-calc (body params)
  "Execute a block of literate-calc code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
         (vars (alist-get :var processed-params))
         (result-type (alist-get :result-type processed-params))
         (full-body (org-babel-expand-body:literate-calc
                     body params processed-params)))
    (with-temp-buffer
      (insert full-body)
      (literate-calc-insert-results)
      (if (equal 'output result-type)
          ;; Return output.
          (buffer-string)
        ;; Return value.
        (progn
          (goto-char (point-max))
          (when-let ((found (re-search-backward literate-calc--result
                                                (point-min)
                                                t)))
            (buffer-substring-no-properties (+ found (length " => "))
                                            (line-end-position))))))))

(provide 'literate-calc-mode)

;;; literate-calc-mode.el ends here
