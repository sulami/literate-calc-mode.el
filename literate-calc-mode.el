;;; literate-calc-mode.el --- calc, but as a document -*- lexical-binding: t -*-

;; Author: Robin Schroer
;; Maintainer: Robin Schroer
;; Version: 0.1
;; Homepage: https://github.com/sulami/literate-calc-mode.el
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
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Major mode for literate calculations.

;;; Code:

(require 'calc)
(require 'cl)
(require 's)

;; TODO semantic highlighting
;; TODO settings
;; TODO org-babel-execute
;; TODO org export

(defun literate-calc--create-overlay (name result)
  (let* ((o (make-overlay (line-beginning-position)
                          (1+ (line-end-position)))))
    (overlay-put o 'after-string
                 (propertize
                  (if (string-empty-p name)
                      (format "=> %s\n" result)
                    (format "=> %s: %s\n" name result))
                  'face 'font-lock-comment-face))))

(defun literate-calc--process-line (line variable-scope)
  (let ((literate-calc--expression (rx string-start
                                      (opt (1+ (or letter
                                                   blank)))
                                      "="
                                      (1+ (not (any ?=)))
                                      string-end)))
    (when (string-match literate-calc--expression line)
      (let* ((whole-line (s-split "=" line))
             (var-name (string-trim (car whole-line)))
             (var-value (string-trim (cadr whole-line)))
             (resolved-value (reduce (lambda (s kv)
                                       (let ((k (car kv))
                                             (v (cadr kv)))
                                         (s-replace k v s)))
                                     variable-scope
                                     :initial-value var-value))
             (var-result (if (string-empty-p resolved-value)
                             "0"
                           (format "%s" (calc-eval resolved-value)))))
        (literate-calc--create-overlay var-name var-result)
        (unless (string-empty-p var-name)
          (list var-name var-result))))))

(defun literate-calc--eval (&rest _)
  (remove-overlays)
  (unless (string-empty-p (buffer-string))
    (save-excursion
      (goto-char (point-min))
      (let ((buffer-line-count (count-lines (point-min) (point-max)))
            (line-number 1)
            (variable-scope (list)))
        (while (<= line-number buffer-line-count)
          (let ((bindings (literate-calc--process-line (thing-at-point 'line)
                                                      variable-scope)))
            (unless (null bindings)
              (setq variable-scope
                    (cl-merge 'list
                              variable-scope
                              (list bindings)
                              (lambda (x y)
                                (<= (length (car y))
                                   (length (car x))))))))
          (setq line-number (1+ line-number))
          (forward-line 1))))))

(setq literate-calc-font-lock-defaults
      (let ((identifier-regexp (rx line-start
                                   (group (1+ (and (or letter
                                                       blank))))
                                   "=")))
        `((,identifier-regexp . (1 font-lock-variable-name-face)))))

(define-derived-mode literate-calc-mode fundamental-mode
  "Literate-Calc"
  (setq font-lock-defaults '((literate-calc-font-lock-defaults)))
  :after-hook
  (progn
    (add-hook 'change-major-mode-hook 'remove-overlays nil t)
    (add-hook 'after-change-functions 'literate-calc--eval nil t)
    (literate-calc--eval)))

(provide 'literate-calc)

;;; literate-calc.el ends here
