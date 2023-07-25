;;; literate-calc-mode-test.el --- tests for literate-calc-mode.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for literate-calc-mode.

;;; Code:

(require 'ert)
(require 'literate-calc-mode)

(ert-deftest literate-calc-mode-test/sanity-test ()
  (should (equal 1 1)))

(ert-deftest literate-calc-mode-test/insert-results-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((calculation "1 + 1"))
      (insert "= " calculation)
      (literate-calc-insert-results)
      (should (equal (concat "= " calculation " => " (literate-calc--eval calculation))
                     (buffer-string))))))

(ert-deftest literate-calc-mode-test/insert-named-results-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((name "Foo Bar")
          (calculation "1 + 1"))
      (insert name " = " calculation)
      (literate-calc-insert-results)
      (should (equal (concat name " = " calculation
                             " => " name ": " (literate-calc--eval calculation))
                     (buffer-string))))))

(ert-deftest literate-calc-mode/remove-results-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((input "= 1 + 1"))
      (insert input)
      (literate-calc-insert-results)
      (literate-calc-remove-results nil nil)
      (should (equal input (buffer-string))))))

(ert-deftest literate-calc-mode/remove-named-results-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((input "Foo Bar = 1 + 1"))
      (insert input)
      (literate-calc-insert-results)
      (literate-calc-remove-results nil nil)
      (should (equal input (buffer-string))))))

(ert-deftest literate-calc-mode/remove-negative-results-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((input "= 1 - 2.5"))
      (insert input)
      (literate-calc-insert-results)
      (literate-calc-remove-results nil nil)
      (should (equal input (buffer-string))))))

(ert-deftest literate-calc-mode/remove-negative-named-results-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((input "Foo Bar = 1 - 2.5"))
      (insert input)
      (literate-calc-insert-results)
      (literate-calc-remove-results nil nil)
      (should (equal input (buffer-string))))))

(ert-deftest literate-calc-mode/remove-results-several-lines-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((first-line "= 1 + 1")
          (second-line "Foo Bar = 2 + 2")
          (third-line "= 3 + 3"))
      (insert first-line "\n" second-line "\n" third-line)
      (literate-calc-insert-results)
      (literate-calc-remove-results nil nil)
      (should (equal (concat first-line "\n"
                             second-line "\n"
                             third-line)
                     (buffer-string))))))

(ert-deftest literate-calc-mode/remove-results-array-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((input "Foo Bar = [1 2 3]"))
      (insert input)
      (literate-calc-insert-results)
      (literate-calc-remove-results nil nil)
      (should (equal input (buffer-string))))))

(ert-deftest literate-calc-mode/remove-results-in-region-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((first-line "= 1 + 1")
          (second-line "= 2 + 2")
          (third-line "= 3 + 3"))
      (insert first-line "\n" second-line "\n" third-line)
      (literate-calc-insert-results)
      (goto-char (point-min))
      (forward-line 1)
      (transient-mark-mode)
      (push-mark (line-beginning-position) t t)
      (goto-char (line-end-position))
      (call-interactively #'literate-calc-remove-results nil)
      (should (equal (concat first-line " => 2\n"
                             second-line "\n"
                             third-line " => 6")
                     (buffer-string))))))

(ert-deftest literate-calc-mode/remove-results-in-region-last-line-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((first-line "= 1 + 1")
          (second-line "= 2 + 2")
          (third-line "= 3 + 3"))
      (insert first-line "\n" second-line "\n" third-line)
      (literate-calc-insert-results)
      (goto-char (point-min))
      (forward-line 2)
      (transient-mark-mode)
      (push-mark (line-beginning-position) t t)
      (goto-char (line-end-position))
      (call-interactively #'literate-calc-remove-results nil)
      (should (equal (concat first-line " => 2\n"
                             second-line " => 4\n"
                             third-line)
                     (buffer-string))))))

(ert-deftest literate-calc-mode/execute-output-test ()
  (should (equal (org-babel-execute:literate-calc
                  "= 1 + 1"
                  '((:results . "output")))
                 "= 1 + 1 => 2")))

(ert-deftest literate-calc-mode/execute-value-test ()
  (should (equal (org-babel-execute:literate-calc
                  "= 1 + 1"
                  '((:results . "value")))
                 "2")))

(ert-deftest literate-calc-mode/execute-vars-test ()
  (should (equal (org-babel-execute:literate-calc
                  "= a + b"
                  '((:var a . 38)
                    (:var b . 4)
                    (:results . "value")))
                 "42")))

(ert-deftest literate-calc-mode/execute-empty-output-test ()
  (should (equal (org-babel-execute:literate-calc
                  "something"
                  '((:results . "output")))
                 "something")))

(ert-deftest literate-calc-mode/execute-empty-value-test ()
  (should (equal (org-babel-execute:literate-calc
                  "something"
                  '((:results . "value")))
                 nil)))

(ert-deftest literate-calc-mode/digit-separator-test ()
  (should (equal "9,001"
                 (literate-calc--eval "9001"))))

(ert-deftest literate-calc-mode/digit-separator-usage-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((value "9001"))
      (insert "x = " value)
      (insert "\n")
      (insert "y = x")
      (literate-calc-insert-results)
      (should (equal (concat "x = " value " => x: " (literate-calc--eval value)
                             "\n"
                             "y = x => y: " (literate-calc--eval value))
                     (buffer-string))))))

(ert-deftest literate-calc-mode/radix-mode-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (insert "= 9001")
    (literate-calc-set-radix 16)
    (literate-calc-insert-results)
    (should (equal "= 9001 => 16#2329" (buffer-string)))))

(ert-deftest literate-calc-mode/should-always-start-minor-mode-test ()
  (let ((literate-calc-mode-max-buffer-size 0))
    (with-temp-buffer
      (fundamental-mode)
      (insert "1234567890")
      (literate-calc-minor-mode)
      (should literate-calc-minor-mode))))

(ert-deftest literate-calc-mode/should-start-minor-mode-test ()
  (let ((literate-calc-mode-max-buffer-size 10))
    (with-temp-buffer
      (fundamental-mode)
      (insert "1234567890")
      (literate-calc-minor-mode)
      (should literate-calc-minor-mode))))

(ert-deftest literate-calc-mode/should-not-start-minor-mode-test ()
  (let ((literate-calc-mode-max-buffer-size 10))
    (with-temp-buffer
      (fundamental-mode)
      (insert "12345678901")
      (literate-calc-minor-mode)
      (should (not literate-calc-minor-mode)))))

;; If in a function, don't expand variable names to avoid clobbering
;; the function.
(ert-deftest literate-calc-mode-test/variables-overlapping-longer-function-names-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (insert "s = sqrt(25)\n= sqrt(25)")
    (literate-calc-insert-results)
    (should (equal "s = sqrt(25) => s: 5\n= sqrt(25) => 5"
                   (buffer-string)))))

(ert-deftest literate-calc-mode-test/variables-overlapping-shorter-function-names-test ()
  "If a variable name overlaps with a function name, in this case
`year', still expand the variable if it's longer than the
function."
  (with-temp-buffer
    (literate-calc-mode)
    (insert "yearly = 25\n= yearly * 2")
    (literate-calc-insert-results)
    (should (equal "yearly = 25 => yearly: 25\n= yearly * 2 => 50"
                   (buffer-string)))))

(provide 'literate-calc-mode-test)

;;; literate-calc-mode-test.el ends here
