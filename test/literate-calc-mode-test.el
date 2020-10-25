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
      (should (equal (concat "= " calculation " => " (calc-eval calculation))
                     (buffer-string))))))

(ert-deftest literate-calc-mode-test/insert-named-results-test ()
  (with-temp-buffer
    (literate-calc-mode)
    (let ((name "Foo Bar")
          (calculation "1 + 1"))
      (insert name " = " calculation)
      (literate-calc-insert-results)
      (should (equal (concat name " = " calculation
                             " => " name ": " (calc-eval calculation))
                     (buffer-string))))))

(provide 'literate-calc-mode-test)

;;; literate-calc-mode-test.el ends here
