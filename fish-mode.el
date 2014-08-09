;;; fish-mode.el --- Major mode for fish shell scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Tony Wang

;; Author: Tony Wang <wwwjfy@gmail.com>
;; Keywords: Fish, shell
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
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

;; A very basic version of major mode for fish shell scripts.
;; Current features:
;;
;;  - keyword highlight
;;  - basic indent
;;  - comment detection
;;
;; TODO:
;;
;;  - more efficient grammar parse for indent

;;; Code:

(defcustom fish-mode-hook nil
  "Hook of fish-mode")

(defconst fish-font-lock-keywords-1
  (list
   '("\\<\\(a\\(?:lias\\|nd\\)\\|b\\(?:egin\\|g\\|ind\\|lock\\|reak\\(?:point\\)?\\|uiltin\\)\\|c\\(?:ase\\|d\\|o\\(?:m\\(?:mand\\(?:line\\)?\\|plete\\)\\|nt\\(?:ains\\|inue\\)\\|unt\\)\\)\\|d\
ir[hs]\\|e\\(?:cho\\|lse\\|mit\\|nd\\|val\\|x\\(?:ec\\|it\\)\\)\\|f\\(?:g\\|ish\\(?:_\\(?:config\\|indent\\|p\\(?:ager\\|rompt\\)\\|right_prompt\\|update_completions\\)\\|d\\)?\\|or\\|\
unc\\(?:ed\\|save\\|tions?\\)\\)\\|h\\(?:elp\\|istory\\)\\|i\\(?:f\\|satty\\)\\|jobs\\|m\\(?:ath\\|imedb\\)\\|n\\(?:extd\\|ot\\)\\|o\\(?:pen\\|r\\)\\|p\\(?:opd\\|revd\\|sub\\|\\(?:ush\\
\|w\\)d\\)\\|r\\(?:andom\\|e\\(?:ad\\|turn\\)\\)\\|s\\(?:et\\(?:_color\\)?\\|ource\\|tatus\\|witch\\)\\|t\\(?:est\\|rap\\|ype\\)\\|u\\(?:limit\\|mask\\)\\|vared\\|while\\)\\>"
    . font-lock-builtin-face)
   '("\\$\\([[:alpha:]_][[:alnum:]_]*\\)" . font-lock-variable-name-face)))

(defvar fish-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\# "<" tab)
    (modify-syntax-entry ?\n ">" tab)
    (modify-syntax-entry ?\" "\"\"" tab)
    (modify-syntax-entry ?\' "\"'" tab)
    tab))

(defun fish-indent-line ()
  "Indent current line."
  (interactive)
  (beginning-of-line)

  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(end\\|else\\)")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*\\(end\\)")
                (progn
                  (setq cur-indent (current-indentation))
                  (if (< cur-indent 0)
                      (setq cur-indent 0))
                  (setq not-indented nil))
              (if (looking-at "[ \t]*\\(begin\\|case\\|else\\|for\\|function\\|if\\|switch\\|while\\)\\>")
                  (if (not (looking-at ".*end$"))
                      (progn
                        (setq cur-indent (+ (current-indentation) tab-width))
                        (setq not-indented nil)))
                (if (bobp)
                    (setq not-indented nil)))))))
        (if cur-indent
            (indent-line-to cur-indent)
          (indent-line-to 0)))))

;;;###autoload
(define-derived-mode fish-mode prog-mode "Fish"
  "Major mode for editing fish shell files."
  (setq-local indent-line-function 'fish-indent-line)
  (setq-local font-lock-defaults '(fish-font-lock-keywords-1))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*"))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'interpreter-mode-alist '("fish" . fish-mode))

(provide 'fish-mode)

;;; fish-mode.el ends here
