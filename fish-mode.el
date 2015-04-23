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

;;; Code:

;;; Syntax highlighting

(defconst fish-font-lock-keywords-1
  (list

   ;; Builtins
   `( ,(rx symbol-start
	   (or
	    "alias"
	    "and"
	    "bg"
	    "bind"
	    "block"
	    "breakpoint"
	    "builtin"
	    "cd"
	    "commandline"
	    "command"
	    "complete"
	    "contains"
	    "count"
	    "dirh"
	    "dirs"
	    "echo"
	    "emit"
	    "eval"
	    "exec"
	    "fg"
	    "fish_config"
	    "fishd"
	    "fish_indent"
	    "fish_pager"
	    "fish_prompt"
	    "fish_right_prompt"
	    "fish"
	    "fish_update_completions"
	    "funced"
	    "funcsave"
	    "functions"
	    "help"
	    "history"
	    "isatty"
	    "jobs"
	    "math"
	    "mimedb"
	    "nextd"
	    "open"
	    "or"
	    "popd"
	    "prevd"
	    "psub"
	    "pushd"
	    "pwd"
	    "random"
	    "read"
	    "set_color"
	    "source"
	    "status"
	    "test"
	    "trap"
	    "type"
	    "ulimit"
	    "umask"
	    "vared"
	    )
	   symbol-end)
      .
      font-lock-builtin-face)

   ;; Keywords
   `( ,(rx symbol-start
	   (or
	    "begin"
	    "break"
	    "case"
	    "continue"
	    "else"
	    "end"
	    "exit"
	    "for"
	    "function"
	    "if"
	    "return"
	    "set"
	    "switch"
	    "while"
	    )
	   symbol-end)
      .
      font-lock-keyword-face)


   ;; Function name
   `( ,(rx symbol-start "function"
	   (1+ space)
	   (group (1+ (or alnum (syntax symbol)))) symbol-end)
      1
      font-lock-function-name-face)

   ;; Variable definition
   `( ,(rx
	symbol-start (or (and "set"
			      (1+ space)
			      (optional "-" (repeat 1 2 letter) (1+ space)))
			 (and "for" (1+ space)))
	(group (1+ (or alnum (syntax symbol)))))
      1
      font-lock-variable-name-face)

   ;; Variable substitution
   `( ,(rx
	symbol-start (group "$") (group (1+ (or alnum (syntax symbol)))) symbol-end)
      (1 font-lock-string-face)
      (2 font-lock-variable-name-face))

   ;; Negation
   `( ,(rx symbol-start
	   (or (and (group "not")
		    symbol-end)))
      1
      font-lock-negation-char-face)

   ;; Important
   `( ,(rx symbol-start (and "set"
			     (1+ space)
			     (group (and "-" (repeat 1 2 letter)))
			     (1+ space)))
      1
      font-lock-negation-char-face)))

(defvar fish-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\# "<" tab)
    (modify-syntax-entry ?\n ">" tab)
    (modify-syntax-entry ?\" "\"\"" tab)
    (modify-syntax-entry ?\' "\"'" tab)
    tab)
  "Syntax table for `fish-mode'.")

;;; Indentation

(defun what-line-number ()
  "Returns the current line number of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun fish-get-normal-indent ()
  (interactive)
  (let ((not-indented t) cur-indent)
    (while not-indented
      ;; move up
      (forward-line -1)
      (cond
       ;; found block-opening term, so increase indentation level by tab-width
       ((looking-at "[ \t]*\\(if\\|else\\|function\\|while\\|for\\|begin\\|switch\\|case\\)")
        (setq cur-indent (+ (current-indentation) tab-width))
        (setq not-indented nil))

       ;; found empty line, so just skip it
       ((looking-at "[ \t]*$"))

       ;; default case, so return indentation level of current line
       (t
        (setq cur-indent (current-indentation))
        (setq not-indented nil))))
    cur-indent))

(defun fish-get-case-indent ()
  (interactive)
  (let ((not-indented t) cur-indent)
    (while not-indented
      ;; move up
      (forward-line -1)
      (cond
       ;; found 'switch', so increase indentation level by tab-width
       ((looking-at "[ \t]*\\(switch\\)")
        (setq cur-indent (+ (current-indentation) tab-width))
        (setq not-indented nil))

       ;; found another 'case', so return it's indentation level
       ((looking-at "[ \t]*\\(case\\)")
        (setq cur-indent (current-indentation))
        (setq not-indented nil))

       ;; found empty line, so just skip it
       ((looking-at "[ \t]*$"))

       ;; default case, so return indentation level of current line - tab-width
       (t
        (setq cur-indent (- (current-indentation) tab-width))
        (setq not-indented nil))))
    cur-indent))

(defun fish-get-end-indent ()
  (interactive)
  (let (cur-indent (count-of-ends 1))
    (while (not (eq count-of-ends 0))
      ;; move up
      (forward-line -1)
      (cond
       ;; found block-opening term, so check if it matches to our end
       ((looking-at "[ \t]*\\(if\\|function\\|while\\|for\\|begin\\|switch\\)")
        (setq count-of-ends (- count-of-ends 1))
        (if (eq count-of-ends 0)
            ;; block-opening term matches, so return it's indentation level
            (progn (setq cur-indent (current-indentation))
                   (setq pair-not-found nil))
          ;; block-opening term does not match, so seek further
          ))

       ;; found another 'end', so increase count of 'end' terms
       ((looking-at "[ \t]*\\(end\\)")
        (setq count-of-ends (+ count-of-ends 1)))

       ;; nothing interesting found, so seek further
       (t)))
    cur-indent))

(defun fish-indent-line ()
  "Indent current line"
  (interactive)

  (if (bobp)
      (indent-line-to 0)
    (let (cur-indent (rpos (- (point-max) (point))))
      (save-excursion
        (beginning-of-line)
        (cond
         ;; already on line 1, so leave it alone
         ((eq (what-line-number) 1)
          (setq cur-indent (current-indentation)))

         ;; found 'end' - need to move back based on level of matching pair
         ((looking-at "[ \t]*\\(end\\)")
          (setq cur-indent (fish-get-end-indent)))

         ;; found 'case' - need to move forth based on matching switch
         ((looking-at "[ \t]*\\(case\\)")
          (setq cur-indent (fish-get-case-indent)))

         ;; found 'else' - like default condition, but also move left
         ((looking-at "[ \t]*\\(else\\)")
          (setq cur-indent (- (fish-get-normal-indent) tab-width)))

         ;; default case - indent based on previous non-empty line
         (t
          (setq cur-indent (fish-get-normal-indent)))))
      (if (< cur-indent 0) (setq cur-indent 0))
      (indent-line-to cur-indent)
      (if (> (- (point-max) rpos) (point))
          (goto-char (- (point-max) rpos))))))

;;; Mode definition

;;;###autoload
(define-derived-mode fish-mode prog-mode "Fish"
  "Major mode for editing fish shell files."
  :syntax-table fish-syntax-table
  (setq-local indent-line-function 'fish-indent-line)
  (setq-local font-lock-defaults '(fish-font-lock-keywords-1))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*"))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("fish" . fish-mode))

(provide 'fish-mode)

;;; fish-mode.el ends here
