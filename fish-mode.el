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
;;  - run fish_indent for indention
;;
;;  To run fish_indent before save, add the following to init script:
;;  (add-hook 'fish-mode-hook (lambda ()
;;                              (add-hook 'before-save-hook 'fish_indent-before-save)))

;;; Code:

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    `(set (make-local-variable ',var) ,val)))

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

;;; Indentation helpers

(defvar fish/block-opening-terms
  (mapconcat
   'identity
   '("\\<if\\>"
     "\\<function\\>"
     "\\<while\\>"
     "\\<for\\>"
     "\\<begin\\>"
     "\\<switch\\>")
   "\\|"))

(defun fish/current-line ()
  "Return the line at point as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun fish/fold (f x list)
  "Recursively applies (F i j) to LIST starting with X.
For example, (fold F X '(1 2 3)) computes (F (F (F X 1) 2) 3)."
  (let ((li list) (x2 x))
    (while li
      (setq x2 (funcall f x2 (pop li))))
    x2))

(defun fish/count-of-tokens-in-string (token token-to-ignore string)
  (let ((count 0)
        (pos 0))
    (while pos
      (if (and token-to-ignore
               (string-match token-to-ignore string pos))
          (setq pos (match-end 0)))
      (if (string-match token string pos)
            (setq pos (match-end 0)
                  count (+ count 1))
        (setq pos nil)))
    count))

(defun fish/at-comment-line? ()
  "Returns t if looking at comment line, nil otherwise."
  (looking-at "[ \t]*#"))

(defun fish/at-empty-line? ()
  "Returns t if looking at empty line, nil otherwise."
  (looking-at "[ \t]*$"))

(defun fish/count-of-opening-terms ()
  (fish/count-of-tokens-in-string fish/block-opening-terms
                                  "\\<else if\\>"
                                  (fish/current-line)))

(defun fish/count-of-end-terms ()
  (fish/count-of-tokens-in-string "\\<end\\>" nil (fish/current-line)))

(defun fish/at-open-block? ()
  "Returns t if line contains block opening term
   that is not closed in the same line, nil otherwise."
  (> (fish/count-of-opening-terms)
     (fish/count-of-end-terms)))

(defun fish/at-open-end? ()
  "Returns t if line contains 'end' term and
   doesn't contain block opening term that matches
   this 'end' term. Returns nil otherwise."
  (> (fish/count-of-end-terms)
     (fish/count-of-opening-terms)))

(defun fish/line-contains-block-opening-term? ()
  "Returns t if line contains block opening term, nil otherwise."
  (fish/at-open-block?))

(defun fish/line-contans-end-term? ()
  "Returns t if line contains end term, nil otherwise."
  (fish/at-open-end?))

(defun fish/line-contains-open-switch-term? ()
  "Returns t if line contains switch term, nil otherwise."
  (> (fish/count-of-tokens-in-string "\\<switch\\>" nil (fish/current-line))
     (fish/count-of-end-terms)))

;;; Indentation

(defun fish-indent-line ()
  "Indent current line."
  ;; start calculating indentation level
  (let ((cur-indent 0)      ; indentation level for current line
        (rpos (- (point-max)
                 (point)))) ; used to move point after indentation :: todo - check if it's possible to avoid this variable
    (save-excursion
      ;; go to beginning of line
      (beginning-of-line)
      ;; check if already at the beginning of buffer
      (unless (bobp)
        (cond
         ;; found comment line
         ;; cur-indent is based on previous non-empty and non-comment line
         ;; todo - answer why we can't move it to default case
         ((fish/at-comment-line?)
          (setq cur-indent (fish-get-normal-indent)))

         ;; found line that starts with 'end'
         ;; this is a special case
         ;; so get indentation level
         ;; from 'fish-get-end-indent function
         ((looking-at "[ \t]*end\\>")
          (setq cur-indent (fish-get-end-indent)))

         ;; found line that stats with 'case'
         ;; this is a special case
         ;; so get indentation level
         ;; from 'fish-get-case-indent
         ((looking-at "[ \t]*case\\>")
          (setq cur-indent (fish-get-case-indent)))

         ;; found line that starts with 'else'
         ;; cur-indent is previous non-empty and non-comment line
         ;; minus tab-width
         ((looking-at "[ \t]*else\\>")
          (setq cur-indent (- (fish-get-normal-indent) tab-width)))

         ;; default case
         ;; cur-indent equals to indentation level of previous
         ;; non-empty and non-comment line
         (t (setq cur-indent (fish-get-normal-indent))))))

    ;; before indenting check cur-indent for negative level
    (if (< cur-indent 0) (setq cur-indent 0))

    ;; indent current line
    (indent-line-to cur-indent)

    ;; shift point to respect previous position
    (if (> (- (point-max) rpos) (point))
        (goto-char (- (point-max) rpos)))))

(defun fish-get-normal-indent ()
  "Returns indentation level based on previous non-empty and non-comment line."
  (let ((cur-indent 0)
        (not-indented t))
    (while (and not-indented
                (not (bobp)))

      ;; move to previous line
      (forward-line -1)

      (cond
       ;; found empty line, so just skip it
       ((fish/at-empty-line?))

       ;; found comment line, so just skip it
       ((fish/at-comment-line?))

       ;; found line that contains an open block
       ;; so increase indentation level
       ((fish/at-open-block?)
        (setq cur-indent (+ (current-indentation)
                            tab-width)
              not-indented nil))

       ;; found line that starts with 'else' or 'case'
       ;; so increase indentation level
       ((looking-at "[ \t]*\\(else\\|case\\)\\>")
        (setq cur-indent (+ (current-indentation) tab-width)
              not-indented nil))

       ;; found a line that starts with 'end'
       ;; so use this line indentation level
       ((looking-at "[ \t]*end\\>")
        (setq cur-indent (current-indentation)
              not-indented nil))

       ;; found a line that contains open 'end' term
       ;; and doesn't start with 'end' (the order matters!)
       ;; it means that this 'end' is indented to the right
       ;; so we need to decrease indentation level
       ((fish/at-open-end?)
        (setq cur-indent (- (current-indentation)
                            tab-width)
              not-indented nil))

       ;; default case
       ;; we just set current indentation level
       (t
        (setq cur-indent (current-indentation)
              not-indented nil))))
    cur-indent))

(defun fish-get-end-indent ()
  "Returns indentation level based on matching block opening term."
  (let ((cur-indent 0)
        (count-of-ends 1))
    (while (not (or (eq count-of-ends 0)
                    (bobp)))

      ;; move to previous line
      (forward-line -1)

      (cond
       ;; found empty line, so just skip it
       ((fish/at-empty-line?))

       ;; found comment line, so just skip it
       ((fish/at-comment-line?))

       ;; we found the line that contains unmatched
       ;; block opening term so decrease the count of end terms
       ((fish/at-open-block?)
        (setq count-of-ends (- count-of-ends 1))
        ;; when count of end terms is zero
        ;; it means that we found matching term that
        ;; opens block
        ;; so cur-indent equals to inden equals to
        ;; indentation level of current line
        (when (eq count-of-ends 0)
          (setq cur-indent (current-indentation))))

       ;; we found new end term
       ;; so just increase the count of end terms
       ((fish/at-open-end?)
        (setq count-of-ends (+ count-of-ends 1)))

       ;; do nothing
       (t)))

    ;; it means that we didn't found a matching pair
    ;; for 'end' term
    (unless (eq count-of-ends 0)
      (error "Found unmatched 'end' term."))

    cur-indent))

(defun fish-get-case-indent ()
  "Returns indentation level based on matching 'switch' term."
  (let ((cur-indent 0)
        (not-indented t))
    (while (and not-indented
                (not (bobp)))
      ;; move to previous line
      (forward-line -1)

      (cond
       ;; found empty line, so just skip it
       ((fish/at-empty-line?))

       ;; found comment line, so just skip it
       ((fish/at-comment-line?))

       ;; line contains switch term
       ;; so cur-indent equials to increased
       ;; indentation level of current line
       ((fish/line-contains-open-switch-term?)
        (setq cur-indent (+ (current-indentation) tab-width)
              not-indented nil))

       ;; do nothing
       (t)))

    ;; it means that we didn't find a matching pair
    (when not-indented
      (error "Found 'case' term without matching 'switch' term"))

    cur-indent))

;;; fish_indent
(defun fish_indent ()
  "Indent current buffer using fish_indent"
  (interactive)
  (let ((current-point (point)))
    (call-process-region (point-min) (point-max) "fish_indent" t t nil)
    (goto-char current-point)
    ))

;;; Mode definition

;;;###autoload
(defun fish_indent-before-save ()
  (interactive)
  (when (eq major-mode 'fish-mode) (fish_indent)))

;;;###autoload
(define-derived-mode fish-mode prog-mode "Fish"
  "Major mode for editing fish shell files."
  :syntax-table fish-mode-syntax-table
  (setq-local indent-line-function 'fish-indent-line)
  (setq-local font-lock-defaults '(fish-font-lock-keywords-1))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("/fish_funced\\..*\\'" . fish-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("fish" . fish-mode))

(provide 'fish-mode)

;;; fish-mode.el ends here
