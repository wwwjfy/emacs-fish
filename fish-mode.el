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
    tab))

(defun fish-swallow-block ()
  "move backward line til begin of the block"
  (let ((not-done t))
    (while not-done
      (forward-line -1)
      (if (looking-at "^[ \t]*end")
          (fish-swallow-block)
        (if (looking-at "^[ \t]*\\(begin\\|for\\|function\\|if\\|switch\\|while\\)")
            (setq not-done nil))))))

(defun fish-get-else-end-indent ()
  (let ((not-indented t) cur-indent)
    (while not-indented
      (forward-line -1)
      (cond
       ((looking-at "^[ \t]*if")
        (setq cur-indent (current-indentation))
        (setq not-indented nil))
       ((looking-at "^[ \t]*\\(begin\\|else\\|for\\|function\\|if\\|switch\\|while\\)")
        (unless (looking-at ".*end$")
          (setq cur-indent (current-indentation))
          (setq not-indented nil)))
       ((looking-at "^[ \t]*case")
        (setq cur-indent (- (current-indentation) tab-width))
        (setq not-indented nil))
       ((looking-at "^[ \t]*end") ; swallow the block
        (fish-swallow-block))
       ((bobp)
        (setq cur-indent 0)
        (setq not-indented nil))))
    (if (< cur-indent 0)
        (setq cur-indent 0)
      cur-indent)))

(defun fish-get-case-indent ()
  (let ((not-indented t) cur-indent)
    (while not-indented
      (forward-line -1)
      (cond
       ((looking-at "^[ \t]*case")
        (setq cur-indent (current-indentation))
        (setq not-indented nil))
       ((looking-at "^[ \t]*switch")
        (message "switch")
        (setq cur-indent (+ (current-indentation) tab-width))
        (setq not-indented nil))
       ((bobp)
        (setq cur-indent 0)
        (setq not-indented nil))))
    (if (< cur-indent 0)
        (setq cur-indent 0)
      cur-indent)))

(defun fish-get-normal-indent ()
  (let ((not-indented t) cur-indent)
    (while not-indented
      (forward-line -1)
      (cond
       ((and (looking-at "[ \t]*\\(begin\\|case\\|else\\|for\\|function\\|if\\|switch\\|while\\)\\>")
             (not (looking-at ".*end$")))
        (setq cur-indent (+ (current-indentation) tab-width))
        (setq not-indented nil))
       ((bobp)
        (setq cur-indent 0)
        (setq not-indented nil))
       ((looking-at "[ \t]*$"))
       (t
        (setq cur-indent (current-indentation))
        (setq not-indented nil))))
    (if (< cur-indent 0)
        (setq cur-indent 0)
      cur-indent)))

(defun fish-indent-line ()
  "Indent current line."
  (interactive)

  (let ((rpos (- (point-max) (point))))
    (if (bobp)
        (indent-line-to 0)
      (let (cur-indent)
        (save-excursion
          (cond
           ((looking-at "^[ \t]*\\(end\\|else\\)")
            (setq cur-indent (fish-get-else-end-indent)))
           ((looking-at "^[ \t]*case")
            (setq cur-indent (fish-get-case-indent))
            )
           (t
            (setq cur-indent (fish-get-normal-indent)))))
        (if cur-indent
            (indent-line-to cur-indent)
          (indent-line-to 0))))
    (if (> (- (point-max) rpos) (point))
        (goto-char (- (point-max) rpos)))
    ))

;;;###autoload
(define-derived-mode fish-mode prog-mode "Fish"
  "Major mode for editing fish shell files."
  (setq-local indent-line-function 'fish-indent-line)
  (setq-local font-lock-defaults '(fish-font-lock-keywords-1))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*"))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("fish" . fish-mode))

(provide 'fish-mode)

;;; fish-mode.el ends here
