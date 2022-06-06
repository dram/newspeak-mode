;;; newspeak-mode.el --- Major mode for the Smalltalk programming language

;; Author: Steve Byrne
;; Version: 3.2.92
;; Copyright 1988-92, 1994-95, 1999, 2000, 2003, 2007, 2008, 2009
;; Free Software Foundation, Inc.

;; This file is part of GNU Smalltalk.

;; GNU Smalltalk is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; GNU Smalltalk is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Smalltalk; see the file COPYING.  If not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Incorporates Frank Caggiano's changes for Emacs 19.
;; Updates and changes for Emacs 20 and 21 by David Forster

;;; Code:

;; ===[ Variables and constants ]=====================================

(defvar newspeak-name-regexp "[A-z][A-z0-9_]*"
  "A regular expression that matches a Smalltalk identifier.")

(defvar newspeak-keyword-regexp (concat newspeak-name-regexp ":")
  "A regular expression that matches a Smalltalk keyword.")

(defvar newspeak-name-chars "A-z0-9"
  "The collection of character that can compose a Smalltalk identifier.")

(defvar newspeak-whitespace " \t\n\f")

(defconst newspeak-indent-amount 4
  "*'Tab size'; used for simple indentation alignment.")

;; ---[ Syntax Table ]------------------------------------------------

;; This may very well be a bug, but certin chars like ?+ are set to be
;; punctuation, when in fact one might think of them as words (that
;; is, they are valid selector names).  Some functions will fail
;; however, (like newspeak-begin-of-defun) so there punctuation.
;; Works for now...

(defvar newspeak-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Make sure A-z0-9 are set to "w   " for completeness
    (let ((c 0))
      (setq c ?0)
      (while (<= c ?9)
	(setq c (1+ c))
	(modify-syntax-entry c "w   " table))
      (setq c ?A)
      (while (<= c ?Z)
	(setq c (1+ c))
	(modify-syntax-entry c "w   " table))
      (setq c ?a)
      (while (<= c ?z)
	(setq c (1+ c))
	(modify-syntax-entry c "w   " table)))
    (modify-syntax-entry 10  " >  " table) ; Comment (generic)
    (modify-syntax-entry ?:  ".   " table) ; Symbol-char
    (modify-syntax-entry ?_  "_   " table) ; Symbol-char
    (modify-syntax-entry ?\" "/   " table) ; Character literal
    (modify-syntax-entry ?'  "\"  " table) ; String
    (modify-syntax-entry ?#  "'   " table) ; Symbol or Array constant
    (modify-syntax-entry ?\( "()1 " table) ; Grouping
    (modify-syntax-entry ?\) ")(4 " table) ; Grouping
    (modify-syntax-entry ?\[ "(]  " table) ; Block-open
    (modify-syntax-entry ?\] ")[  " table) ; Block-close
    (modify-syntax-entry ?{  "(}  " table) ; Array-open
    (modify-syntax-entry ?}  "){  " table) ; Array-close
    (modify-syntax-entry ?!  ".   " table) ; End message / Delimit defs
    (modify-syntax-entry ?\; ".   " table) ; Cascade
    (modify-syntax-entry ?|  ".   " table) ; Temporaries
    (modify-syntax-entry ?^  ".   " table) ; Return
    ;; Just to make sure these are not set to "w   "
    (modify-syntax-entry ?<  ".   " table)
    (modify-syntax-entry ?>  ".   " table)
    (modify-syntax-entry ?+  ".   " table) ; math
    (modify-syntax-entry ?-  ".   " table) ; math
    (modify-syntax-entry ?*  ". 23" table) ; math
    (modify-syntax-entry ?/  ".2  " table) ; math
    (modify-syntax-entry ?=  ".   " table) ; bool/assign
    (modify-syntax-entry ?%  ".   " table) ; valid selector
    (modify-syntax-entry ?&  ".   " table) ; boolean
    (modify-syntax-entry ?\\ ".   " table) ; ???
    (modify-syntax-entry ?~  ".   " table) ; misc. selector
    (modify-syntax-entry ?@  ".   " table) ; Point
    (modify-syntax-entry ?,  ".   " table) ; concat
    table)
  "Syntax table used by Smalltalk mode.")

;; ---[ Abbrev table ]------------------------------------------------

(defvar newspeak-mode-abbrev-table nil
  "Abbrev table in use in `newspeak-mode' buffers.")
(define-abbrev-table 'newspeak-mode-abbrev-table ())

;; ---[ Keymap ]------------------------------------------------------

(defvar newspeak-template-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "p" 'newspeak-private-template)
    (define-key keymap "c" 'newspeak-class-template)
    (define-key keymap "i" 'newspeak-instance-template)
    keymap)
  "Keymap of template creation keys.")

(defvar newspeak-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\n" 	   'newspeak-newline-and-indent)
    (define-key keymap "\C-c\C-a"   'newspeak-begin-of-defun)
    (define-key keymap "\C-c\C-e"   'newspeak-end-of-defun)
    (define-key keymap "\C-c\C-f"   'newspeak-forward-sexp)
    (define-key keymap "\C-c\C-b"   'newspeak-backward-sexp)
    (define-key keymap "\C-c\C-p"   'newspeak-goto-previous-keyword)
    (define-key keymap "\C-c\C-n"   'newspeak-goto-next-keyword)
    ;; the following three are deprecated
    (define-key keymap "\C-\M-a"   'newspeak-begin-of-defun)
    (define-key keymap "\C-\M-f"   'newspeak-forward-sexp)
    (define-key keymap "\C-\M-b"   'newspeak-backward-sexp)
    (define-key keymap "!" 	   'newspeak-bang)
    (define-key keymap ":"	   'newspeak-colon)
    (define-key keymap "\C-ct"      newspeak-template-map)

    ;; -----

    (define-key keymap "\C-cd"     'newspeak-doit)
    (define-key keymap "\C-cf"     'newspeak-filein-buffer)
    (define-key keymap "\C-cm"     'gst)
    (define-key keymap "\C-cp"     'newspeak-print)
    (define-key keymap "\C-cq"     'newspeak-quit)
    (define-key keymap "\C-cs"     'newspeak-snapshot)
    
    keymap)
  "Keymap for Smalltalk mode.")

(defconst newspeak-binsel "\\([-+*/~,<>=&?]\\{1,2\\}\\|:=\\|||\\)"
  "Smalltalk binary selectors.")

(defconst newspeak-font-lock-keywords
  (list
   '("#[[:alpha:]][[:alnum:]_]*" . font-lock-constant-face)
   '("\\<[A-z][A-z0-9_]*:" . font-lock-function-name-face)
   '("\".\"" . font-lock-string-face) ;; Chars
   (cons newspeak-binsel 'font-lock-function-name-face)
   '("\\^" . font-lock-keyword-face)
   '("\\<[A-Z]\\sw*\\>" . font-lock-type-face))
  "Basic Smalltalk keywords font-locking.")

(defconst newspeak-font-lock-keywords-1
  newspeak-font-lock-keywords
  "Level 1 Smalltalk font-locking keywords.")

(defconst newspeak-font-lock-keywords-2
  (append newspeak-font-lock-keywords-1
	  (list
	   '("\\<\\(class\\|private\\|protected\\|public\\)\\>"
	     . font-lock-keyword-face)
	   '("\\<\\(true\\|false\\|nil\\|self\\|super\\)\\>"
	     . font-lock-builtin-face)
	   '(":[a-z][A-z0-9_]*" . font-lock-variable-name-face)
	   '(" |" . font-lock-type-face)
	   '("<.*>" . font-lock-builtin-face)))
  
  "Level 2 Smalltalk font-locking keywords.")

(defvar newspeak-last-category ""
  "Category of last method.")

;; ---[ Interactive functions ]---------------------------------------

;;;###autoload
(defun newspeak-mode ()
  "Major mode for editing Newspeak code.

Commands:
\\{newspeak-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'newspeak-mode)
  (setq mode-name "Newspeak")

  (use-local-map newspeak-mode-map)
  (set-syntax-table newspeak-mode-syntax-table)
  (setq local-abbrev-table newspeak-mode-abbrev-table)
  
  ;; Buffer locals

  (set (make-local-variable 'paragraph-start)
       (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate)
       paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'indent-line-function)
       'newspeak-indent-line)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start) "\"")
  (set (make-local-variable 'comment-end) "\"")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "\" *")
  ;; Doesn't seem useful...?
  (set (make-local-variable 'comment-indent-function)
       'newspeak-comment-indent)
  ;; For interactive f-b sexp
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  ;; font-locking
  (set (make-local-variable 'font-lock-defaults)
       '((newspeak-font-lock-keywords
	  newspeak-font-lock-keywords-1
	  newspeak-font-lock-keywords-2)
	 nil nil nil nil))

  ;; tags
  (set (make-local-variable 'find-tag-default-function)
       'newspeak-find-message)
  ;; Run hooks, must be last
  (run-hooks 'newspeak-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.st\\'" . newspeak-mode))

(defun newspeak-tab ()
  (interactive)
  (let (col)
    ;; round up, with overflow
    (setq col (* (/ (+ (current-column) newspeak-indent-amount)
		    newspeak-indent-amount)
		 newspeak-indent-amount))
    (indent-to-column col)))

(defun newspeak-bang-begin-of-defun ()
  (let ((parse-sexp-ignore-comments t) here delim start)
    (setq here (point))
    (while (and (search-backward "!" nil 'to-end)
		(setq delim (newspeak-in-string)))
      (search-backward delim))
    (setq start (point))
    (if (looking-at "!")
	(forward-char 1))
    (newspeak-forward-whitespace)
    ;; check to see if we were already at the start of a method
    ;; in which case, the semantics are to go to the one preceeding
    ;; this one
    (if (and (= here (point))
	     (/= start (point-min)))
	(progn
	  (goto-char start)
	  (newspeak-backward-whitespace) ;may be at ! "foo" !
	  (if (= (preceding-char) ?!)
	      (backward-char 1))
	  (newspeak-begin-of-defun)))))  ;and go to the next one

(defun newspeak-scope-begin-of-defun ()
  (let (here prev (start (newspeak-current-scope-point)))
    (if (and start (/= (point) start))
	(progn
    (backward-char 1)
    (skip-chars-backward " \t")
    (if (bolp)
	(backward-char 1)
      (end-of-line))
    (setq here (point))
	       (goto-char start)
	       (skip-chars-forward "^[")
	       (forward-char 1)
	       (condition-case nil
		   (while (< (point) here)
		     (if (looking-at "[ \t]*\\[") (setq prev (point)))
		     (forward-sexp 1))
		 (error t))
	       (if prev
		   (progn
		     (goto-char prev)
		     (condition-case nil
			 (progn
			   (forward-sexp 1)
			   (if (and (< (point) here)
				    (= (char-before) ?\]))
			       (progn
				 (skip-syntax-forward " \t")
				 (setq prev (point)))))
		       (error t))
		     (goto-char prev)
		     (beginning-of-line)
		     (skip-chars-forward " \t"))
		 (goto-char start))))))

(defun newspeak-begin-of-defun ()
  "Skips to the beginning of the current method.
If already at the beginning of a method, skips to the beginning
of the previous one."
  (interactive)
  (if (newspeak-in-bang-syntax)
      (newspeak-bang-begin-of-defun)
    (newspeak-scope-begin-of-defun)))

(defun newspeak-begin-of-scope ()
  "Skips to the beginning of the current method.
If already at the beginning of a method, skips to the beginning
of the previous one."
  (interactive)
  (let ((start (newspeak-current-scope-point)))
    (if start (goto-char start))))


(defun newspeak-forward-sexp (n)
  "Move point left to the next smalltalk expression."
  (interactive "p")
  (let (i)
    (cond ((< n 0)
	   (newspeak-backward-sexp (- n)))
	  ((null parse-sexp-ignore-comments)
	   (forward-sexp n))
	  (t
	   (while (> n 0)
	     (newspeak-forward-whitespace)
	     (forward-sexp 1)
	     (setq n (1- n)))))))

(defun newspeak-backward-sexp (n)
  "Move point right to the next smalltalk expression."
  (interactive "p")
  (let (i)
    (cond ((< n 0)
	   (newspeak-forward-sexp (- n)))
	  ((null parse-sexp-ignore-comments)
	   (backward-sexp n))
	  (t
	   (while (> n 0)
	     (newspeak-backward-whitespace)
	     (backward-sexp 1)
	     (setq n (1- n)))))))

(defun newspeak-reindent ()
  (interactive)
  (newspeak-indent-line))

(defun newspeak-newline-and-indent ()
  "Called basically to do newline and indent.
Sees if the current line is a new statement, in which case the
indentation is the same as the previous statement (if there is
one), or is determined by context; or, if the current line is not
the start of a new statement, in which case the start of the
previous line is used, except if that is the start of a new line
in which case it indents by `newspeak-indent-amount'."
  (interactive)
  (newline)
  (newspeak-indent-line))

(defun newspeak-colon ()
  "Possibly reindents a line when a colon is typed.
If the colon appears on a keyword that's at the start of the line (ignoring
whitespace, of course), then the previous line is examined to see if there
is a colon on that line, in which case this colon should be aligned with the
left most character of that keyword.  This function is not fooled by nested
expressions."
  (interactive)
  (let (needs-indent state (parse-sexp-ignore-comments t))
    (setq state (parse-partial-sexp (point-min) (point)))

    (if (null (nth 3 state))		;we're not in string or comment
	(progn
	  (save-excursion
      	    (skip-chars-backward "A-z0-9_")
	    (if (and (looking-at newspeak-name-regexp)
		     (not (newspeak-at-begin-of-defun)))
		(setq needs-indent (newspeak-white-to-bolp))))
	  (and needs-indent
	       (newspeak-indent-for-colon))))
    ;; out temporarily
    ;;    (expand-abbrev)			;I don't think this is the "correct"
    ;;					;way to do this...I suspect that
    ;;					;some flavor of "call interactively"
    ;;					;is better.
    (self-insert-command 1)))

(defun newspeak-bang ()
  "Go to the end of the method definition."
  (interactive)
  (cond ((or (newspeak-in-string) (newspeak-in-comment)) (insert "!"))
        ((newspeak-in-bang-syntax)
         (progn (insert "!")
                (save-excursion
                  (beginning-of-line)
                  (if (looking-at "^[ \t]+!")
                      (delete-horizontal-space)))))
        (t (newspeak-end-of-defun))))

(defun newspeak-end-of-defun ()
  (interactive)
  (if (newspeak-in-bang-syntax)
      (progn (search-forward "!")
	     (forward-char 1)
	     (if (looking-at "[ \t\n]+!")
		 (progn (search-forward 1)
			(forward-char 1))))
    (progn (end-of-line)
	   (newspeak-begin-of-defun)
	   (skip-chars-forward "^[")
	   (forward-sexp 1)
	   (skip-chars-forward " \t\n"))))

(defun newspeak-last-category-name ()
  newspeak-last-category)

(defun newspeak-insert-indented-line (string)
  (insert (format "%s\n" string))
  (save-excursion
    (backward-char 1)
    (newspeak-indent-line)))
 
(defun newspeak-maybe-insert-spacing-line (n)
  (if (not (save-excursion
	     (forward-line (- n))
	     (looking-at "^[ \t]*$")))
      (insert "\n")))

(defun newspeak-insert-method-body (selector-name category-name)
  (let (insert-at-top)
    (beginning-of-line)
    (newspeak-forward-whitespace)
    (beginning-of-line)
    (setq insert-at-top (newspeak-at-begin-of-defun))
    (if (not insert-at-top)
	(progn (newspeak-end-of-defun)
	       (beginning-of-line)))
    (newspeak-maybe-insert-spacing-line 1)
    (newspeak-insert-indented-line (format "%s [" selector-name))
    (save-excursion
      (insert "\n")
      (if (not (equal category-name ""))
	  (newspeak-insert-indented-line (format "<category: '%s'>" category-name)))
      (newspeak-insert-indented-line "]")
      (newspeak-maybe-insert-spacing-line 0))
    (newspeak-indent-line)
    (end-of-line)))

(defun newspeak-instance-template-fn (class-name selector-name category-name)
  (setq newspeak-last-category category-name)
  (newspeak-exit-class-scope)
  (newspeak-insert-method-body
   (if (equal class-name (newspeak-current-class-name))
       selector-name
     (format "%s >> %s" class-name selector-name))
   category-name))

(defun newspeak-class-template-fn (class-name selector-name category-name)
  (setq newspeak-last-category category-name)
  (if (and (equal selector-name "")
	   (equal class-name (newspeak-current-class-name)))
      (progn (newspeak-insert-method-body (format "    %s class" class-name) "")
	     (setq newspeak-last-category "instance creation"))
    (newspeak-insert-method-body
     (if (and (newspeak-in-class-scope)
	      (equal class-name (newspeak-current-class-name)))
	 selector-name
       (format "%s class >> %s" class-name selector-name))
     category-name)))

(defun newspeak-private-template-fn (class-name selector-name)
  (if (newspeak-in-class-scope)
      (newspeak-class-template-fn class-name selector-name "private")
    (newspeak-instance-template-fn class-name selector-name "private")))

(defun newspeak-maybe-read-class (with-class)
   (if (= with-class 1)
       (newspeak-current-class-name)
     (read-string "Class: " (newspeak-current-class-name))))

(defun newspeak-instance-template (with-class)
  (interactive "p")
  (newspeak-instance-template-fn
   (newspeak-maybe-read-class with-class)
   (read-string "Selector: ")
   (read-string "Category: " (newspeak-last-category-name))))

(defun newspeak-class-template (with-class)
  (interactive "p")
  (let* ((class-name (newspeak-maybe-read-class with-class))
	 (selector-name (read-string "Selector: "))
	 (category-name (if (equal selector-name "") ""
			  (read-string "Category: "
				       (newspeak-last-category-name)))))
  (newspeak-class-template-fn class-name selector-name category-name)))
   

(defun newspeak-private-template (with-class)
  (interactive "p")
  (newspeak-private-template-fn
   (newspeak-maybe-read-class with-class)
   (read-string "Selector: ")))

;; ---[ Non-interactive functions ]-----------------------------------

;; This is used by indent-for-comment
;; to decide how much to indent a comment in Smalltalk code
;; based on its context.
(defun newspeak-comment-indent ()
  (if (looking-at "^\"")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.

(defun newspeak-indent-line ()
  (newspeak-indent-to-column
   (save-excursion
     (beginning-of-line)
     (skip-chars-forward " \t")
     (if (and (not (newspeak-in-comment))
	      (looking-at "[A-z][A-z0-9_]*:")
	      (not (newspeak-at-begin-of-defun)))
	 (newspeak-indent-for-colon)
       (newspeak-calculate-indent)))))
 
(defun newspeak-toplevel-indent (for-scope)
  (let (orig)
    (condition-case nil
	(save-excursion
	  (save-restriction
	    (widen)
	    (end-of-line)
	    (setq orig (line-number-at-pos))
	    (if for-scope (newspeak-begin-of-scope) (newspeak-begin-of-defun))
	    (newspeak-forward-whitespace)
	    (if (= orig (line-number-at-pos))
		(newspeak-current-column)
	      (+ newspeak-indent-amount (newspeak-current-column)))))
      (error 0))))
     
(defun newspeak-statement-indent ()
  (let (needs-indent indent-amount done c state orig start-of-line close
		     (parse-sexp-ignore-comments nil))
    (save-excursion
      (save-restriction
	(widen)
	(beginning-of-line)
	(setq close (looking-at "[ \t]*\]"))
	(narrow-to-region (point-min) (point)) ;only care about what's before
	(setq state (parse-partial-sexp (point-min) (point)))
	(cond ((nth 4 state) ;in a comment
	       (save-excursion
		 (newspeak-backward-comment)
		 (setq indent-amount
		       (+ (current-column) (if (= (current-column) 0) 0 1)))))
	      ((equal (nth 3 state) ?')	;in a string
	       (setq indent-amount 0))
	      (close ;just before a closing bracket
	       (save-excursion
		 (condition-case nil
		     (progn (widen)
			    (newspeak-forward-whitespace)
			    (forward-char)
			    (backward-sexp 1)
			    (beginning-of-line)
			    (newspeak-forward-whitespace)
			    (setq indent-amount (current-column))))))
	      (t
	       (save-excursion
		 (newspeak-backward-whitespace)
		 (if (or (bobp)
			 (= (preceding-char) ?!))
		     (setq indent-amount 0)))))
	(if (null indent-amount)
	    (progn
	      (newspeak-narrow-to-method)
	      (beginning-of-line)
	      (setq state (newspeak-parse-sexp-and-narrow-to-paren))
	      (newspeak-backward-whitespace)
	      (cond ((bobp)		;must be first statment in block or exp
		     (if (nth 1 state)	;we're in a paren exp
			 (if (looking-at "$")
			     ;; block with no statements, indent by 4
			     (setq indent-amount (+ (newspeak-current-indent)
						    newspeak-indent-amount))

			     ;; block with statements, indent to first non-whitespace
			     (setq indent-amount (newspeak-current-column)))

		       ;; we're top level
		       (setq indent-amount (newspeak-toplevel-indent nil))))
		    ((newspeak-at-end-of-statement) ;end of statement or after temps
		     (newspeak-find-statement-begin)
		     (setq indent-amount (newspeak-current-column)))
		    ((= (preceding-char) ?:)
		     (beginning-of-line)
		     (newspeak-forward-whitespace)
		     (setq indent-amount (+ (newspeak-current-column)
					    newspeak-indent-amount)))
		    ((= (preceding-char) ?>) ;maybe <primitive: xxx>
		     (save-excursion
		       (beginning-of-line)
		       (if (looking-at "[ \t]*<[ \t]*[a-zA-Z]+:")
			   (setq indent-amount (newspeak-toplevel-indent nil))))))))
	(or indent-amount
	    (save-excursion
	      (condition-case nil
		  (newspeak-find-statement-begin)
		  (error (beginning-of-line)))
	      (+ (newspeak-current-column)
		 newspeak-indent-amount)))))))

(defun newspeak-at-end-of-statement ()
  (save-excursion
    (or (= (preceding-char) ?.)
	(and (= (preceding-char) ?|)
	     (progn
	       (backward-char 1)
	       (while (and (not (bobp)) (looking-back "[ \t\na-zA-Z]" nil))
		 (skip-chars-backward " \t\n")
		 (skip-chars-backward "a-zA-Z"))
	       (if (= (preceding-char) ?|)
		   (progn
		     (backward-char 1)
		     (skip-chars-backward " \t\n")))
	       (bobp))))))

(defun newspeak-calculate-indent ()
    (cond
     ((newspeak-at-begin-of-scope) (newspeak-toplevel-indent t))
     ((newspeak-at-begin-of-defun) (newspeak-toplevel-indent t))
     (t (newspeak-statement-indent))))


(defun newspeak-in-string ()
  "Returns non-nil delimiter as a string if the current location is
actually inside a string or string like context."
  (let (state)
    (setq state (parse-partial-sexp (point-min) (point)))
    (and (nth 3 state)
	 (char-to-string (nth 3 state)))))

(defun newspeak-in-comment ()
  "Return non-nil if the current location is inside a comment."
  (let (state)
    (setq state (parse-partial-sexp (point-min) (point)))
    (nth 4 state)))

(defun newspeak-forward-whitespace ()
  "Skip white space and comments forward, stopping at end of buffer
or non-white space, non-comment character"
  (while (looking-at (concat "[" newspeak-whitespace "]"))
    (skip-chars-forward newspeak-whitespace)
    (if (= (following-char) ?\")
	(forward-comment 1))))

;; (defun newspeak-forward-whitespace ()
;;   "Skip white space and comments forward, stopping at end of buffer
;; or non-white space, non-comment character"
;;   (forward-comment 1)
;;   (if (= (following-char) ?\n)
;;       (forward-char)))

(defun newspeak-backward-whitespace ()
  "Like forward whitespace only going towards the start of the buffer."
  (while (progn (skip-chars-backward newspeak-whitespace)
		(= (preceding-char) ?\"))
    (search-backward "\"" nil t 2)))
	
(defun newspeak-current-column ()
  "Return the current column of the given line, regardless of narrowed buffer."
  (save-restriction
    (widen)
    (current-column)))			;this changed in 18.56

(defun newspeak-current-indent ()
  "Return the indentation of the given line, regardless of narrowed buffer."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (current-column))))

(defun newspeak-find-statement-begin ()
  "Leaves the point at the first non-blank, non-comment character of a new
statement.  If begininning of buffer is reached, then the point is left there.
This routine only will return with the point pointing at the first non-blank
on a line; it won't be fooled by multiple statements on a line into stopping
prematurely.  Also, goes to start of method if we started in the method
selector."
  (let (start ch)
    (if (= (preceding-char) ?.)		;if we start at eos
	(backward-char 1))		;we find the begin of THAT stmt
    (while (and (null start) (not (bobp)))
      (newspeak-backward-whitespace)
      (cond ((= (setq ch (preceding-char)) ?.)
	     (let (saved-point)
	       (setq saved-point (point))
	       (newspeak-forward-whitespace)
	       (if (newspeak-white-to-bolp)
		   (setq start (point))
		 (goto-char saved-point)
		 (newspeak-backward-sexp 1))
	       ))
	    ((= ch ?^)			;HACK -- presuming that when we back
					;up into a return that we're at the
					;start of a statement
	     (backward-char 1)
	     (setq start (point)))
	    ((= ch ?!)
	     (newspeak-forward-whitespace)
	     (setq start (point)))
	    (t
	     (newspeak-backward-sexp 1))))
    (if (null start)
      (progn
	(goto-char (point-min))
	(newspeak-forward-whitespace)
	(setq start (point))))
    start))

(defun newspeak-match-paren (state)
  "Answer the closest previous open paren.
Actually, skips over any block parameters, and skips over the whitespace
following on the same line."
  (let ((paren-addr (nth 1 state))
	start c done)
    (if (not paren-addr)
	()
      (save-excursion
	(goto-char paren-addr)
	(setq c (following-char))
	(cond ((or (eq c ?\() (eq c ?{))
	       (1+ (point)))
	      ((eq c ?\[)
	       (forward-char 1)

	       ;; Now skip over the block parameters, if any
	       (setq done nil)
	       (while (not done)
		 (skip-chars-forward " \t")
		 (setq c (following-char))
		 (cond ((eq c ?:)
			(newspeak-forward-sexp 1))
		       ((eq c ?|)
			(forward-char 1) ;skip vbar
			(skip-chars-forward " \t")
			(setq done t))	;and leave
		       (t
			(setq done t))))

	       ;; Now skip over the block temporaries, if any
	       (cond ((eq (following-char) ?|)
		      (setq done nil)
		      (forward-char 1))
		     (t
		      (setq done t)))
	       
	       (while (not done)
		 (skip-chars-forward " \t")
		 (setq c (following-char))
		 (cond ((eq c ?|)
			(forward-char 1) ;skip vbar
			(skip-chars-forward " \t")
			(setq done t))	;and leave
		       (t
			(newspeak-forward-sexp 1))))

	       (point)))))))

(defun newspeak-parse-sexp-and-narrow-to-paren ()
  "Narrows the region to between point and the closest previous open paren.
Actually, skips over any block parameters, and skips over the whitespace
following on the same line."
  (let*	((parse-sexp-ignore-comments t)
	 (state (parse-partial-sexp (point-min) (point)))
	 (start (newspeak-match-paren state)))
    (if (null start) () (narrow-to-region start (point)))
    state))

(defun newspeak-at-begin-of-scope ()
  "Return T if at the beginning of a class or namespace definition, otherwise nil."
  (save-excursion
    (end-of-line)
    (if (newspeak-in-bang-syntax)
	(let ((parse-sexp-ignore-comments t))
	  (and (bolp)
	       (progn (newspeak-backward-whitespace)
		      (= (preceding-char) ?!))))
      (let ((curr-line-pos (line-number-at-pos)))
	(if (newspeak-begin-of-scope)
	    (= curr-line-pos (line-number-at-pos)))))))

(defun newspeak-at-begin-of-defun ()
  "Return T if at the beginning of a method definition, otherwise nil."
  (save-excursion
    (end-of-line)
    (if (newspeak-in-bang-syntax)
	(let ((parse-sexp-ignore-comments t))
	  (and (bolp)
	       (progn (newspeak-backward-whitespace)
		      (= (preceding-char) ?!))))
      (let ((curr-line-pos (line-number-at-pos)))
	(if (newspeak-begin-of-defun)
	    (= curr-line-pos (line-number-at-pos)))))))

(defun newspeak-indent-for-colon ()
  (let (indent-amount c start-line state done default-amount
		     (parse-sexp-ignore-comments t))
    ;; we're called only for lines which look like "<whitespace>foo:"
    (save-excursion
      (save-restriction
	(widen)
	(beginning-of-line)
	(newspeak-end-of-paren)
	(newspeak-narrow-to-method)
	(setq state (newspeak-parse-sexp-and-narrow-to-paren))
	(narrow-to-region (point-min) (point))
	(setq start-line (point))
	(newspeak-backward-whitespace)
	(cond
	 ((bobp)
	  (setq indent-amount (newspeak-toplevel-indent t)))
	 ((eq (setq c (preceding-char)) ?\;)	; cascade before, treat as stmt continuation
	  (newspeak-find-statement-begin)
	  (setq indent-amount (+ (newspeak-current-column)
				 newspeak-indent-amount)))
	 ((eq c ?.)	; stmt end, indent like it (syntax error here?)
	  (newspeak-find-statement-begin)
	  (setq indent-amount (newspeak-current-column)))
	 (t				;could be a winner
	    (newspeak-find-statement-begin)
	    ;; we know that since we weren't at bobp above after backing
	    ;; up over white space, and we didn't run into a ., we aren't
	    ;; at the beginning of a statement, so the default indentation
	    ;; is one level from statement begin
	    (setq default-amount
		  (+ (newspeak-current-column) ;just in case
		     newspeak-indent-amount))
	    ;; might be at the beginning of a method (the selector), decide
	    ;; this here
	    (if (not (looking-at newspeak-keyword-regexp ))
		;; not a method selector
		(while (and (not done) (not (eobp)))
		  (newspeak-forward-sexp 1) ;skip over receiver
		  (newspeak-forward-whitespace)
		  (cond ((eq (following-char) ?\;)
			 (setq done t)
			 (setq indent-amount default-amount))
			((and (null indent-amount) ;pick up only first one
			      (looking-at newspeak-keyword-regexp))
			 (setq indent-amount (newspeak-current-column))))))
	    (and (null indent-amount)
		 (setq indent-amount default-amount))))))
    (or indent-amount (newspeak-current-indent))))

(defun newspeak-end-of-paren ()
  (let ((prev-point (point)))
	(newspeak-safe-forward-sexp)
	(while (not (= (point) prev-point))
	  (setq prev-point (point))
	  (newspeak-safe-forward-sexp))))

(defun newspeak-indent-to-column (col)
  (if (/= col (newspeak-current-indent))
      (save-excursion
	(beginning-of-line)
	(delete-horizontal-space)
	(indent-to col)))
  (if (bolp)
      ;;delete horiz space may have moved us to bol instead of staying where
      ;; we were.  this fixes it up.
      (move-to-column col)))

(defun newspeak-narrow-to-method ()
  "Narrows the buffer to the contents of the method, exclusive of the
method selector and temporaries."
  (let ((end (point))
	(parse-sexp-ignore-comments t)
	done handled)
    (save-excursion
      (newspeak-begin-of-defun)
      (if (looking-at "[a-zA-z]")	;either unary or keyword msg
	  ;; or maybe an immediate expression...
	  (progn
	    (forward-sexp)
	    (if (= (following-char) ?:) ;keyword selector
		(progn			;parse full keyword selector
		  (backward-sexp 1)	;setup for common code
		  (newspeak-forward-keyword-selector))
	      ;; else maybe just a unary selector or maybe not
	      ;; see if there's stuff following this guy on the same line
	      (let (here eol-point)
		(setq here (point))
		(end-of-line)
		(setq eol-point (point))
		(goto-char here)
		(newspeak-forward-whitespace)
		(if (< (point) eol-point) ;if there is, we're not a method
					; (a heuristic guess)
		    (beginning-of-line)
		  (goto-char here)))))	;else we're a unary method (guess)
	;; this must be a binary selector, or a temporary
	(if (= (following-char) ?|)
	    (progn			;could be temporary
	      (end-of-line)
	      (newspeak-backward-whitespace)
	      (if (= (preceding-char) ?|)
		  (progn
		    (setq handled t)))
	      (beginning-of-line)))
	(if (not handled)
	    (progn
	      (skip-chars-forward (concat "^" newspeak-whitespace))
	      (newspeak-forward-whitespace)
	      (skip-chars-forward newspeak-name-chars)))) ;skip over operand
      (if (not (newspeak-in-bang-syntax))
	  (progn (skip-chars-forward "^[")
		 (forward-char)))
      (newspeak-forward-whitespace)

      ;;sbb  6-Sep-93 14:58:54 attempted fix(skip-chars-forward newspeak-whitespace)
      (if (= (following-char) ?|)	;scan for temporaries
	  (progn
	    (forward-char)		;skip over |
	    (newspeak-forward-whitespace)
	    (while (and (not (eobp))
			(looking-at "[a-zA-Z_]"))
	      (skip-chars-forward newspeak-name-chars)
	      (newspeak-forward-whitespace)
	      )
	    (if (and (= (following-char) ?|) ;only if a matching | as a temp
		     (< (point) end))	;and we're after the temps
		(narrow-to-region (1+ (point)) end))) ;do we limit the buffer
	;; added "and <..." Dec 29 1991 as a test
	(and (< (point) end)
	     (narrow-to-region (point) end))))))

(defun newspeak-forward-keyword-selector ()
  "Starting on a keyword, this function skips forward over a keyword selector.
It is typically used to skip over the actual selector for a method."
  (let (done)
    (while (not done)
      (if (not (looking-at "[a-zA-Z_]"))
	  (setq done t)
	(skip-chars-forward newspeak-name-chars)
	(if (= (following-char) ?:)
	    (progn
	      (forward-char)
	      (newspeak-forward-sexp 1)
	      (newspeak-forward-whitespace))
	  (setq done t)
	  (backward-sexp 1))))))

(defun newspeak-white-to-bolp ()
  "Return T if from the current position to beginning of line is whitespace.
Whitespace is defined as spaces, tabs, and comments."
  (let (done is-white line-start-pos)
    (save-excursion
      (save-excursion
	(beginning-of-line)
	(setq line-start-pos (point)))
      (while (not done)
	(and (not (bolp))
	     (skip-chars-backward " \t"))
	(cond ((bolp)
	       (setq done t)
	       (setq is-white t))
	      ((= (char-after (1- (point))) ?\")
	       (backward-sexp)
	       (if (< (point) line-start-pos) ;comment is multi line
		   (setq done t)))
	      (t
	       (setq done t))))
      is-white)))


(defun newspeak-backward-comment ()
  (search-backward "\"")		;find its start
  (while (= (preceding-char) ?\")	;skip over doubled ones
    (backward-char 1)
    (search-backward "\"")))

(defun newspeak-current-class ()
  (let ((here (point))
	curr-hit-point curr-hit new-hit-point new-hit)
    (save-excursion
      (if (setq curr-hit-point
		(search-backward-regexp "^![ \t]*\\(\\(\\w+\\.\\)*\\w+\\)[ \t]+" nil t))
	  (setq curr-hit (buffer-substring
			  (match-beginning 1)
			  (match-end 1)))))

    (save-excursion
      (if (setq new-hit-point
		(search-backward-regexp
		 "^[ \t]*\\(\\w+\\)[ \t]+class[ \t]+\\[" nil t))
	  (setq new-hit (buffer-substring
			 (match-beginning 1)
			 (match-end 1)))))
    (if (and new-hit-point
	     (or (not curr-hit-point) (> new-hit-point curr-hit-point))
	     (newspeak-in-class-scope-of here new-hit-point))
	  (progn (setq curr-hit-point new-hit-point)
		 (setq curr-hit new-hit)))

    (save-excursion
      (if (setq new-hit-point
		(search-backward-regexp
		 "^[ \t]*\\(\\(\\w+\\.\\)*\\w+\\)[ \t]+extend[ \t]+\\[" nil t))
	  (setq new-hit (buffer-substring
			 (match-beginning 1)
			 (match-end 1)))))
    (if (and new-hit-point
	     (or (not curr-hit-point) (> new-hit-point curr-hit-point)))
	  (progn (setq curr-hit-point new-hit-point)
		 (setq curr-hit new-hit)))

    (save-excursion
      (if (setq new-hit-point
		(search-backward-regexp
		 "^[ \t]*\\(\\w+\\.\\)*\\w+[ \t]+\\(variable\\|variableWord\\|variableByte\\)?subclass:[ \t]+#?\\(\\w+\\)" nil t))
	  (setq new-hit (buffer-substring
			 (match-beginning 3)
			 (match-end 3)))))
    (if (and new-hit-point
	     (or (not curr-hit-point) (> new-hit-point curr-hit-point)))
	(progn (setq curr-hit-point new-hit-point)
	       (setq curr-hit new-hit)))
    (cons curr-hit curr-hit-point)))

(defun newspeak-update-hit-point (current search)
  (save-excursion
    (let ((new-hit-point (funcall search)))
      (if (and new-hit-point
               (or (not current) (> new-hit-point current)))
          new-hit-point
        current))))

(defun newspeak-current-scope-point ()
  (let ((curr-hit-point (newspeak-current-class-point)))
    (setq curr-hit-point
	  (newspeak-update-hit-point curr-hit-point
				      (lambda () (search-backward-regexp "^[ \t]*Eval[ \t]+\\[" nil t))))
    (setq curr-hit-point
	  (newspeak-update-hit-point curr-hit-point
				      (lambda () (search-backward-regexp "^[ \t]*Namespace[ \t]+current:[ \t]+[A-Za-z0-9_.]+[ \t]+\\[" nil t))))
    curr-hit-point))

(defun newspeak-current-class-point ()
    (cdr (newspeak-current-class)))

(defun newspeak-current-class-name ()
    (car (newspeak-current-class)))

(defun newspeak-in-bang-syntax ()
  (let ((curr-hit-point (newspeak-current-class-point)))
    (and curr-hit-point
	 (save-excursion
	   (goto-char curr-hit-point)
	   (beginning-of-line)
	   (looking-at "!")))))

(defun newspeak-in-class-scope-of (orig curr-hit-point)
  (save-excursion
    (goto-char curr-hit-point)
    (skip-chars-forward " \t")
    (skip-chars-forward newspeak-name-chars)
    (skip-chars-forward " \t")
    (and (= (following-char) ?c)
	 ;; check if the class scope ends after the point
	 (condition-case nil
	     (progn (skip-chars-forward "^[")
		    (forward-sexp 1)
		    (> (point) orig))
	   (error t)))))

(defun newspeak-in-class-scope ()
  (let ((curr-hit-point (newspeak-current-class-point)))
    (and curr-hit-point
	 (newspeak-in-class-scope-of (point) curr-hit-point))))

(defun newspeak-exit-class-scope ()
  (interactive)
  (if (newspeak-in-class-scope)
      (progn (newspeak-begin-of-scope)
	     (skip-chars-forward "^[")
	     (newspeak-end-of-defun))))

(defun newspeak-find-message ()
  (save-excursion
    (newspeak-goto-beginning-of-statement)
    (cond
     ((newspeak-looking-at-unary-send)
      (if (not (newspeak-has-sender))
          (progn
            (newspeak-safe-forward-sexp)
            (newspeak-safe-forward-sexp)
            (newspeak-find-message))
        (buffer-substring-no-properties (point) (progn (newspeak-safe-forward-sexp)(point)))))
     ((newspeak-looking-at-keyword-send)
      (concat (newspeak-find-beginning-of-keyword-send) (newspeak-find-end-of-keyword-send))))))
	 
(defun newspeak-safe-backward-sexp ()
  (let (prev-point)
    (condition-case nil
	(progn
	  (setq prev-point (point))
	  (newspeak-backward-sexp 1))
      (error (goto-char prev-point)))))

(defun newspeak-safe-forward-sexp ()
  (let (prev-point)
    (condition-case nil
	(progn
	  (setq prev-point (point))
	  (newspeak-forward-sexp 1))
      (error (goto-char prev-point)))))

(defun newspeak-goto-beginning-of-statement ()
  (if (not (looking-back "[ \t\n]" nil nil))
      (newspeak-safe-backward-sexp)))

(defun newspeak-has-sender ()
  (save-excursion
    (newspeak-backward-whitespace)
    (looking-back "[]})A-Za-z0-9']" nil)))

(defun newspeak-looking-at-binary-send ()
  (looking-at "[^]A-Za-z0-9:_(){}[;.\'\"]+[ \t\n]"))

(defun newspeak-looking-at-unary-send ()
  (looking-at "[A-Za-z][A-Za-z0-9]*[ \t\n]"))

(defun newspeak-looking-at-keyword-send ()
  (looking-at "[A-Za-z][A-Za-z0-9_]*:"))

(defun newspeak-looking-back-keyword-send ()
  (looking-back "[A-z][A-z0-9_]*:" nil))

(defun newspeak-find-end-of-keyword-send ()
  (save-excursion
    (newspeak-forward-whitespace)
    (if (or (looking-at "[.;]") (= (newspeak-next-keyword) (point)))
	""
      (progn
	(newspeak-goto-next-keyword)
	(concat (buffer-substring-no-properties (save-excursion (progn (newspeak-safe-backward-sexp) (point))) (point))
		(newspeak-find-end-of-keyword-send))))))

(defun newspeak-find-beginning-of-keyword-send ()
  (save-excursion
    (let ((begin-of-defun (newspeak-at-begin-of-defun)))
      (newspeak-backward-whitespace)
      (if (or (if begin-of-defun
		  (looking-back "[].;]" nil)
		(looking-back "[.;]" nil))
	      (= (newspeak-previous-keyword) (point)))
	  ""
	(progn
	  (newspeak-goto-previous-keyword)
	  (concat (newspeak-find-beginning-of-keyword-send)
		  (buffer-substring-no-properties (point) (progn (newspeak-safe-forward-sexp)(+ (point) 1)))))))))

(defun newspeak-goto-previous-keyword ()
  "Go to the previous keyword of the current message send."
  (goto-char (newspeak-previous-keyword)))

(defun newspeak-goto-next-keyword ()
  "Go to the next keyword of the current message send."
  (goto-char (newspeak-next-keyword)))

(defun newspeak-previous-keyword-1 ()
  (newspeak-backward-whitespace)
  (if (looking-back "[>[({.^]" nil) ;; not really ok when > is sent in a keyword arg
      nil
    (if (= (point) (save-excursion (newspeak-safe-backward-sexp) (point)))
	nil
      (progn
	(newspeak-safe-backward-sexp)
	(if (newspeak-looking-at-keyword-send)
	    (point)
	  (newspeak-previous-keyword-1))))))

(defun newspeak-next-keyword-1 ()
  (newspeak-forward-whitespace)
  (if (looking-at "[])};.]")
      nil
    (if (= (point) (save-excursion (newspeak-safe-forward-sexp) (point)))
	nil
      (progn
	(newspeak-safe-forward-sexp)
        (skip-chars-forward ":")
        (if (newspeak-looking-back-keyword-send)
            (point)
          (newspeak-next-keyword-1))))))

(defun newspeak-previous-keyword ()
  (or (save-excursion (newspeak-previous-keyword-1)) (point)))

(defun newspeak-next-keyword ()
  (or (save-excursion (newspeak-next-keyword-1)) (point)))

(provide 'newspeak-mode)

;;; newspeak-mode.el ends here
