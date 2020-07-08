;;; affiliated-buffers-test.el --- tests of affiliated buffers -*- lexical-binding: t -*-

;; COPYRIGHT

;; Copyright © 2020 Douglas Lewan, d.lewan2000@gmail.com
;; All rights reserved.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Douglas Lewan <d.lewan2000@gmail.com>
;; Maintainer: Douglas Lewan <d.lewan2000@gmail.com>
;; Created: 2020 Jun 05
;; Version: 0.01
;; Keywords: 

;;; Commentary:

;;; Documentation:

;;; Code:

;;
;; Development
;;
(defun etst-renumber-tests ()
  "Renumber all tests of ETS so that each file's tests start at 000.
Tests are numbered in the order declared.
The intent is that the order in which they are defined is
the order in which they are meaningful."
  (interactive)
  (let ((fname "etst-renumber-tests")
	(etst-files (directory-files "."
				     (not 'full)
				     ".+-test.el\\'"))
	(test-nr 0)
	(end))
    (mapc (lambda (f)
	    (setq test-nr 0)
	    ;; (with-current-buffer (find-file-noselect "etst.el")
	    (with-current-buffer (find-file-noselect f)
	      (save-excursion
		(save-restriction (widen)
				  (goto-char (point-min))
				  (save-match-data
				    ;; This may not catch faux test definitions in strings, but that's OK.
				    (while (re-search-forward "^(ert-deftest\\s-+\\([[:digit:]]+\\)-" (point-max) t)
				      (replace-match (format "%03d" test-nr) nil nil nil 1)
				      (if (save-excursion (re-search-forward "^(ert-deftest\\s-" (point-max) t))
					  (setq end (match-beginning 0))
					(setq end (point-max)))
				      (while (re-search-forward "\"\\([[:digit:]]\\{3\\}\\)-" end t)
					(replace-match (format "%03d" test-nr) nil nil nil 1))
				      (setq test-nr (1+ test-nr))))))))
	  etst-files)))

(defun etst-new-tests (arg)
  "Create a new ert test defintion for the name under point
and the next ( ARG - 1) target columns.
The generated test's name is prepended with '000-ets-test-.
Use M-x etst-renumber-tests to renumber the tests properly."
  (interactive "p")
  (let ((fname "etst-new-tests")
	(names ())
	(target-column (current-column)))
    (save-excursion
      (while (< 0 arg)
	(push (thing-at-point 'symbol) names)
	(forward-line)
	(ets-goto-column target-column)
	(setq arg (1- arg))))
    (setq names (reverse names))
    (save-excursion)
    (end-of-defun)
    (mapc (lambda (n)
	    (insert (format "
\(ert-deftest 000-%s ()
  \"Test that DESCRIBE THE TEST HERE.\"
  (let ((tname \"000-%s\"))
    (error \"000-%s is not yet implemented.\" tname)))
" n n n)))
	  names)))

(defun etst-new-should ()
  "Insert a skeletal (should) structure at point."
  (interactive)
  (let ((fname "etst-new-should"))
    (insert "
    (should (and (message \"%s(): Checking ...\" tname)
		 ()))
")))

(defun etst-new-should-not ()
  "Insert a skeletal (should-not) structure at point."
  (interactive)
  (let ((fname "etst-new-should-not"))
    (insert "
    (should-not (and (message \"%s(): Checking that ... is false.\" tname)
		 ()))
")))

(defun etst-new-should-error ()
  "Insert a skeletal (should-error) structure at point."
  (interactive)
  (let ((fname "etst-new-should-error"))
    (insert "
    (should-error (and (message \"%s(): Checking that ... errors out.\" tname)
		 ()))
")))

(defun ets-goto-column (column)
  "Move to the given COLUMN if possible.
That is, don't go off the end of the line and
don't go into tabs."
  (let ((fname "goto-column"))
    (beginning-of-line)
    (while (and (< (point) (1- (line-end-position)))
		(< (current-column) column))
      (forward-char 1))))

(defun abt-dbg ()
  "Insert a debuger line."
  (interactive)
  (let ((fname "abt-dbg"))
    (beginning-of-line)
    (if (looking-at "\\s-*$")
	(insert (format "    (message \"%%s():     %d.\" tname)" (count-lines (point-min) (point))))
      (insert (format "    (message \"%%s():     %d.\" tname)\n" (count-lines (point-min) (point)))))))


;;
;; Generic functions
;;

(defun local-variable-exists-p (buffer variable)
  "Return non-NIL if VARIABLE is local to BUFFER and bound there."
  (let ((fname "local-variable-exists"))
    (if (buffer-live-p buffer)
	(with-current-buffer buffer
	  (and (local-variable-p variable)
	       (boundp variable)))
      nil)))


;;
;; Dependencies
;;
(require 'cl)				;for (caddr).
(require 'ert)
(if (file-exists-p "affiliated-buffers.elc")
    (load-file "affiliated-buffers.elc")
  (load-file "affiliated-buffers.el"))

;;
;; Vars
;;

(defvar abtsb-root-name "abtsb-root"
  "Name of the root buffer in the abt-stock-buffers affiliation.")
(defvar abtsb-child-0-name "abtsb-child-0"
  "Name of the first child in the abt-stock-buffers affiliation.")
(defvar abtsb-child-1-name "abtsb-child-1"
  "Name of the second child in the abt-stock-buffers affiliation.")
(defvar abtsb-child-2-name "abtsb-child-2"
  "Name of the third-child in the abt-stock-buffers affiliation.")
(defvar abtsb-grandchild-00-name "abtsb-grandchild-00"
  "Name of the a child of the first child in the abt-stock-buffers affiliation.")
(defvar abtsb-grandchild-01-name "abtsb-grandchild-01"
  "Name of the a child of the first child in the abt-stock-buffers affiliation.")
(defvar abtsb-grandchild-20-name "abtsb-grandchild-20"
  "Name of the a child of the third child in the abt-stock-buffers affiliation.")

(defvar abtsb-root ()
  "Name of the root buffer in the abt-stock-buffers affiliation.")
(defvar abtsb-child-0 ()
  "Name of the root buffer in the abt-stock-buffers affiliation.")
(defvar abtsb-child-1 ()
  "Name of the root buffer in the abt-stock-buffers affiliation.")
(defvar abtsb-child-2 ()
  "Name of the root buffer in the abt-stock-buffers affiliation.")
(defvar abtsb-grandchild-00 ()
  "Name of the root buffer in the abt-stock-buffers affiliation.")
(defvar abtsb-grandchild-01 ()
  "Name of the root buffer in the abt-stock-buffers affiliation.")
(defvar abtsb-grandchild-20 ()
  "Name of the root buffer in the abt-stock-buffers affiliation.")

(defvar abtsb-other-buffer (get-buffer-create "abtsb-other-buffer")
  "A buffer outside the standard hierarchy of affiliation.")


;;
;; Library
;;

(defun abt-stock-buffers ()
  "Create a small but non-trivially complex hierarchy of affiliations.
this also cleans those buffers up if they already exist.
Here's what the stock buffer hierarchy of affiliations looks like.
    abtsb-root
	├─── abtsb-child-0
	│      ├─── abtsb-grandchild-00
	│      └─── abtsb-grandchild-01
	├─── abtsb-child-1
	└─── abtsb-child-2
	       └─── abtsb-grandchild-20
"
  (let ((fname "abt-stock-buffers"))
    (abt-clean-up-stock-buffers)
    (setq abtsb-root (get-buffer-create abtsb-root-name))
    (setq abtsb-child-0 (get-buffer-create abtsb-child-0-name))
    (setq abtsb-child-1 (get-buffer-create abtsb-child-1-name))
    (setq abtsb-child-2 (get-buffer-create abtsb-child-2-name))
    (setq abtsb-grandchild-00 (get-buffer-create abtsb-grandchild-00-name))
    (setq abtsb-grandchild-01 (get-buffer-create abtsb-grandchild-01-name))
    (setq abtsb-grandchild-20 (get-buffer-create abtsb-grandchild-20-name))

    (unless (buffer-live-p abtsb-other-buffer)
      (setq abtsb-other-buffer  (get-buffer-create "abtsb-other-buffer")))

    (with-current-buffer abtsb-root
      (ab-init)
      (ab-new-buffer abtsb-child-1)
      (ab-new-buffer abtsb-child-0)
      (with-current-buffer abtsb-child-0
	(ab-new-buffer abtsb-grandchild-00)
	(ab-new-buffer abtsb-grandchild-01))
      (ab-new-buffer abtsb-child-2)
      (with-current-buffer abtsb-child-2
	(ab-new-buffer abtsb-grandchild-20)))))

(defun abt-clean-up-stock-buffers ()
  "Delete all the buffers of the stock hierarchy of affiliations.
Also kill any buffer local variables in abtsb-other-buffer."
  (let ((fname "abt-clean-up-stock-buffers"))
    (mapc (lambda (b)
	    (if (buffer-live-p b)
		(mapc (lambda (bb)
			(cond ((string-equal (buffer-name b) bb)
			       (kill-buffer b))
			      (t t)))
		      (list abtsb-root-name
			    abtsb-child-0-name
			    abtsb-child-1-name
			    abtsb-child-2-name
			    abtsb-grandchild-00-name
			    abtsb-grandchild-01-name
			    abtsb-grandchild-20-name))))
	  (buffer-list))

    (if (buffer-live-p abtsb-other-buffer)
	(with-current-buffer abtsb-other-buffer
	  (kill-all-local-variables)))))

(defun abt-random-element (l)
  "Return a random element from the given List."
  (let ((fname "abt-random-element"))
    (unless (listp l)
      (signal 'wrong-type-error (list l)))
    (nth (random (length l)) l)))

(defun abt-clean-buffers (&rest buffers)
  "Kill the given BUFFERS if they are live."
  (let ((fname "abt-clean-buffers"))
    (mapc (lambda (b)
	    (if (buffer-live-p b)
		(kill-buffer b)))
	  buffers)
    (setq file-buffers ())))

(defun abt-stock-some-file-buffers (&optional all)
  "Create an affiliation with some file buffers.
If the optional argument ALL is non-NIL, then all the buffers are file buffers.
The affiliation has the same structure as the one constructed in (abt-stock-buffers).
If ALL is NIL, then the starred buffers below are file buffers.
    abtsb-root*
	├─── abtsb-child-0
	│      ├─── abtsb-grandchild-00*
	│      └─── abtsb-grandchild-01
	├─── abtsb-child-1*
	└─── abtsb-child-2
	       └─── abtsb-grandchild-20*
"
  (let ((fname "abt-stock-some-file-buffers"))
    (abt-clean-up-stock-buffers)
    (setq file-buffers ())

    (setq abtsb-root (find-file-noselect abtsb-root-name))
    (setq abtsb-child-0 (cond (all (find-file-noselect abtsb-child-0-name))
			      (t (get-buffer-create abtsb-child-0-name))))
    (setq abtsb-child-1 (find-file-noselect abtsb-child-1-name))
    (setq abtsb-child-2 (cond (all (find-file-noselect abtsb-child-2-name))
			      (t (get-buffer-create abtsb-child-2-name))))
    (setq abtsb-grandchild-00 (find-file-noselect abtsb-grandchild-00-name))
    (setq abtsb-grandchild-01 (cond (all (find-file-noselect abtsb-grandchild-01-name))
				    (t (get-buffer-create abtsb-grandchild-01-name))))
    (setq abtsb-grandchild-20 (find-file-noselect abtsb-grandchild-20-name))
    (unless (buffer-live-p abtsb-other-buffer)
      (setq abtsb-other-buffer  (get-buffer-create "abtsb-other-buffer")))
    (with-current-buffer abtsb-root
      (ab-init)
      (ab-new-buffer abtsb-child-0)
      (with-current-buffer abtsb-child-0
	(ab-new-buffer abtsb-grandchild-00)
	(ab-new-buffer abtsb-grandchild-01))
      (ab-new-buffer abtsb-child-1)
      (ab-new-buffer abtsb-child-2)
      (with-current-buffer abtsb-child-2
	(ab-new-buffer abtsb-grandchild-20)))
    (mapc (lambda (b)
	    (if (buffer-file-name b)
		(push b file-buffers)))
	  (list abtsb-root
		abtsb-child-0
		abtsb-child-1
		abtsb-child-2
		abtsb-grandchild-00
		abtsb-grandchild-01
		abtsb-grandchild-20))
    file-buffers))

(defun abt-sorted-p (l)
  "Return non-NIL if the List of buffers is sorted by filename."
  (let ((fname "abt-sorted-p")
	(previous nil)
	(result t)			; We'll figure this out.
	)
    (catch 'not-sorted
      (mapc (lambda (b)
	      (cond ((null previous)
		     (setq previous b))
		    (t
		     (if (string-lessp (buffer-name previous) (buffer-name b))
			 t
		       (setq result nil)
		       (throw 'not-sorted nil))))
	      (setq previous b))
	    l))
    result))

(defun abt-bump (buffer)
  "Bump the value of alpha in BUFFER; return the bumped result."
  (let ((fname "abt-bump"))
    (with-current-buffer buffer
      (let ((value (1+ alpha)))
	(ab-setq alpha value))
      alpha)))


;;
;; Commands
;;


;;
;; Test definitions
;;
(ert-deftest 000-uninitialized ()
  "Test a buffer that is uninitialized for affiliated-buffers."
  (let ((tname "000-uninitialized")
	(buf (get-buffer-create "000-uninitialized")))
    (with-current-buffer buf
      (mapc (lambda (v)
	      (cond ((eq v 'ab-protected-variables)
		     (should (and (message "%s(): Expecting »%S« to be a list of variables." tname v)
				  (listp (eval v))
				  (consp (eval v))))
		     (mapc (lambda (vv)
			     (should (and (message "%s(): Expecting »%S« to be a symbol." tname vv)
					  (symbolp vv))))
			   (eval v)))
		    (t
		     (should (and (message "%s(): Expecting »%S« to be null." tname v)
				  (null (eval v))))))
	      (should-not (and (message "%s(): Expecting that variable »%S« is not a buffer-local variable." tname v)
			       (local-variable-p v))))
	    ab-protected-variables))
    (kill-buffer buf)))

(ert-deftest 001-initialized ()
  "Test how a buffer is initialized."
  (let ((tname "001-initialized")
	(buf-name "001-initialized")
	(buf))
    (mapc (lambda (b)
	    (if (string-equal (buffer-name b) buf-name)
		(kill-buffer b)))
	  (buffer-list))
    (setq buf (get-buffer-create buf-name))
    (with-current-buffer buf
      (ab-init)
      (mapc (lambda (v)
	      (cond ((eq v 'ab-protected-variables) ;This is not local.
		     (should-not (and (message "%s(): Expecting that variable »%S« is not a buffer-local variable." tname v)
				      (local-variable-p v)))
		     (should (and (message "%s(): Expecting »%S« to be a list of variables." tname (symbol-name v))
				  (listp (eval v))
				  (consp (eval v))))
		     (mapc (lambda (vv)
			     (should (and (message "%s(): Expecting »%S« to be a symbol." tname vv)
					  (symbolp vv))))
			   (eval v)))
		    ((eq v 'ab-initialized)
		     (should (and (message "%s(): Expecting »%S« to be t." tname v)
				  (eq t (eval v)))))
		    (t
		     (should (and (message "%s(): Expecting »%S« to be null." tname v)
				  (null (eval v))))
		     (should (and (message "%s(): Expecting that variable »%S« is indeed a buffer-local variable." tname v)
				  (local-variable-p v))))))
	    ab-protected-variables))
    (abt-clean-buffers buf)))

(ert-deftest 002-root-single-existing-child ()
  "Test that an initialized root with exactly one child really has exactly one child, and
that that child is correctl configured for affiliation."
  (let ((tname "002-root-single-child")
	(root-buffer-name "002-root-single-existing-child-root")
	(child-buffer-name "002-root-single-existing-child-child")
	(root-buffer)
	(child-buffer))
    (setq root-buffer  (get-buffer-create root-buffer-name))
    (setq child-buffer (get-buffer-create child-buffer-name))
    (with-current-buffer root-buffer
      (ab-init)
      (should (and (message "%s(): Checking the return value of (ab-new-buffer)." tname child-buffer)
		   (eq child-buffer (ab-new-buffer child-buffer))))
      (should (and (message "%s(): Checking that there's exactly one child." tname)
		   (= 1 (length ab-children))))
      (should (and (message "%s(): Checking that that one is the right one, »%S«." tname child-buffer)
		   (eq child-buffer (car ab-children))))
      (mapc (lambda (v)
	      (cond ((eq v 'ab-protected-variables)
		     (should-not (and (message "%s(): Checking that ab-protected-variables is not local." tname)
				      (local-variable-exists-p (current-buffer) v))))
		    ((eq v 'ab-parent)
		     (should (and (message "%s(): Checking that ab-parent is local." tname)
				  (local-variable-p v)))
		     (should (and (message "%s(): Checking the value of ab-parent." tname)
				  (eq nil (eval v)))))
		    ((or (eq v 'ab-initialized)
			 (eq v 'ab-children))
		     (should (and (message "%s(): Checking that »%S« is not null." tname v)
				  ab-initialized)))
		    (t
		     (should (and (message "%s(): Checking that »%S« is nil." tname v)
				  (null (eval v)))))))
	  ab-protected-variables))
    (abt-clean-buffers root-buffer child-buffer)))

(ert-deftest 003-root-single-new-child ()
  "Test that an initialized root with exactly one child really has exactly one child, and
that that child is correctly configured for affiliation."
  (let ((tname "003-root-single-child")
	(root-buffer-name "003-root-single-existing-child-root")
	(child-buffer-name "003-root-single-existing-child-child")
	(root-buffer)
	(child-buffer))
    (mapc (lambda (b)
	    (if (or (string-equal (buffer-name b) root-buffer-name)
		    (string-equal (buffer-name b) child-buffer-name))
		(kill-buffer b)))
	  (buffer-list))
    (setq root-buffer	     (get-buffer-create root-buffer-name))
    (with-current-buffer root-buffer
      (ab-init)
      (should (and (message "%s(): Checking the return value of (ab-new-buffer)." tname child-buffer)
		   (eq (get-buffer-create child-buffer-name) (setq child-buffer (ab-new-buffer child-buffer-name)))))
      (should (and (message "%s(): Checking that there's exactly one child." tname)
		   (= 1 (length ab-children))))
      (should (and (message "%s(): Checking that that one is the right one, »%S«." tname child-buffer)
		   (eq child-buffer (car ab-children)))))
    (with-current-buffer child-buffer
      (mapc (lambda (v)
	      (cond ((eq v 'ab-protected-variables)
		     (should-not (and (message "%s(): Checking that ab-protected-variables is not local." tname)
				      (local-variable-p v))))
		    ((eq v 'ab-parent)
		     (should (and (message "%s(): Checking that ab-parent is local." tname)
				  (local-variable-p v)))
		     (should (and (message "%s(): Checking the value of ab-parent." tname)
				  (eq root-buffer (eval v)))))
		    (t
		     (should (and (message "%s(): Checking that »%S« is nil." tname v)
				  (null (eval v)))))))
	    ab-protected-variables))
    (abt-clean-buffers root-buffer child-buffer)))

(ert-deftest 004-root-two-existing-children ()
  "Test that an initialized root with exactly two children really has
exactly two children, and
that those children are correctly configured for affiliation."
  (let ((tname "004-root-single-child")
	(root-buffer-name "004-root-single-existing-child-root")
	(child-buffer-0-name "004-root-single-existing-child-child-0")
	(child-buffer-1-name "004-root-single-existing-child-child-1")
	(root-buffer)
	(child-buffer-0)
	(child-buffer-1))
    (mapc (lambda (b)
	    (if (or (string-equal (buffer-name b) root-buffer-name)
		    (string-equal (buffer-name b) child-buffer-0-name)
		    (string-equal (buffer-name b) child-buffer-1-name))
		(kill-buffer b)))
	  (buffer-list))
    (setq root-buffer	 (get-buffer-create root-buffer-name))
    (setq child-buffer-0 (get-buffer-create child-buffer-0-name))
    (setq child-buffer-1 (get-buffer-create child-buffer-1-name))
    (with-current-buffer root-buffer
      (ab-init)
      (should (and (message "%s(): Checking the return value of (ab-new-buffer). (0)" tname child-buffer-0)
		   (eq child-buffer-0 (ab-new-buffer child-buffer-0))))
      (should (and (message "%s(): Checking the return value of (ab-new-buffer). (1)" tname child-buffer-1)
		   (eq child-buffer-1 (ab-new-buffer child-buffer-1))))
      (should (and (message "%s(): Checking that there are exactly two children." tname)
		   (= 2 (length ab-children))))
      (should (and (message "%s(): Checking that they are the right ones, »%S« and »%S«." tname child-buffer-0 child-buffer-1)
		   (member child-buffer-0 ab-children)
		   (member child-buffer-1 ab-children))))
    (mapc (lambda (b)
	    (with-current-buffer b
	      (mapc (lambda (v)
		      (cond ((eq v 'ab-protected-variables)
			     (should-not (and (message "%s(): Checking that ab-protected-variables is not local." tname)
					      (local-variable-p v))))
			    ((eq v 'ab-parent)
			     (should (and (message "%s(): Checking that ab-parent is local." tname)
					  (local-variable-p v)))
			     (should (and (message "%s(): Checking the value of ab-parent." tname)
					  (eq root-buffer (eval v)))))
			    (t
			     (should (and (message "%s(): Checking that »%S« is nil." tname v)
					  (null (eval v)))))))
		    ab-protected-variables)))
	  (list child-buffer-0 child-buffer-1))
    (abt-clean-buffers root-buffer child-buffer-0 child-buffer-1)))

(ert-deftest 005-root-two-new-children ()
  "Test that an initialized root with exactly two children really has
exactly two children, and
that those children are correctly configured for affiliation."
  (let ((tname "005-root-single-child")
	(root-buffer-005-name "005-root-single-existing-child-root")
	(child-buffer-005-0-name "005-root-single-existing-child-child-0")
	(child-buffer-005-1-name "005-root-single-existing-child-child-1")
	(root-buffer-005)
	(child-buffer-005-0)
	(child-buffer-005-1))
    (mapc (lambda (b)
	    (if (or (string-equal (buffer-name b) root-buffer-005-name)
		    (string-equal (buffer-name b) child-buffer-005-0-name)
		    (string-equal (buffer-name b) child-buffer-005-1-name))
		(kill-buffer b)))
	  (buffer-list))
    (setq root-buffer-005	       (get-buffer-create root-buffer-005-name))
    ;; (setq child-buffer-005-0 (get-buffer-create child-buffer-005-0-name))
    ;; (setq child-buffer-005-1 (get-buffer-create child-buffer-005-1-name))
    (with-current-buffer root-buffer-005
      (ab-init)
      (should (and (message "%s(): Checking the return value of (ab-new-buffer). (0)" tname child-buffer-005-0)
		   (eq (get-buffer-create child-buffer-005-0-name)
		       (setq child-buffer-005-0
			     (ab-new-buffer child-buffer-005-0-name)))))
      (should (and (message "%s(): Checking the return value of (ab-new-buffer). (1)" tname child-buffer-005-1)
		   (eq (get-buffer-create child-buffer-005-1-name)
		       (setq child-buffer-005-1
			     (ab-new-buffer child-buffer-005-1-name)))))
      (should (and (message "%s(): Checking that there are exactly two children." tname)
		   (= 2 (length ab-children))))
      (should (and (message "%s(): Checking that they are the right ones, »%S« and »%S«." tname child-buffer-005-0 child-buffer-005-1)
		   (member child-buffer-005-0 ab-children)
		   (member child-buffer-005-1 ab-children))))
    (mapc (lambda (b)
	    (with-current-buffer b
	      (mapc (lambda (v)
		      (cond ((eq v 'ab-protected-variables)
			     (should-not (and (message "%s(): Checking that ab-protected-variables is not local." tname)
					      (local-variable-p v))))
			    ((eq v 'ab-parent)
			     (should (and (message "%s(): Checking that ab-parent is local." tname)
					  (local-variable-p v)))
			     (should (and (message "%s(): Checking the value of ab-parent." tname)
					  (eq root-buffer-005 (eval v)))))
			    (t
			     (should (and (message "%s(): Checking that »%S« is nil." tname v)
					  (null (eval v)))))))
		    ab-protected-variables)))
	  (list child-buffer-005-0 child-buffer-005-1))
    (abt-clean-buffers root-buffer-005 child-buffer-005-0 child-buffer-005-1)))

(ert-deftest 006-root-multiple-mixed-children ()
  "Test that (ab-new-buffer) does the right things when adding a mixture of new and existing buffers."
  (let ((tname "006-root-multiple-mixed-children")
	(root-buffer-name "006-root-multiple-mixed-children")
	(root-buffer)
	(child-names (list "006-root-multiple-mixed-children-0"
			   "006-root-multiple-mixed-children-1"
			   "006-root-multiple-mixed-children-2"
			   "006-root-multiple-mixed-children-3"
			   "006-root-multiple-mixed-children-5"
			   "006-root-multiple-mixed-children-6"
			   "006-root-multiple-mixed-children-7"
			   "006-root-multiple-mixed-children-8"
			   "006-root-multiple-mixed-children-9"
			   "006-root-multiple-mixed-children-10"
			   "006-root-multiple-mixed-children-11"
			   "006-root-multiple-mixed-children-12"))
	(existing-children ())
	(buf)
	(rand -1))
    (mapc (lambda (b)
	    (if (or (string-equal (buffer-name b) root-buffer-name)
		    (string-match (regexp-opt child-names) (buffer-name b)))
		(kill-buffer b)))
	  (buffer-list))
    (with-current-buffer (setq root-buffer (get-buffer-create root-buffer-name))
      (ab-init)
      ;; Make sure there's one child.
      (push (get-buffer-create (car child-names))
	    existing-children)
      (mapc (lambda (bn)
	      (cond ((= (setq rand (random 2)) 0)
		     ;; HEREHERE I still need to figure out how to carry existence forward to the invocation of (ab-new-buffer).
		     (push (get-buffer-create bn) existing-children))
		    ((= rand 1))
		    (t
		     (error "%s(): Impossible condition." tname))))
	    (cddr child-names))	; Make sure one buffer is not a child.
      (mapc (lambda (bn)
	      (cond ((setq buf (get-buffer bn))
		     (should (and (message "%s(): Checking the return value of (ab-new-buffer) for the existing buffer »%S«." tname buf)
				  (eq (get-buffer-create bn) (ab-new-buffer buf))))
		     (push buf existing-children))
		    (t
		     (should (and (message "%s(): Checking the return value of (ab-new-buffer) for the new buffer »%s«." tname bn)
				  (eq (get-buffer-create bn) (setq buf (ab-new-buffer bn)))))
		     (setq existing-children (add-to-list 'existing-children buf)))))
	    child-names)
      (should (and (message "%s(): Checking the length of child-buffers." tname)
		   (= (length child-names) (length ab-children))))
      (should (and (message "%s(): Checking that the root buffer has no parent." tname)
		   (null ab-parent)))
      (mapc (lambda (b)
	      (should (and (message "%s(): Checking that »%S« is a child buffer." tname b)
			   (member b ab-children))))
	    existing-children))
    (mapc (lambda (b)
	    (with-current-buffer b
	      (should (and (message "%s(): Checking that »%S« has no children." tname b)
			   (null ab-children)))
	      (should (and (message "%s(): Checking that the parent of »%S« is »%S«." tname b root-buffer)
			   (eq root-buffer ab-parent)))))
	  existing-children)
    (apply #'abt-clean-buffers root-buffer (with-current-buffer root-buffer existing-children))))

(ert-deftest 007-root-multiple-children-ab-root ()
  "Test that (ab-root-buffer) returns the right values
for a root and each of multiple children."
  (let* ((tname "007-root-multiple-children-ab-root")
	 (root-buffer-name "root-buffer-007")
	 (root-buffer-007 (get-buffer-create root-buffer-name))
	 (child-buffers)
	 (this-buffer)
	 (i 0)
	 (max 5))
    (with-current-buffer root-buffer-007
      (ab-init)
      (while (< i max)
	(setq this-buffer (get-buffer-create (format "child-buffer-007-%d" i)))
	(ab-new-buffer this-buffer)
	(push this-buffer child-buffers)
	(should (and (message "%s(): Confirming the return value of (ab-new-buffer)." tname)
		     (equal this-buffer (ab-new-buffer this-buffer))))
	(setq i (1+ i))))
    (should (and (message "%s(): Checking that the root buffer knows the right root-buffer." tname)
		 (eq root-buffer-007 (ab-root-buffer root-buffer-007))))
    (mapc (lambda (b)
	    (with-current-buffer b
	      (should (and (message "%s(): Checking that »%S« knows the right parent." tname (buffer-name b))
			   (message "                »%S« eq »%S«." root-buffer-007 (ab-root-buffer b))
			   (equal root-buffer-007 (ab-root-buffer))))))
	  child-buffers)
    (apply #'abt-clean-buffers root-buffer-007 child-buffers)))


(ert-deftest 008-two-simple-generations ()
  "Test creation of a simple hierarchy of depth 3 with two generations
of child buffers.
    root
     └── child
           └─── grandchild."
  (let* ((tname "008-two-simple-generations")
	 (root-buffer-name "008-root")
	 (child-buffer-name "008-child")
	 (grandchild-buffer-name "008-grandchild")
	 (root-buffer)
	 (child-buffer)
	 (grandchild-buffer))
    (mapc (lambda (b)
	    (if (or (string-equal (buffer-name b) root-buffer-name)
		    (string-equal (buffer-name b) child-buffer-name)
		    (string-equal (buffer-name b) grandchild-buffer-name))
		(kill-buffer b)))
	  (buffer-list))
    (with-current-buffer (setq root-buffer (get-buffer-create root-buffer-name))
      (ab-init))
    (with-current-buffer root-buffer
      (setq child-buffer (ab-new-buffer child-buffer-name)))
    (with-current-buffer child-buffer
      (setq grandchild-buffer (ab-new-buffer grandchild-buffer-name)))
    (with-current-buffer root-buffer
      (should (and (message "%s(): Checking that the root buffer »%S« has no parent." tname (current-buffer))
		   (null ab-parent)))
      (should (and (message "%s(): Checking that the root buffer »%S« has exactly one child..." tname(current-buffer))
		   (= 1 (length ab-children))))
      (should (and (message "%s(): Checking that that child »%S« is the expected child buffer »%S«." tname (car ab-children) child-buffer)
		   (eq child-buffer (car ab-children)))))
    (with-current-buffer child-buffer
      (should (and (message "%s(): Checking that the child-buffer »%S« has the root buffer »%S« as its parent." tname (current-buffer) root-buffer)
		   (eq root-buffer ab-parent)))
      (should (and (message "%s(): Checking that the child buffer »%S« has exactly one child." tname (current-buffer))
		   (= 1 (length ab-children))))
      (should (and (message "%s(): Checking that that child is the grandchild buffer »%S«." tname grandchild-buffer)
		   (eq grandchild-buffer (car ab-children)))))
    (abt-clean-buffers root-buffer child-buffer grandchild-buffer)))

(ert-deftest 009-two-children-two-generations ()
  "Test creation of a two generation hierarchy with two children at each level.
    root
     ├─── child-0
     │      ├─── grandchild-00
     │	    └─── grandchild-01
     └─── child─1
	    ├─── grandchild-10
	    └─── grandchild-11
"
  (let ((tname "009-two-children-two-generations")
	;; HEREHERE There has to be a better structure for this.
	(root-buffer-name "009-root")
	(child-buffer-0-name "009-child-0")
	(child-buffer-1-name "009-child-1")
	(grandchild-buffer-00-name "009-grandchild-00")
	(grandchild-buffer-01-name "009-grandchild-01")
	(grandchild-buffer-10-name "009-grandchild-10")
	(grandchild-buffer-11-name "009-grandchild-11")
	(root-buffer)
	(child-buffer-0)
	(child-buffer-1)
	(grandchild-buffer-00)
	(grandchild-buffer-01)
	(grandchild-buffer-10)
	(grandchild-buffer-11))
    (mapc (lambda (b)
	    (mapc (lambda (bb)
		    (if (string-equal (buffer-name b) bb)
			(kill-buffer b)))
		  (list root-buffer-name
			child-buffer-0-name
			child-buffer-1-name
			grandchild-buffer-00-name
			grandchild-buffer-01-name
			grandchild-buffer-10-name
			grandchild-buffer-11-name)))
	    (buffer-list))
    (with-current-buffer (setq root-buffer (get-buffer-create root-buffer-name))
      (ab-init)
      (setq child-buffer-0 (ab-new-buffer child-buffer-0-name))
      (setq child-buffer-1 (ab-new-buffer child-buffer-1-name)))
    (with-current-buffer child-buffer-0
      (setq grandchild-buffer-00 (ab-new-buffer grandchild-buffer-00-name))
      (setq grandchild-buffer-01 (ab-new-buffer grandchild-buffer-01-name)))
    (with-current-buffer child-buffer-1
      (setq grandchild-buffer-10 (ab-new-buffer grandchild-buffer-10-name))
      (setq grandchild-buffer-11 (ab-new-buffer grandchild-buffer-11-name)))
    (with-current-buffer root-buffer
      (should (and (message "%s(): Checking that the root buffer »%S« has no parent." tname (current-buffer))
		   (null ab-parent)))
      (should (and (message "%s(): Checking that the root buffer »%S« has exactly two children..." tname (current-buffer))
		   (= 2 (length ab-children))))
      (mapc (lambda (c)
	      (should (and (message "%s(): Checking that »%S« is one child buffer." tname c)
			   (member c ab-children))))
	    (list child-buffer-0 child-buffer-1))
      (mapc (lambda (gc)
	      (should-not (and (message "%s(): Checking that »%S« is not a child of root." tname gc)
			       (member gc ab-children))))
	    (list grandchild-buffer-00-name
		  grandchild-buffer-01-name
		  grandchild-buffer-10-name
		  grandchild-buffer-11-name)))
    (mapc (lambda (ci)
	    (let ((c (car ci))
		  (gcs (cdr ci)))
	      (with-current-buffer c
		(should (and (message "%s(): Checking that »%s« is the parent of »%S«." tname root-buffer c)
			     (eq root-buffer ab-parent)))
		(should (and (message "%s(): Checking that »%S« has exactly two children." tname (current-buffer))
			     (= 2 (length ab-children))))
		(mapc (lambda (gc)
			(should (and (message "%s(): Checking that »%S« is one of those children." tname gc)
				     (member gc ab-children)))
			(with-current-buffer gc
			  (should (and (message "%s(): Checking that »%S« has »%S« as a parent" tname (current-buffer) c)
				       (eq c ab-parent)))
			  (should (and (message "%s(): Checking that »%S« has no children" tname (current-buffer))
				       (null ab-children)))))
		      gcs))))
	  (list (list child-buffer-0 grandchild-buffer-00 grandchild-buffer-01)
		(list child-buffer-1 grandchild-buffer-10 grandchild-buffer-11)))
    (abt-clean-buffers root-buffer
		       child-buffer-0
		       child-buffer-1
		       grandchild-buffer-00
		       grandchild-buffer-01
		       grandchild-buffer-10
		       grandchild-buffer-11)))

(ert-deftest 010-multiple-children-multiple-generations ()
  "Test the creation of a small, but complex hierarchy
with multiple children each having multiple descendents.
For most uses this is probably overkill;
it's the most complex hierrchy of affiliations in these tests.
All tests hereafter use the stock hierrchy of affiliations defined by (abt-stock-buffers).

For completeness here's a picture of this hierarchy
    root-buffer
        ├─── child-buffer-0
        │        ├─── grandchild-buffer-00
        │        │       ├─── greatgrandchild-buffer-000
        │        │       ├─── greatgrandchild-buffer-001
        │        │       └─── greatgrandchild-buffer-002
        │        └─── grandchild-buffer-01
        ├─── child-buffer-1
        │        ├─── grandchild-buffer-10
        │        │       ├─── greatgrandchild-buffer-100
        │        │       └─── greatgrandchild-buffer-101
        │        │    grandchild-buffer-11
        │        │       └─── greatgrandchild-buffer-110
        │        └─── grandchild-buffer-12
        ├─── child-buffer-2
        │        ├─── grandchild-buffer-20
        │        │       ├─── greatgrandchild-buffer-200
        │        │       ├─── greatgrandchild-buffer-201
        │        │       └─── greatgrandchild-buffer-202
        │        ├─── grandchild-buffer-21
        │        │       ├─── greatgrandchild-buffer-210
        │        │       └─── greatgrandchild-buffer-212
        │        ├─── grandchild-buffer-22
        │        └─── grandchild-buffer-23
        │                ├─── greatgrandchild-buffer-230
        │                ├─── greatgrandchild-buffer-231
        │                ├─── greatgrandchild-buffer-232
        │                └─── greatgrandchild-buffer-233
        ├─── child-buffer-3
        │        ├─── grandchild-buffer-30
        │        │       └─── greatgrandchild-buffer-300
        │        ├─── grandchild-buffer-31
        │        │       ├─── greatgrandchild-buffer-310
        │        │       └─── greatgrandchild-buffer-312
        │        └─── grandchild-buffer-32
        └─── child-buffer-4
"
  (let* ((tname "010-multiple-children-multiple-generations")
	 (root-buffer-name "root-buffer")

	 (child-buffer-0-name "child-buffer-0")
	 (child-buffer-1-name "child-buffer-1")
	 (child-buffer-2-name "child-buffer-2")
	 (child-buffer-3-name "child-buffer-3")
	 (child-buffer-4-name "child-buffer-4")

	 (grandchild-buffer-00-name "grandchild-buffer-00")
	 (grandchild-buffer-01-name "grandchild-buffer-01")

	 (grandchild-buffer-10-name "grandchild-buffer-10")
	 (grandchild-buffer-11-name "grandchild-buffer-11")
	 (grandchild-buffer-12-name "grandchild-buffer-12")

	 (grandchild-buffer-20-name "grandchild-buffer-20")
	 (grandchild-buffer-21-name "grandchild-buffer-21")
	 (grandchild-buffer-22-name "grandchild-buffer-22")
	 (grandchild-buffer-23-name "grandchild-buffer-23")

	 (grandchild-buffer-30-name "grandchild-buffer-30")
	 (grandchild-buffer-31-name "grandchild-buffer-31")
	 (grandchild-buffer-32-name "grandchild-buffer-32")

	 (greatgrandchild-buffer-000-name "greatgrandchild-buffer-000")
	 (greatgrandchild-buffer-001-name "greatgrandchild-buffer-001")
	 (greatgrandchild-buffer-002-name "greatgrandchild-buffer-002")

	 (greatgrandchild-buffer-100-name "greatgrandchild-buffer-010")
	 (greatgrandchild-buffer-101-name "greatgrandchild-buffer-011")

	 (greatgrandchild-buffer-110-name "greatgrandchild-buffer-110")

	 (greatgrandchild-buffer-200-name "greatgrandchild-buffer-200")
	 (greatgrandchild-buffer-201-name "greatgrandchild-buffer-201")
	 (greatgrandchild-buffer-202-name "greatgrandchild-buffer-202")

	 (greatgrandchild-buffer-210-name "greatgrandchild-buffer-210")
	 (greatgrandchild-buffer-211-name "greatgrandchild-buffer-211")

	 (greatgrandchild-buffer-230-name "greatgrandchild-buffer-230")
	 (greatgrandchild-buffer-231-name "greatgrandchild-buffer-231")
	 (greatgrandchild-buffer-232-name "greatgrandchild-buffer-232")
	 (greatgrandchild-buffer-233-name "greatgrandchild-buffer-233")

	 (greatgrandchild-buffer-300-name "greatgrandchild-buffer-300")

	 (greatgrandchild-buffer-320-name "greatgrandchild-buffer-320")
	 (greatgrandchild-buffer-321-name "greatgrandchild-buffer-321")

	 (root-buffer)

	 (child-buffer-0)
	 (child-buffer-1)
	 (child-buffer-2)
	 (child-buffer-3)
	 (child-buffer-4)

	 (grandchild-buffer-00)
	 (grandchild-buffer-01)

	 (grandchild-buffer-10)
	 (grandchild-buffer-11)
	 (grandchild-buffer-12)

	 (grandchild-buffer-20)
	 (grandchild-buffer-21)
	 (grandchild-buffer-22)
	 (grandchild-buffer-23)

	 (grandchild-buffer-30)
	 (grandchild-buffer-31)
	 (grandchild-buffer-32)

	 (greatgrandchild-buffer-000)
	 (greatgrandchild-buffer-001)
	 (greatgrandchild-buffer-002)

	 (greatgrandchild-buffer-100)
	 (greatgrandchild-buffer-101)

	 (greatgrandchild-buffer-110)

	 (greatgrandchild-buffer-200)
	 (greatgrandchild-buffer-201)
	 (greatgrandchild-buffer-202)

	 (greatgrandchild-buffer-210)
	 (greatgrandchild-buffer-211)

	 (greatgrandchild-buffer-230)
	 (greatgrandchild-buffer-231)
	 (greatgrandchild-buffer-232)
	 (greatgrandchild-buffer-233)

	 (greatgrandchild-buffer-300)

	 (greatgrandchild-buffer-320)
	 (greatgrandchild-buffer-321)

	 (hierarchy))
    (mapc (lambda (b)
	    (mapc (lambda (bb)
		    (if (string-equal (buffer-name b) bb)
			(kill-buffer b)))
		  (list root-buffer-name
			child-buffer-0-name
			child-buffer-1-name
			child-buffer-2-name
			child-buffer-3-name
			child-buffer-4-name
			grandchild-buffer-00-name
			grandchild-buffer-01-name
			grandchild-buffer-10-name
			grandchild-buffer-11-name
			grandchild-buffer-12-name
			grandchild-buffer-20-name
			grandchild-buffer-21-name
			grandchild-buffer-22-name
			grandchild-buffer-23-name
			grandchild-buffer-30-name
			grandchild-buffer-31-name
			grandchild-buffer-32-name
			greatgrandchild-buffer-000-name
			greatgrandchild-buffer-001-name
			greatgrandchild-buffer-002-name
			greatgrandchild-buffer-100-name
			greatgrandchild-buffer-101-name
			greatgrandchild-buffer-110-name
			greatgrandchild-buffer-200-name
			greatgrandchild-buffer-201-name
			greatgrandchild-buffer-202-name
			greatgrandchild-buffer-210-name
			greatgrandchild-buffer-211-name
			greatgrandchild-buffer-230-name
			greatgrandchild-buffer-231-name
			greatgrandchild-buffer-232-name
			greatgrandchild-buffer-233-name
			greatgrandchild-buffer-300-name
			greatgrandchild-buffer-320-name
			greatgrandchild-buffer-321-name)))
	  (buffer-list))

    (setq root-buffer (get-buffer-create root-buffer-name))
    (setq child-buffer-0 (get-buffer-create child-buffer-0-name))
    (setq child-buffer-1 (get-buffer-create child-buffer-1-name))
    (setq child-buffer-2 (get-buffer-create child-buffer-2-name))
    (setq child-buffer-3 (get-buffer-create child-buffer-3-name))
    (setq child-buffer-4 (get-buffer-create child-buffer-4-name))
    (setq grandchild-buffer-00 (get-buffer-create grandchild-buffer-00-name))
    (setq grandchild-buffer-01 (get-buffer-create grandchild-buffer-01-name))
    (setq grandchild-buffer-10 (get-buffer-create grandchild-buffer-10-name))
    (setq grandchild-buffer-11 (get-buffer-create grandchild-buffer-11-name))
    (setq grandchild-buffer-12 (get-buffer-create grandchild-buffer-12-name))
    (setq grandchild-buffer-20 (get-buffer-create grandchild-buffer-20-name))
    (setq grandchild-buffer-21 (get-buffer-create grandchild-buffer-21-name))
    (setq grandchild-buffer-22 (get-buffer-create grandchild-buffer-22-name))
    (setq grandchild-buffer-23 (get-buffer-create grandchild-buffer-23-name))
    (setq grandchild-buffer-30 (get-buffer-create grandchild-buffer-30-name))
    (setq grandchild-buffer-31 (get-buffer-create grandchild-buffer-31-name))
    (setq grandchild-buffer-32 (get-buffer-create grandchild-buffer-32-name))
    (setq greatgrandchild-buffer-000 (get-buffer-create greatgrandchild-buffer-000-name))
    (setq greatgrandchild-buffer-001 (get-buffer-create greatgrandchild-buffer-001-name))
    (setq greatgrandchild-buffer-002 (get-buffer-create greatgrandchild-buffer-002-name))
    (setq greatgrandchild-buffer-100 (get-buffer-create greatgrandchild-buffer-100-name))
    (setq greatgrandchild-buffer-101 (get-buffer-create greatgrandchild-buffer-101-name))
    (setq greatgrandchild-buffer-110 (get-buffer-create greatgrandchild-buffer-110-name))
    (setq greatgrandchild-buffer-200 (get-buffer-create greatgrandchild-buffer-200-name))
    (setq greatgrandchild-buffer-201 (get-buffer-create greatgrandchild-buffer-201-name))
    (setq greatgrandchild-buffer-202 (get-buffer-create greatgrandchild-buffer-202-name))
    (setq greatgrandchild-buffer-210 (get-buffer-create greatgrandchild-buffer-210-name))
    (setq greatgrandchild-buffer-211 (get-buffer-create greatgrandchild-buffer-211-name))
    (setq greatgrandchild-buffer-230 (get-buffer-create greatgrandchild-buffer-230-name))
    (setq greatgrandchild-buffer-231 (get-buffer-create greatgrandchild-buffer-231-name))
    (setq greatgrandchild-buffer-232 (get-buffer-create greatgrandchild-buffer-232-name))
    (setq greatgrandchild-buffer-233 (get-buffer-create greatgrandchild-buffer-233-name))
    (setq greatgrandchild-buffer-300 (get-buffer-create greatgrandchild-buffer-300-name))
    (setq greatgrandchild-buffer-320 (get-buffer-create greatgrandchild-buffer-320-name))
    (setq greatgrandchild-buffer-321 (get-buffer-create greatgrandchild-buffer-321-name))

    ;; The root is special, so it doesn't go in the heirarchy.
    (setq hierarchy (list (list child-buffer-0
				(list grandchild-buffer-00
				      (list greatgrandchild-buffer-000
					    greatgrandchild-buffer-001
					    greatgrandchild-buffer-002))
				(list grandchild-buffer-01
				      (list greatgrandchild-buffer-100
					    greatgrandchild-buffer-101)))
			  (list child-buffer-1
				(list grandchild-buffer-10)
				(list grandchild-buffer-11
				      (list greatgrandchild-buffer-110))
				(list grandchild-buffer-12))
			  (list child-buffer-2
				(list grandchild-buffer-20
				      (list greatgrandchild-buffer-200
					    greatgrandchild-buffer-201
					    greatgrandchild-buffer-202))
				(list grandchild-buffer-21
				      (list greatgrandchild-buffer-210
					    greatgrandchild-buffer-211))
				(list grandchild-buffer-22)
				(list grandchild-buffer-23
				      (list greatgrandchild-buffer-230
					    greatgrandchild-buffer-231
					    greatgrandchild-buffer-232
					    greatgrandchild-buffer-233)))
			  (list child-buffer-3
				(list grandchild-buffer-30
				      (list greatgrandchild-buffer-300))
				(list grandchild-buffer-31)
				(list grandchild-buffer-32
				      (list greatgrandchild-buffer-320
					    greatgrandchild-buffer-321)))
			  (list child-buffer-4)))
    (message "%s(): For reference here is the expected hierarchy:" tname)
    (pp hierarchy)

    (with-current-buffer root-buffer
      (ab-init)
      (mapc (lambda (ci)
	      (let ((c (car ci))
		    (gcis (cdr ci)))
		(ab-new-buffer c)
		(with-current-buffer c
		  (mapc (lambda (gci)
			  (let ((gc (car gci))
				(ggcs (cadr gci)))
			    (ab-new-buffer gc)
			    (with-current-buffer gc
			      (mapc (lambda (ggc)
				      (ab-new-buffer ggc))
				    ggcs))))
			  gcis))))
	    hierarchy))
    (with-current-buffer root-buffer
      (should (and (message "%s(): Checking that the root buffer »%S« has no parent." tname (current-buffer))
		   (null ab-parent)))
      (should (and (message "%s(): Checking that the root buffer »%S« has exactly %d children..." tname (current-buffer) (length hierarchy))
		   (= (length hierarchy) (length ab-children))))
      (mapc (lambda (ci)
	      (let ((c (car ci)))
		(should (and (message "%s(): Checking that »%S« is one child buffer." tname c)
			     (member c ab-children)))))
	    hierarchy))
    (with-current-buffer root-buffer
      (mapc (lambda (ci)
	      (let ((c (car ci))
		    (gcis (cdr ci)))
		(with-current-buffer c
		  (should (and (message "%s(): Checking that »%S« is the parent of »%S«." tname root-buffer (current-buffer))
			       (eq root-buffer ab-parent)))
		  (should (and (message "%s(): Checking that the root of »%S« is »%s«." tname (current-buffer) root-buffer)
			       (eq root-buffer (ab-root-buffer))))
		  (should (and (message "%s(): Checking that »%S« has exactly %d children (actual is %d)." tname (current-buffer) (length gcis) (length ab-children))
			       (= (length gcis) (length ab-children))))
		  (mapc (lambda (gci)
			  (let ((gc (car gci))
				(ggcs (cadr gci)))
			    (should (and (message "%s(): Checking that »%S« is one of those children." tname gc)
					 (member gc ab-children)))
			    (with-current-buffer gc
			      (should (and (message "%s(): Checking that »%S« is the parent of »%S«." tname c (current-buffer))
					   (eq c ab-parent)))
			      (should (and (message "%s(): Checking that the root of »%S« is »%s«." tname (current-buffer) root-buffer)
					   (eq root-buffer (ab-root-buffer))))
			      (should (and (message "%s(): Checking that »%S« has exactly %d children (actual is %d)." tname (current-buffer) (length ggcs) (length ab-children))
					   (= (length ggcs) (length ab-children))))
			      (mapc (lambda (ggc)
				      (should (and (message "%s(): Checking that »%S« is one of those children." tname ggc)
						   (member ggc ab-children)))
				      (with-current-buffer ggc
					(should (and (message "%s(): Checking that »%S« is the parent of »%S«." tname gc (current-buffer))
						     (eq gc ab-parent)))
					(should (and (message "%s(): Checking that the root of »%S« is »%s«." tname (current-buffer) root-buffer)
						     (eq root-buffer (ab-root-buffer))))
					(should (and (message "%s(): Checking that »%S« has exactly 0 children." tname (current-buffer))
						     (= 0 (length ab-children))))))
				    ggcs))))
			gcis))))
	    hierarchy))

    (abt-clean-buffers root-buffer
		       child-buffer-0
		       child-buffer-1
		       child-buffer-2
		       child-buffer-3
		       child-buffer-4
		       grandchild-buffer-00
		       grandchild-buffer-01
		       grandchild-buffer-10
		       grandchild-buffer-11
		       grandchild-buffer-12
		       grandchild-buffer-20
		       grandchild-buffer-21
		       grandchild-buffer-22
		       grandchild-buffer-23
		       grandchild-buffer-30
		       grandchild-buffer-31
		       grandchild-buffer-32
		       greatgrandchild-buffer-000
		       greatgrandchild-buffer-001
		       greatgrandchild-buffer-002
		       greatgrandchild-buffer-100
		       greatgrandchild-buffer-101
		       greatgrandchild-buffer-110
		       greatgrandchild-buffer-200
		       greatgrandchild-buffer-201
		       greatgrandchild-buffer-202
		       greatgrandchild-buffer-210
		       greatgrandchild-buffer-211
		       greatgrandchild-buffer-230
		       greatgrandchild-buffer-231
		       greatgrandchild-buffer-232
		       greatgrandchild-buffer-233
		       greatgrandchild-buffer-300
		       greatgrandchild-buffer-320
		       greatgrandchild-buffer-321)))

(ert-deftest 011-ab-descendents ()
  "Test (ab-descendents) on the stock hierarchy of affiliations."
  (let ((tname "011-ab-descendents")
	(expected-descendents))
    (abt-stock-buffers)
    (setq expected-descendents (list abtsb-root
				     abtsb-child-0
				     abtsb-child-1
				     abtsb-child-2
				     abtsb-grandchild-00
				     abtsb-grandchild-01
				     abtsb-grandchild-20))

    (with-current-buffer abtsb-root
      (should (and (message "%s(): Checking that the root has exactly %d descendents. (%d vs %d)" tname (length expected-descendents) (length expected-descendents) (length (ab-descendents)))
		   (eq (length expected-descendents) (length (ab-descendents)))))
      (mapc (lambda (b)
	      (should (and (message "%s(): Checking that »%S« is a descendent of abtsb-root." tname b)
			   (member b (ab-descendents)))))
	    expected-descendents)
      (should-not (and (message "%s(): Checking that abtsb-other-buffer is not a descendent of abtsb-root." tname)
		     (member abtsb-other-buffer (ab-descendents)))))
    ;; (message "%s(): Would check that (ab-descendents) errors out in abtsb-other-buffer here." tname)
    (should-error (and (message "%s(): Checking that (ab-descendents) errors out in abtsb-other-buffer." tname)
		       (with-current-buffer abtsb-other-buffer (ab-descendents))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 012-ab-defvar-root ()
  "Test (ab-defvar) on the root of the stock hierarchy of affiliations."
  (let ((tname "012-ab-defvar-root"))
    (abt-stock-buffers)
    (defvar bob "other")
    (with-current-buffer abtsb-root
      (ab-defvar bob "carol")
      (should (and (message "%s(): Checking that the variable »bob« was set correctly in the current buffer %S." tname (current-buffer))
		   (string-equal "carol" bob)))
      (should (and (message "%s(): Checking that the variable »bob« was set correctly in the root buffer." tname)
		   (string-equal "carol" bob))))
    (mapc (lambda (b)
	    (with-current-buffer b
	      (should (and (message "%s(): Checking that »bob« has been set properly in buffer %S within the hierarchy of affiliations." tname (current-buffer))
			   (string-equal "carol" bob)))))
	  (ab-descendents abtsb-root))
    (should (and (message "%s(): Checking that setting »bob« in the hierarchy of affiliations does not affect other buffers" tname)
		 (string-equal "other" bob)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 013-ab-defvar-other ()
  "Test that (ab-defvar) does the right things in buffers other than the root buffer."
  (let ((tname "013-ab-defvar-other"))
    (abt-stock-buffers)
    (defvar bob "other" "Your name here.")
    (with-current-buffer abtsb-child-0
      (ab-defvar bob "carol" "We print anything.")
      (should (and (message "%s(): Checking that the variable »bob« was set correctly in »%S«." tname abtsb-child-0)
		   (string-equal "carol" bob))))
    (should (and (message "%s(): Checking that is was set right in the root." tname)
		 (string-equal "carol" (with-current-buffer abtsb-root bob))))
    (should-not (and (message "%s(): Checking that »bob« in other buffers has not been changed." tname)
		     (equal "carol" (with-current-buffer abtsb-other-buffer
							       bob))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 014-ab-setq-root ()
  "Test (ab-setq) on the stock hierarchy of affiliations.
Since (ab-setq) is really just an alias for (ab-defvar)
this isn't strictly needed.
However, since someday it might not be this test is here for completeness."
  (let ((tname "014-ab-setq"))
    (abt-stock-buffers)
    (with-current-buffer abtsb-other-buffer
      (defvar alpha -1 "alpha's docstring"))
    (with-current-buffer abtsb-root
      (ab-setq alpha 1))
    (should (and (message "%s(): Checking that the global value of alpha has not been changed." tname)
		 (= -1 alpha)))
    (should (and (message "%s(): Checking that the value has changed in the root buffer." tname)
		 (with-current-buffer abtsb-root
		   (= 1 alpha))))
    (setq alpha -2)
    (with-current-buffer abtsb-root
      (ab-setq alpha 2))
    (should (and (message "%s(): Checking that the global value of alpha is now -2." tname)
		 (= -2 alpha)))
    (should (and (message "%s(): Checking that the value has changed to 2 in the root buffer." tname)
		 (with-current-buffer abtsb-root
		   (= 2 alpha))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 015-ab-defvar-other ()
  "Test that (ab-defvar) does the right things in buffers other than the root buffer."
  (let ((tname "015-ab-defvar-other"))
    (abt-stock-buffers)
    (defvar bob "other")
    (with-current-buffer abtsb-child-0
      (ab-defvar bob "carol")
      (should (and (message "%s(): Checking that the variable »bob« was set correctly in the »%S«." tname abtsb-child-0)
		   (string-equal "carol" bob))))
    (should (and (message "%s(): Checking that is was set right in the root" tname)
		 (string-equal "carol" (with-current-buffer abtsb-root bob))))
    (should (and (message "%s(): Checking that »bob« in other buffers has not been changed." tname)
		     (string-equal "other" (with-current-buffer abtsb-other-buffer
					     bob))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 016-ab-setq-other ()
  "Test (ab-setq) in buffers other than the root buffer."
  (let ((tname "016-ab-setq-other")
	(descendents)
	(value 1))
    (abt-stock-buffers)
    (setq descendents (ab-descendents abtsb-root))
    (setq alpha -10)
    (mapc (lambda (b)
	    (with-current-buffer b
	      (ab-setq alpha value))
	    (mapc (lambda (bb)
		    (should (and (message "%s(): Checking that the global value of alpha is still -10." tname)
				 (= -10 alpha)))
		    (should (and (message "%s(): Checking that the new value %d has propagated throughout the affiliation." tname value)
				 (with-current-buffer bb (= value alpha)))))
		  descendents)
	    (setq value (1+ value)))
	  descendents)
    (abt-clean-up-stock-buffers)))

(ert-deftest 017-ab-defvar-protected-variables ()
  "Test that trying to set protected variables errors out."
  (let ((tname "017-ab-defvar-protected-variables"))
    (abt-stock-buffers)
    (mapc (lambda (b)
	    (with-current-buffer b
	      (mapc (lambda (v)
		      (should-error (and (message "%s(): Expecting an error trying to set »%S« in %S." tname v) b
					 (ab-defvar (eval v) 5))))
		    ab-protected-variables)))
	  (list abtsb-root abtsb-child-0))
    (abt-clean-up-stock-buffers)))

(ert-deftest 018-ab-defvar-registry ()
  "Test low level registration function."
  (let ((tname "018-ab-defvar-registry")
	(local-registry))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 1 "doc string")
      (should (and (message "%s(): Checking that (ab-defvar %S) really registers its arg." tname (symbol-name 'alpha))
		   (member 'alpha ab-variables-registry)))
      (ab-defvar beta 1 "doc string")
      (should (and (message "%s(): Checking that (ab-defvar %S) really registers its arg." tname (symbol-name 'beta))
		   (member 'beta ab-variables-registry)))
      (ab-defvar gamma 2 "doc string")
      (should (and (message "%s(): Checking that (ab-defvar %S) really registers its arg." tname (symbol-name 'gamma))
		   (member 'gamma ab-variables-registry)))

      (setq local-registry (copy-sequence ab-variables-registry))

      (ab-defvar alpha 3 "doc string")
      (should (and (message "%s(): Checking that a second call to (ab-defvar %S) doesn't change the registry." tname 'alpha)
		   (equal local-registry ab-variables-registry)))
      (ab-defvar beta 5 "doc string")
      (should (and (message "%s(): Checking that a second call to (ab-defvar %S) doesn't change the registry." tname 'betaa)
		   (equal local-registry ab-variables-registry)))
      (ab-defvar gamma 8 "doc string")
      (should (and (message "%s(): Checking that a second call to (ab-defvar %S) doesn't change the registry." tname 'gamma)
		   (equal local-registry ab-variables-registry))))
    (should (and (message "%s(): Checking that the variable registry is not set outside the affiliation." tname)
		 (with-current-buffer abtsb-other-buffer
		   (null ab-variables-registry))))
    (abt-clean-up-stock-buffers)
    (mapc #'makunbound (list 'alpha 'beta 'gamma))))

(ert-deftest 019-abt-ab-new-buffer-variables ()
  "Test that a new buffer inherits variables of the affiliation."
  (let ((tname "019-abt-ab-new-buffer-variables")
	(new-buffer)
	(variable-info (list (list 'alpha 1 "docstring 1")
			     (list 'beta 2 "docstring 2")
			     (list 'gamma 3 "docstring 3"))))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 1 "docstring 1")
      (ab-defvar beta 2 "docstring 2")
      (ab-defvar gamma 3 "docstring 3")
      (setq new-buffer (ab-new-buffer "new-buffer")))
    (with-current-buffer new-buffer
      (mapc (lambda (vi)
	      (let ((var (car vi))
		    (value (cadr vi))
		    (docstring (caddr vi)))
		(should (and (message "%s(): Checking that the new buffer has inherited the variable »%S«." tname var)
			     (local-variable-p var)))
		(should (and (message "%s(): Checking the value of »%S« (expect %d)" tname var value)
			     (equal value (eval var))))
		(should (and (message "%s(): Checking that the right docstring »%s« stuck." tname docstring)
			     (string-equal docstring (get var 'variable-documentation))))))
    	    variable-info))
    (abt-clean-up-stock-buffers)
    (kill-buffer new-buffer)
    (mapc #'makunbound (list 'alpha 'beta 'gamma))))

(ert-deftest 020-get-variables-small ()
  "Test that (ab-get-variables) returns exactly the right list of variables,
that they have exactly the right values
and that they haven't affected any global variables."
  (let ((tname "020-get-variables")
	(root-buffer-020 (get-buffer-create "root-buffer-020"))
	(child-buffer-020-0 (get-buffer-create "child-buffer-020-0"))
	(child-buffer-020-1 (get-buffer-create "child-buffer-020-1"))
	(expected-variables ())
	(var)
	(value))
    (defvar alpha -1)
    (defvar beta -2)
    (defvar gamma -3)
    (with-current-buffer root-buffer-020
      (ab-init))

    (with-current-buffer root-buffer-020
      (ab-defvar alpha 1 "docstring")
      (push (cons (make-local-variable 'alpha) 1) expected-variables)
      (should (and (message "%s(): Checking that the affiliation has exactly 1 variable." tname)
		   (equal 1 (length (ab-variables)))))
      (should (and (message "%s(): Checking that the variables contain »alpha«." tname)
		   (member 'alpha (ab-variables))))
      (should (and (message "%s(): Checking the value of »alpha«; it should be 1." tname)
		   (= 1 alpha)))

      (ab-new-buffer child-buffer-020-0))
    (message "%s(): FYI The global value of alpha is %d." tname alpha)
    (should (and (message "%s(): Checking the global value of alpha; it should be -1." tname)
		 (= -1 alpha)))

    (with-current-buffer child-buffer-020-0
      (ab-defvar beta 2 "docstring")
      (push (cons (make-local-variable 'beta) 1) expected-variables)
      (should (and (message "%s(): Checking that the affiliation has exactly 2 variables." tname)
		   (equal 2 (length (ab-variables)))))
      (should (and (message "%s(): Checking that the variables contain »alpha«." tname)
		   (member 'alpha (ab-variables))))
      (should (and (message "%s(): Checking the value of alpha; it should be 1." tname)
		   (= 1 alpha)))
      (should (and (message "%s(): Checking that the variables contain »beta«." tname)
		   (member 'beta (ab-variables))))
      (should (and (message "%s(): Checking the value of beta; it should be 2." tname)
		   (= 2 beta))))
    (should (and (message "%s(): Checking the global value of alpha; it should be -1." tname)
		 (= -1 alpha)))
    (should (and (message "%s(): Checking the global value of beta; it should be -2." tname)
		 (= -2 beta)))
    (with-current-buffer root-buffer-020
      (ab-new-buffer child-buffer-020-1))
    (with-current-buffer child-buffer-020-1
      (ab-defvar gamma 4 "docstring")
      (push (cons (make-local-variable 'gamma) 1) expected-variables)
      (should (and (message "%s(): Checking that the affiliation has exactly 3 variables." tname)
		   (equal 3 (length (ab-variables)))))
      (should (and (message "%s(): Checking that the variables contain »alpha«." tname)
		   (member 'alpha (ab-variables))))
      (should (and (message "%s(): Checking the value of alpha; it should be 1." tname)
		   (= 1 alpha)))
      (should (and (message "%s(): Checking that the variables contain »beta«." tname)
		   (member 'beta (ab-variables))))
      (should (and (message "%s(): Checking the value of beta; it should be 2." tname)
		   (= 2 beta)))
      (should (and (message "%s(): Checking that the variables contain »gamma«." tname)
		   (member 'gamma (ab-variables))))
      (should (and (message "%s(): Checking the value of gamma; it should be 4." tname)
		   (= 4 gamma))))
    (should (and (message "%s(): Checking the global value of alpha; it should be -1." tname)
		 (= -1 alpha)))
    (should (and (message "%s(): Checking the global value of beta; it should be -2." tname)
		 (= -2 beta)))
    (should (and (message "%s(): Checking the global value of gamma; it should be -3." tname)
		 (= -3 gamma)))

    (abt-clean-buffers root-buffer-020 child-buffer-020-0 child-buffer-020-1)
    (mapc #'makunbound (list 'alpha 'beta 'gamma))))

(ert-deftest 021-get-variables-1 ()
  "Test that (ab-variables) gets the right set of variables
for a random selection of members of the stock buffers.
N.B. This overlaps somewhat with the tests of (ab-defvar)."
  (let ((tname "021-get-variables-1")
	(stock-buffers)
	(expected-variables ())
	(var))
    (abt-stock-buffers)
    (setq stock-buffers (ab-descendents abtsb-root))
    (defvar alpha -1)
    (defvar beta -2)
    (defvar gamma -3)
    (defvar delta -4)
    (defvar epsilon -5)
    (with-current-buffer (abt-random-element stock-buffers)
      (should (and (message "%s(): expecting 0 variables in a brand new affiliation." tname)
		   (= 0 (length (ab-variables))))))
    (with-current-buffer (abt-random-element stock-buffers)
      (ab-defvar alpha 1)
      (push (cons 'alpha 1) expected-variables))
    (with-current-buffer (abt-random-element stock-buffers)
      (should (and (message "%s(): expecting 1 variable." tname)
		   (= 1 (length (ab-variables))))))
    (with-current-buffer (abt-random-element stock-buffers)
      (should (and (message "%s(): Checking that variable is alpha." tname)
		   (member (make-local-variable 'alpha) (ab-variables)))))
    (should (and (message "%s(): Checking that the global alpha is unchanged." tname)
		 (= alpha -1)))

    (with-current-buffer (abt-random-element stock-buffers)
      (ab-defvar beta 3)
      (push (cons 'beta 3) expected-variables))
    (with-current-buffer (abt-random-element stock-buffers)
      (should (and (message "%s(): expecting 2 variables." tname)
		   (= 2 (length (ab-variables))))))
    (mapc (lambda (vi)
	    (let ((var (car vi))
		  (value (cdr vi)))
	      (with-current-buffer (abt-random-element stock-buffers)
		(should (and (message "%s(): Checking that »%S« is there." tname var)
			     (member (make-local-variable var) (ab-variables)))))
	      (with-current-buffer (abt-random-element stock-buffers)
		(should (and (message "%s(): Checking that »%S« has the right value »%d«." tname var value)
			     (= value (eval var)))))))
	  expected-variables)
    (should (and (message "%s(): Checking that the global alpha is unchanged." tname)
		 (= alpha -1)))
    (should (and (message "%s(): Checking that the global beta is unchanged." tname)
		 (= beta -2)))
    (with-current-buffer (abt-random-element stock-buffers)
      (ab-defvar gamma 9)
      (push (cons 'gamma 9) expected-variables))
    (with-current-buffer (abt-random-element stock-buffers)
      (should (and (message "%s(): expecting 3 variable." tname)
		   (= 3 (length (ab-variables))))))
    (mapc (lambda (vi)
	    (let ((var (car vi))
		  (value (cdr vi)))
	      (with-current-buffer (abt-random-element stock-buffers)
		(should (and (message "%s(): Checking that »%S« is there." tname var)
			     (member (make-local-variable var) (ab-variables)))))
	      (with-current-buffer (abt-random-element stock-buffers)
		(should (and (message "%s(): Checking that »%S« has the right value »%d«." tname var value)
			     (= value (eval var)))))))
	  expected-variables)
	  ;; (list (cons 'alpha 1) (cons 'beta 3) (cons 'gamma 9)))
    (should (and (message "%s(): Checking that the global alpha is unchanged." tname)
		 (= alpha -1)))
    (should (and (message "%s(): Checking that the global beta is unchanged." tname)
		 (= beta -2)))
    (should (and (message "%s(): Checking that the global gamma is unchanged." tname)
		 (= gamma -3)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 022-get-variables-1-alt ()
  "Test that affiliation variables don't interfere with their global counter parts,
and that (ab-variables) returns exactly the affiliation variables."
  (let ((tname "022-get-variables-1-alt")
	(var)
	(value)
	(var-info-so-far ())
	(stock-buffers)
	(gvar-info (list (cons 'alpha   -1)
			 (cons 'beta    -2)
			 (cons 'gamma   -3)
			 (cons 'delta   -4)
			 (cons 'epsilon -5))))
    (abt-stock-buffers)
    (setq stock-buffers (ab-descendents abtsb-root))

    (setq var 'alpha)
    (setq value 1)
    (push (cons var value) var-info-so-far)
    (with-current-buffer (abt-random-element stock-buffers)
      (ab-defvar alpha value))
    (with-current-buffer (abt-random-element stock-buffers)
      (should (and (message "%s(): expecting %d variable." tname (length var-info-so-far))
		   (= (length var-info-so-far) (length (ab-variables))))))
    (mapc (lambda (gvi)
	    (let ((gvar (car gvi))
		  (gvalue (cdr gvi)))
	      (with-current-buffer (abt-random-element stock-buffers)
		(should (and (message "%s(): Checking that »%S« is there." tname var)
			     (member (make-local-variable var) (ab-variables)))))
	      (with-current-buffer (abt-random-element stock-buffers)
		(should (and (message "%s(): Checking that »%S« has the right value »%d«." tname var value)
			     (= value (eval var)))))))
	  var-info-so-far)
    (mapc (lambda (vi)
	    (let ((var (car vi))
		  (value (cdr vi)))
	      (should (and (message "%s(): Checking that the global »%S« is unchanged »%d«." tname var value)
			   (= value (eval var))))))
	  gvar-info)
    (list (cons 'alpha    1)
	  (cons 'beta     3)
	  (cons 'gamma    9)
	  (cons 'delta   27)
	  (cons 'epsilon 81))
    (abt-clean-up-stock-buffers)))

(ert-deftest 023-kill-buffer-key ()
  "Test that C-xk gets remapped by (ab-init) and (ab-new-buffer) to (ab-kill)
and that it does kill buffers.
There's no structural testing here; that's done in other tests."
  (let ((tname "023-kill-buffer-key"))
    (abt-stock-buffers)
    (mapc (lambda (b)
	    (with-current-buffer b
	      (should (and (message "%s(): Checking that C-xk is mapped to M-x ab-kill-buffer in %S." tname b)
			   (eq (key-binding "\C-xk") 'ab-kill-buffer)))))
	  (ab-descendents abtsb-root))
    (abt-clean-up-stock-buffers)))

(ert-deftest 024-kill-buffer-one-child-do-not-kill-descendents ()
  "Test exhaustively that in a hierarchy like this
	parent
	  │
	child
that killing the parent does not kill the child, but does remove affiliated variables
and that killing the child does the right thing to the parent."
   (let* ((tname "024-kill-buffer-one-child-do-not-kill-descendents")
	  (parent-024)
	  (child-024)
	  (set-up (lambda ()
		    (setq parent-024 (get-buffer-create "parent-024"))
		    (setq child-024  (get-buffer-create "child-024"))
		    (with-current-buffer parent-024
		      (ab-init)
		      (ab-new-buffer child-024)
		      (ab-defvar alpha 0)
		      (ab-defvar beta 1)))))
     (funcall set-up)

     (ab-kill-buffer nil parent-024)
     (should-not (and (message "%s(): Expecting that parent-024 is no longer live." tname)
		      (buffer-live-p parent-024)))
     (should (and (message "%s(): Expecting that child-024 is still alive." tname)
		  (buffer-live-p child-024)))
     (should-not (and (message "%s(): Expecting that the variable alpha no longer exists in child." tname)
		      (with-current-buffer child-024 (boundp 'alpha))))
     (should-error (and (message "%s(): Expecting that the variable beta no longer exists in child." tname)
			(with-current-buffer child-024 beta)))

     (abt-clean-buffers parent-024 child-024)
     (funcall set-up)

     (ab-kill-buffer nil child-024)
     (should-not (and (message "%s(): Expecting that child-024 is no longer live." tname)
		      (buffer-live-p child-024)))
     (should (and (message "%s(): Expecting that parent-024 is still alive." tname)
		  (buffer-live-p parent-024)))

     (abt-clean-buffers parent-024 child-024)))

(ert-deftest 025-kill-buffer-one-child-do-kill-descendents ()
  "Test exhaustively that in a hierarchy like this
	parent
	  │
	child
that killing the parent does kill the child,
and that killing the child does the right thing to the parent."
  (let* ((tname "025-kill-buffer-one-child-do-kill-descendents")
	 (parent-025)
	 (child-025)
	 (set-up (lambda ()
		   (setq parent-025 (get-buffer-create "parent-025"))
		   (setq child-025  (get-buffer-create "child-025"))
		   (with-current-buffer parent-025
		     (ab-init)
		     (ab-new-buffer child-025)
		     (ab-defvar alpha 0)
		     (ab-defvar beta 1)))))
    (funcall set-up)

    (ab-kill-buffer 'kill-descendents-too parent-025)
    (should-not (and (message "%s(): Expecting that parent-025 is no longer live." tname)
		     (buffer-live-p parent-025)))
    (should-not (and (message "%s(): Expecting that child-025 is no longer live either" tname)
		      (buffer-live-p parent-025)))

    (abt-clean-buffers parent-025 child-025)
    (funcall set-up)

    (ab-kill-buffer 'kill-descendents-too child-025)
    (should-not (and (message "%s(): Expecting that child-025 is no longer live." tname)
		     (buffer-live-p child-025)))
    (should (and (message "%s(): Expecting that parent-025 is still alive." tname)
		 (buffer-live-p parent-025)))

    (abt-clean-buffers parent-025 child-025)))

(ert-deftest 026-kill-buffer-one-child-one-grandchild-do-not-kill-descendents ()
  "Test exhaustively that (ab-kill-buffer nil) does the right things
in the following small hierarchy:
	parent
	  │
	child
          │
      grandchild"
  (let* ((tname "026-kill-buffer-one-child-one-grandchildren-do-not-kill-descendents")
	 (parent-026)
	 (child-026)
	 (grandchild-026)
	 (set-up (lambda ()
		   (setq parent-026 (get-buffer-create "parent-026"))
		   (setq child-026 (get-buffer-create "child-026"))
		   (setq grandchild-026 (get-buffer-create "grandchild-026"))
		   (with-current-buffer parent-026
		     (ab-init)
		     (ab-new-buffer child-026)
		     (with-current-buffer parent-026
		       (ab-defvar alpha 1)
		       (ab-defvar beta 2)))
		   (with-current-buffer child-026
		     (ab-new-buffer grandchild-026)))))
    (funcall set-up)
    (mapc #'makunbound (list 'alpha 'beta))

    (ab-kill-buffer nil parent-026)
    (should-not (and (message "%s(): (36) Expecting that parent-026 is no longer live." tname)
		     (buffer-live-p parent-026)))
    (mapc (lambda (b)
	    (should (and (message "%s(): (39) Expecting that %S is still alive." tname b)
			 (buffer-live-p child-026)))
	    (mapc (lambda (v)
		    (if (boundp v)
			(should-not (and (message "%s(): (46) %S is bound; check that it's not buffer-local.e" tname)
					 (local-variable-p v)))
		      (should-not (and (message "%s(): (48) Expecting that the variable %S no longer exists in %S." tname v b)
				       (with-current-buffer b (boundp v))))))
		  (list 'alpha 'beta)))
	  (list child-026 grandchild-026))

    (abt-clean-buffers parent-026 child-026 grandchild-026)
    (funcall set-up)

    (ab-kill-buffer nil child-026)
    (should-not (and (message "%s(): (57) Expecting that child-026 is no longer live." tname)
		     (buffer-live-p child-026)))
    (mapc (lambda (b)
	    (should (and (message "%s(): (60) Expecting that %S is still live." tname b)
			 (buffer-live-p b))))
	  (list parent-026 grandchild-026))
    (mapc (lambda (b)
	    (mapc (lambda (v)
		    (should-not (and (message "%s(): (65) Expecting that the variable %S no longer exists in %S." tname v b)
				     (local-variable-exists-p b v))))
		  (list 'alpha 'beta)))
	  (list grandchild-026))
    (mapc (lambda (b)
	    (mapc (lambda (v)
		    (with-current-buffer b
		      (should (and (message "%s(): (71) Expecting that the variable %s still exists in %S." tname v b)
				     (local-variable-p v)))))
		  (list 'alpha 'beta)))
	  (list parent-026))

    (abt-clean-buffers parent-026 child-026 grandchild-026)
    (funcall set-up)

    (ab-kill-buffer nil grandchild-026)
    (should-not (and (message "%s(): (81) Expecting that grandchild-026 is no longer live." tname)
		     (buffer-live-p grandchild-026)))
    (mapc (lambda (b)
	    (should (and (message "%s(): (84) Expecting that %S is still live." tname b)
			 (buffer-live-p b))))
	  (list parent-026 child-026))
    (mapc (lambda (b)
	    (mapc (lambda (v)
		    (should (and (message "%s(): (89) Expecting that the variable %s still exists in %S." tname v b)
				 (with-current-buffer b
				   (boundp v)))))
		  (list 'alpha 'beta)))
	  (list parent-026 child-026))

    (abt-clean-buffers parent-026 child-026 grandchild-026)))

(ert-deftest 027-kill-buffer-one-child-one-grandchild-do-kill-descendents ()
  "Test exhaustively that (ab-kill-buffer 'do-kill-descendents) does the right things
in the following small hierarchy:
	parent
	  │
	child
          │
      grandchild"
  (let* ((tname "027-kill-buffer-one-child-one-grandchildren-do-not-kill-descendents")
	 (parent-027)
	 (child-027)
	 (grandchild-027)
	 (set-up (lambda ()
		   (setq parent-027 (get-buffer-create "parent-027"))
		   (setq child-027 (get-buffer-create "child-027"))
		   (setq grandchild-027 (get-buffer-create "grandchild-027"))
		   (with-current-buffer parent-027
		     (ab-init)
		     (ab-new-buffer child-027))
		   (with-current-buffer child-027
		     (ab-defvar alpha 3)
		     (ab-defvar beta 4)
		     (ab-new-buffer grandchild-027)))))
    (funcall set-up)

    (ab-kill-buffer 'do-kill-descendents parent-027)
    (mapc (lambda (b)
	    (should-not (and (message "%s(): (28) Expecting that parent-027 is no longer live." tname)
			     (buffer-live-p parent-027))))
	  (list parent-027 child-027 grandchild-027))

    (abt-clean-buffers parent-027 child-027 grandchild-027)
    (funcall set-up)

    (ab-kill-buffer 'do-kill-descendents child-027)
    (mapc (lambda (b)
	    (should (and (message "%s(): (37) Expecting that »%S« is still live." tname b)
			 (buffer-live-p b))))
	  (list parent-027))
    (mapc (lambda (b)
	    (should-not (and (message "%s(): (41) Expecting that %S is no longer live." tname b)
			     (buffer-live-p b))))
	  (list child-027 grandchild-027))
    (mapc (lambda (b)
	    (should (and (message "%s(): (45) Expecting that %S is still live." tname b)
			 (buffer-live-p b)))
	    (mapc (lambda (v)
		    (should (and (message "%s(): (48) Expecting that the variable %s still exists in %S." tname v b)
				 (with-current-buffer b (local-variable-p v)))))
		  (list 'alpha 'beta)))
	  (list parent-027))

    (abt-clean-buffers parent-027 child-027 grandchild-027)
    (funcall set-up)

    (ab-kill-buffer 'do-kill-descendents grandchild-027)
    (should-not (and (message "%s(): (57) Expecting that grandchild-027 is no longer live." tname)
		     (buffer-live-p grandchild-027)))
    (mapc (lambda (b)
	    (should (and (message "%s(): (60) Expecting that %S is still live." tname b)
			 (buffer-live-p b)))
	    (mapc (lambda (v)
		    (should (and (message "%s(): (63) Expecting that the variable %s still exists in %S." tname v b)
				 (with-current-buffer b
				   (boundp v)))))
		  (list 'alpha 'beta)))
	  (list parent-027 child-027))

    (abt-clean-buffers parent-027 child-027 grandchild-027)))


(ert-deftest 028-kill-buffer-stock-buffers-do-kill-descdents ()
  "Test that (ab-kill) does the right thing on the root of the stock buffers
and on a random selection of other members of the stock buffers."
  (let* ((tname "028-kill-buffer-stock-buffers-do-kill-descdents")
	 (all-buffers)
	 (these-buffers)
	 (those-buffers)
	 (set-up (lambda ()
		   (abt-stock-buffers)
		   (setq all-buffers (ab-descendents abtsb-root))
		   (setq these-buffers (copy-sequence all-buffers))
		   (setq those-buffers (copy-sequence all-buffers))
		   (with-current-buffer (abt-random-element all-buffers)
		     (ab-defvar alpha 5)
		     (ab-defvar beta 6)))))
    ;; abtsb-root
    ;;	├─── abtsb-child-0
    ;;	│      ├─── abtsb-grandchild-00
    ;;	│      └─── abtsb-grandchild-01
    ;;	├─── abtsb-child-1
    ;;	└─── abtsb-child-2
    ;;	       └─── abtsb-grandchild-20
    (funcall set-up)

    (mapc (lambda (b)
	    (ab-kill-buffer 'do-kill-descendents b)
	    (should-not (and (message "%s(): Checking that %S has been killed." tname b)
			     (buffer-live-p b))))
	  (list abtsb-root))
    (mapc (lambda (b)
	    (should-not (and (message "%s(): Checking that %S is no longer there." tname b)
			     (buffer-live-p b))))
	  (delete abtsb-root these-buffers))

    (abt-clean-up-stock-buffers)
    (funcall set-up)

    (mapc (lambda (b)
	    (ab-kill-buffer 'do-kill-descendents b))
	  (list abtsb-child-0))
    (mapc (lambda (b)
	    (should-not (and (message "%s(): Checking that %S has been killed." tname b)
			 (buffer-live-p b))))
	  (setq those-buffers (list abtsb-child-0 abtsb-grandchild-00 abtsb-grandchild-01)))
    (mapcar (lambda (b)
	      (setq these-buffers (delete b these-buffers)))
	    those-buffers)
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is still there." tname b)
			 (buffer-live-p b))))
	  these-buffers)

    (abt-clean-up-stock-buffers)
    (funcall set-up)

    (mapc (lambda (b)
	    (ab-kill-buffer 'do-kill-descendents b)
	    (should-not (and (message "%s(): Checking that %S has been killed." tname b)
			     (buffer-live-p b))))
	  (setq those-buffers (list abtsb-grandchild-20)))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is still there." tname b)
			 (buffer-live-p b)))
	    (mapc (lambda (v)
		    (should (and (message "%s(): Checking that %S is still defined in %S." tname v b)
				 (with-current-buffer b (boundp v)))))
		  (list 'alpha 'beta)))
	  (setq these-buffers (delete abtsb-grandchild-20 these-buffers)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 029-kill-buffer-stock-buffers-do-not-kill-descdents ()
  "Test that (ab-kill) does the right thing on the root of the stock buffers
and on a random selection of other members of the stock buffers."
  (let* ((tname "029-kill-buffer-stock-buffers-do-not-kill-descdents")
	 (all-buffers)
	 (these-buffers)
	 (those-buffers)
	 (set-up (lambda ()
		   (abt-stock-buffers)
		   (setq all-buffers (ab-descendents abtsb-root))
		   (setq these-buffers (copy-sequence all-buffers))
		   (setq those-buffers (copy-sequence all-buffers))
		   (with-current-buffer (abt-random-element all-buffers)
		     (ab-defvar alpha 7)
		     (ab-defvar beta 8)))))
    ;; abtsb-root
    ;;  ├─── abtsb-child-0
    ;;  │      ├─── abtsb-grandchild-00
    ;;  │      └─── abtsb-grandchild-01
    ;;  ├─── abtsb-child-1
    ;;  └─── abtsb-child-2
    ;;         └─── abtsb-grandchild-20
    (funcall set-up)

    (mapc (lambda (b)
	    (ab-kill-buffer nil b)
	    (should-not (and (message "%s(): Checking that %S has been killed." tname b)
			     (buffer-live-p b))))
	  (list abtsb-root))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is still there." tname b)
			 (buffer-live-p b)))
	    (mapc (lambda (v)
		    (should-not (and (message "%s(): Checking that %S is no longer defined in %S." tname v b)
				     (local-variable-exists-p b v))))
		  (list 'alpha 'beta)))
	  (delete abtsb-root these-buffers))

    (abt-clean-up-stock-buffers)
    (funcall set-up)

    (mapc (lambda (b)
	    (ab-kill-buffer nil b)
	    (should-not (and (message "%s(): Checking that %S has been killed." tname b)
			     (buffer-live-p b))))
	  (list abtsb-child-0))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is still there." tname b)
			 (buffer-live-p b)))
	    (mapc (lambda (v)
		    (should (and (message "%s(): Checking that %S is still defined in %S." tname v b)
				     (local-variable-exists-p b v))))
		  (list 'alpha 'beta)))
	  (list abtsb-root
		abtsb-child-1
		abtsb-child-2
		abtsb-grandchild-20))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is still there." tname b)
			 (buffer-live-p b)))
	    (mapc (lambda (v)
		    (should-not (and (message "%s(): Checking that %S is no longer defined in %S." tname v b)
				     (local-variable-exists-p b v))))
		  (list 'alpha 'beta)))
	  (list abtsb-grandchild-00
		abtsb-grandchild-01))
    (mapc (lambda (b)
	    (setq these-buffers (delete b these-buffers)))
	  those-buffers)
    (setq these-buffers (delete abtsb-child-0 these-buffers))
    (mapc (lambda (b)
	    (mapc (lambda (v)
		    (should (and (message "%s(): Checking that %S is still defined in %S." tname v b)
				 (local-variable-exists-p b v))))
		  (list 'alpha 'beta)))
	  these-buffers)

    (abt-clean-up-stock-buffers)
    (funcall set-up)

    (mapc (lambda (b)
	    (ab-kill-buffer nil b)
	    (should-not (and (message "%s(): Checking that %S has been killed." tname b)
			     (buffer-live-p b))))
	  (list abtsb-grandchild-20))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is still there." tname b)
			 (buffer-live-p b)))
	    (mapc (lambda (v)
		    (should (and (message "%s(): Checking that %S is still defined in %S." tname v b)
				 (local-variable-exists-p b v))))
		  (list 'alpha 'beta)))
	  (delete abtsb-grandchild-20 these-buffers))
    (abt-clean-up-stock-buffers)))

(ert-deftest 030-trivial-get-leaves ()
  "Test that (ab-leaves) in a trivial hierarchy returns a list
containing exactly the root buffer."
  (let ((tname "030-trivial-get-leaves")
	(abt-root-030 (get-buffer-create "abt-root-030"))
	(proposed-leaves))
    (with-current-buffer abt-root-030
      (ab-init))
    (should (and (message "%s(): Expecting exactly 1 leaf." tname)
		 (= 1 (length (ab-leaves abt-root-030)))))
    (setq proposed-leaves (with-current-buffer abt-root-030 (ab-leaves)))
    (should (and (message "%s(): Checking that abt-root-030 is in the returned leaves »%S«." tname proposed-leaves)
		 (member abt-root-030 proposed-leaves)))
  (abt-clean-buffers abt-root-030)))

(ert-deftest 031-one-child-get-leaves ()
  "Test that (ab-leaves) returns a list containing exactly the child
in the following hierarchy:
	parent
	  │
	child
"
  (let ((tname "031-one-child-get-leaves")
	(parent-031 (get-buffer-create "parent-031"))
	(child-031 (get-buffer-create "child-031"))
	(proposed-leaves))
    (with-current-buffer parent-031
      (ab-init)
      (ab-new-buffer child-031)
      (setq proposed-leaves (ab-leaves)))
    (should (and (message "%s(): Expecting one leaf." tname)
		 (= 1 (length proposed-leaves))))
    (should (and (message "%s(): Checking that it is child-031." tname)
		 (member child-031 proposed-leaves)))
    (abt-clean-buffers parent-031 child-031)))

(ert-deftest 032-stock-buffers-get-leaves ()
  "Test that (ab-leaves) returns exactly the leaf buffers
from randomly selected chosen buffers in the stock buffer hierarchy."
 (let ((tname "032-stock-buffers-get-leaves")
       (leaves)
       (proposed-leaves))
   (abt-stock-buffers)
	;; abtsb-root
	;;  ├─── abtsb-child-0
	;;  │      ├─── abtsb-grandchild-00
	;;  │      └─── abtsb-grandchild-01
	;;  ├─── abtsb-child-1
	;;  └─── abtsb-child-2
	;;         └─── abtsb-grandchild-20
   (setq leaves (list abtsb-child-1 abtsb-grandchild-00 abtsb-grandchild-01 abtsb-grandchild-20))
   (setq proposed-leaves (with-current-buffer abtsb-root (ab-leaves)))
   (should (and (message "%s(): Checking that there are exactly %d leaves." tname (length leaves))
		(= (length leaves) (length proposed-leaves))))
   (mapc (lambda (b)
	   (should (and (message "%s(): Checking that %S is one of them." tname b)
			(member b proposed-leaves))))
	 leaves)
   (abt-clean-up-stock-buffers)))

(ert-deftest 033-get-modified-buffers ()
  "For 5 configurations of the stock buffers confirm that (ab-modified-buffers)
returns exactly the list of modified buffers.
One configuration no modified buffers; another has all buffers modified;
the remaining 3 configurations are randomly arranged.
The node used for calling (ab-modified-buffers) is chosen at random."
  (let ((tname "033-get-modified-buffers")
	(modified-buffers)
	(proposed-modified-buffers)
	(buffer-to-modify)
	(modified-buffers ())
	(i 1)
	(max 6)
	(buffers-left))
    (abt-stock-buffers)
    (should (and (message "%s(): Checking that there are no modified stock buffers." tname)
		 (= 0 (length (with-current-buffer abtsb-root (ab-modified-buffers))))))

    (mapc (lambda (b)
	    (with-current-buffer b
	      (set-buffer-modified-p t)))
	  (ab-descendents abtsb-root))
    (mapc (lambda (b)
	    (should (and (message "%s():    %S" tname b)
			 (buffer-modified-p b))))
	  (ab-descendents abtsb-root))

    (abt-clean-up-stock-buffers)

    (abt-stock-buffers)
    (setq buffers-left (ab-descendents abtsb-root))
    (while (< i max)
      (setq buffer-to-modify (abt-random-element buffers-left))
      (setq buffers-left (delete buffer-to-modify buffers-left))
      (push buffer-to-modify modified-buffers)
      (with-current-buffer buffer-to-modify
	(set-buffer-modified-p 'modify))
      (should (and (message "%s(): Checking that there are now %d modified buffers." tname i)
		   (= i (length (with-current-buffer abtsb-root (ab-modified-buffers))))))
      (mapc (lambda (b)
	      (should (and (message "%s(): Checking that %S is modified." tname b)
			   (buffer-modified-p b))))
	    (with-current-buffer abtsb-root (ab-modified-buffers)))
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 034-get-file-buffers ()
  "Test that (ab-files) returns exactly the list of buffers with files.
Test this with 5 configurations again:
one with no files; one with only files;
the remaining 3 chosen at random.
The buffers containing files contain existing files (from this source directory)
and ficticious files.
The buffers containing the files are added at randomly selected nodes
in the stock buffer hierarchy."
  (let* ((tname "034-get-file-buffers")
	 (root-034 (find-file-noselect "root-034"))
	 (child-034-0 (get-buffer-create "child-034-0"))
	 (child-034-1 (find-file-noselect "child-034-1"))
	 (child-034-2 (get-buffer-create "child-034-2"))
	 (child-034-3 (find-file-noselect "child-034-3"))
	 (file-buffers (list root-034 child-034-0 child-034-1 child-034-2 child-034-3))
	 (proposed-file-buffers ()))
    (with-current-buffer root-034
      (ab-init)
      (mapc #'ab-new-buffer
	    (list child-034-0 child-034-1 child-034-2 child-034-3))
      (setq proposed-file-buffers (ab-files)))
    (should (and (message "%s(): Checking that (ab-files) finds exactly %d buffers." tname (length file-buffers))
		 (= (length file-buffers) (length proposed-file-buffers))))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is in the proposed file buffers." tname b)
			 (member b proposed-file-buffers))))
	  file-buffers)
    (abt-clean-buffers root-034 child-034-0 child-034-1 child-034-2 child-034-3)))

(ert-deftest 035-get-modified-file-buffers ()
  "Confirm that (ab-modified-files) returns the right list."
  (let ((tname "035-get-modified-file-buffers")
	(file-buffers)
	(modified-file-buffers)
	(proposed-file-buffers))
    (abt-stock-some-file-buffers)
    ;; abtsb-root*
    ;; ├─── abtsb-child-0
    ;; │      ├─── abtsb-grandchild-00**
    ;; │      └─── abtsb-grandchild-01
    ;; ├─── abtsb-child-1**
    ;; └─── abtsb-child-2
    ;;        └─── abtsb-grandchild-20*
    (setq file-buffers (list abtsb-root abtsb-grandchild-00 abtsb-child-1 abtsb-grandchild-20))
    (mapc (lambda (b)
	    (with-current-buffer b
	      (set-buffer-modified-p 'modified)))
	  (setq modified-file-buffers (list abtsb-grandchild-20 abtsb-child-1)))
    (setq proposed-file-buffers (with-current-buffer abtsb-root (ab-modified-files)))
    (should (and (message "%s(): Checking that there are %d modified file buffers." tname (length modified-file-buffers))
		 (= (length modified-file-buffers) (length proposed-file-buffers))))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is a modified file buffer" tname b)
			 (member b proposed-file-buffers))))
	  modified-file-buffers)
    (abt-clean-up-stock-buffers)))

(ert-deftest 036-trivial-root-p ()
  "Test that retrieving the root directory from an affiliation
with only a root buffer."
  (let ((tname "036-trivial-root-p")
	(root-036 (get-buffer-create "root-036")))
    (with-current-buffer root-036
      (ab-init))
    (should (and (message "%s(): Checking that (ab-root-buffer) gets the right root buffer in an affiliation with only one buffer." tname)
		 (eq root-036 (ab-root-buffer root-036))))
    (abt-clean-buffers root-036)))

(ert-deftest 037-one-child-root-p ()
  "Test that retrieving the root buffer from the following hierarchy works correctly
in both buffers.
	parent
	  │
	child
"
  (let ((tname "037-one-child-root-p")
	(parent-037 (get-buffer-create "parent-037"))
	(child-037  (get-buffer-create "child-037")))
    (with-current-buffer parent-037
      (ab-init)
      (ab-new-buffer child-037)
    (should (and (message "%s(): Checking that (ab-root-buffer) finds the root in the root." tname)
		 (eq parent-037 (with-current-buffer parent-037 (ab-root-buffer))))))
    (should (and (message "%s(): Checking that (ab-root-buffer) finds the root in the child." tname)
		 (eq parent-037 (with-current-buffer child-037 (ab-root-buffer)))))
    (abt-clean-buffers parent-037 child-037)))

(ert-deftest 038-stock-buffers-root-p ()
  "Test that (ab-root-buffer) works from random selections
of the stock buffers."
  (let ((tname "038-stock-buffers-root-p")
	(this-buffer)
	(buffers)
	(i 0)
	(max 5))
    (abt-stock-some-file-buffers)
    (setq buffers (ab-descendents abtsb-root))
    (while (< i max)
      (setq this-buffer (nth (random (length buffers)) buffers))
      (should (and (message "%s(): Checking that (ab-root-buffer) gets the right buffer in %S." tname this-buffer)
		   (eq abtsb-root (ab-root-buffer this-buffer))))
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 039-trivial-leaf-p ()
  "Test that the only buffer in a trivial buffer is also a leaf."
  (let ((tname "039-trivial-leaf-p")
	(root-039 (get-buffer-create "root-039")))
    (with-current-buffer root-039
      (ab-init))
    (should (and (message "%s(): Checking that the root is a leaf in a trivial affiliation." tname)
		 (equal root-039 (ab-root-buffer root-039))))
    (abt-clean-buffers root-039)))

(ert-deftest 040-one-child-leaf-p ()
  "Test that (ab-child-p) works correctly on both
buffers in the folloing hierarchy:
	parent
	  │
	child
"
  (let ((tname "040-one-child-leaf-p")
	(parent-040 (get-buffer-create "parent-040"))
	(child-040 (get-buffer-create "child-040")))
    (with-current-buffer parent-040
      (ab-init)
      (ab-new-buffer child-040))
    (should (and (message "%s(): Checking that parent-040 is not a leaf." tname)
		 (null (ab-leaf-p parent-040))))
    (should (and (message "%s(): Checking that child-040 is a leaf." tname)
		 (ab-leaf-p child-040)))
    (abt-clean-buffers parent-040 child-040)))

(ert-deftest 041-stock-buffers-leaf-p ()
  "Test that (ab-child-leaf-p) works correctly
at randomly chosen buffers in the stock buffer hierarchy."
  (let ((tname "041-stock-buffers-leaf-p")
	(leaves)
	(proposed-leaves)
	(buffers)
	(this-buffer)
	(i 0)
	(max 10))
    (abt-stock-buffers)
    (setq buffers (ab-descendents abtsb-root))
    (setq proposed-leaves (ab-leaves abtsb-root))
    ;; abtsb-root
    ;;  ├─── abtsb-child-0
    ;;  │      ├─── abtsb-grandchild-00
    ;;  │      └─── abtsb-grandchild-01
    ;;  ├─── abtsb-child-1
    ;;  └─── abtsb-child-2
    ;;         └─── abtsb-grandchild-20
    (setq leaves (list abtsb-grandchild-00
		       abtsb-grandchild-01
		       abtsb-child-1
		       abtsb-grandchild-20))
    (while (< i max)
      (setq this-buffer (abt-random-element buffers))
      (if (member this-buffer leaves)
	  (should (and (message "%s(): Checking that %S is a leaf." tname this-buffer)
		       (member this-buffer proposed-leaves)))
	(should-not (and (message "%s(): Checking that %S is not a leaf." tname this-buffer)
			 (member this-buffer proposed-leaves))))
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 042-trivial-affiliate-p ()
  "Test that the root buffer is an affiliate of itself in the trivial hierarchy."
    (let ((tname "042-trivial-affiliate-p")
	  (root-042 (get-buffer-create "root-042")))
      (with-current-buffer root-042
	(ab-init)
	(should (and (message "%s(): Checking that root-042 is an affiliate of itself." tname)
		     (ab-affiliate-p root-042))))
      (abt-clean-buffers root-042)))

(ert-deftest 043-one-child-affiliate-p ()
  "Test exhaustively that all 4 pairs of buffers in the following hierarchy
are affiliates.
	parent
	  │
	child
"
  (let ((tname "043-one-child-affiliate-p")
	(parent-043 (get-buffer-create "root-043"))
	(child-043  (get-buffer-create "child-043")))
    (with-current-buffer parent-043
      (ab-init)
      (ab-new-buffer child-043))
    (mapc (lambda (b0)
	    (mapc (lambda (b1)
		    (should (and (message "%s(): Checking that %S and %S are affiliated." tname b0 b1)
				 (with-current-buffer b0
				   (ab-affiliate-p b1)))))
		  (list parent-043 child-043)))
	  (list parent-043 child-043))
    (abt-clean-buffers parent-043 child-043)))

(ert-deftest 044-two-children-affiliate-p ()
  "Test that all 6 pairs of buffers  in the following hierarchy
are affiliates.
	parent
	  ├── child-0
	  └── child-1
"
  (let* ((tname "044-two-children-affiliate-p")
	 (parent-044 (get-buffer-create "root-044"))
	 (child-044-0  (get-buffer-create "child-044-0"))
	 (child-044-1  (get-buffer-create "child-044-1"))
	 (buffers (list parent-044 child-044-0 child-044-1)))
    (with-current-buffer parent-044
      (ab-init)
      (ab-new-buffer child-044-0)
      (ab-new-buffer child-044-1))
    (mapc (lambda (b0)
	    (mapc (lambda (b1)
		    (should (and (message "%s(): Checking that %S and %S are affiliated." tname b0 b1)
				 (with-current-buffer b0
				   (ab-affiliate-p b1)))))
		  (list parent-044 child-044-0 child-044-1)))
	  (list parent-044 child-044-0 child-044-1))
    (abt-clean-buffers parent-044 child-044-0 child-044-1)))

(ert-deftest 045-random-pairs-affiliates-p ()
  "Test that 5 randomly chosen pairs from the stock hierarchy are affiliates."
  (let ((tname "045-random-pairs-affiliates-p")
	(all-buffers)
	(this-buffer)
	(that-buffer)
	(i 0)
	(max 5))
    (abt-stock-buffers)
    (setq all-buffers (ab-descendents abtsb-root))
    (while (< i max)
      (setq this-buffer (abt-random-element all-buffers))
      (setq that-buffer (abt-random-element all-buffers))
      (should (and (message "%s(): Checking that %S and %S are affiliated." tname this-buffer that-buffer)
		   (with-current-buffer this-buffer (ab-affiliate-p that-buffer))))
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 046-ab-flatten ()
  "Test (ab-flatten)."
  (let* ((tname "046-ab-flatten")
	 (datum '((0 1) 2 (3 4 5 (6 7)) (8 9 (10 11 (12 13 (14 15))))))
	 (result (ab-flatten datum)))
    (should (and (message "%s(): Checking that the flattened list has length 16." tname)
		 (= 16 (length result))))
    (mapc (lambda (n)
	    (should (and (message "%s(): Checking that %d is a member of the flattened list." tname n)
			 (member n result))))
	  (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))

(ert-deftest 047-ab-mapcar-at-root-of-stock-buffers ()
  "Test that (ab-mapcar) really sweeps the entire affiliation
and returns a value reflecting its structure and
the return values of the function in the map."
  (let ((tname "047-ab-mapcar-at-root-of-stock-buffers")
	(results)
	(subresults)
	(subsubresults))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable.")
      (setq results (ab-mapcar #'abt-bump)))

    (pp results)
    (should (and (message "%s(): Checking a map of integer increments implicitly over all the stock buffers." tname)
		 results))

    (with-current-buffer abtsb-root
      (ab-setq alpha 100))
    (setq results (ab-mapcar (lambda (b)
			      (with-current-buffer b
				(let ((value (1+ alpha)))
				  (ab-setq alpha value))))
			    abtsb-root))
    (pp results)
    (should (and (message "%s(): Checking that the length of the results is 5." tname)
		 (= 5 (length results))))
    (should (and (message "%s(): Checking that the first element of the results is abtsb-root." tname)
		 (eq abtsb-root (car results))))
    (should (and (message "%s(): Checking that the value for the abtsb-root is 101." tname)
		 (= 101 (cadr results))))
    (setq subresults (cdr results))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is in the subresults." tname b)
			 (assoc b subresults)))
	    (setq subsubresults (cddr (assoc b subresults)))
	    (cond ((eq b abtsb-child-0)
		   (should (and (message "%s(): Checking that abtsb-child-0 has exactly 2 child results." tname)
				(= 2 (length subsubresults))))
		   (mapc (lambda (bb)
			   (should (and (message "%s(): Checking that »%S« is a child of %S." tname bb b)
					(assoc bb subsubresults))))
			 (list abtsb-grandchild-00 abtsb-grandchild-01)))
		  ((eq b abtsb-child-1)
		   (should (and (message "%s(): Checking that %S  has no child results." tname b)
				(null subsubresults))))
		  ((eq b abtsb-child-2)
		   (should (and (message "%s(): Checking that %S has exactly 1 child results." tname b)
				(= 1 (length subsubresults))))
		   (should (and (message "%s(): Checking that that child result is for abtsb-grandchild-20." tname)
				(assoc abtsb-grandchild-20 subsubresults))))
		  (t
		   (should (and (message "%s(): Checking a bad condition for a descendent of abtsb-root »%s«." tname b)
				(error "%s(): Unknown buffer »%S«." tname b))))))
	  (list abtsb-child-0 abtsb-child-1 abtsb-child-2))

    (setq subresults (ab-flatten results))
    (message "%s(): Except for the root of the map, the order of mapping is unknown." tname)
    (message "%s(): The root case was checked above. Here are the others." tname)
    (let ((i 102))
      (while (<= i (+ 100 (/ (length subresults) 2)))
	(should (and (message "%s(): Checking that %d is one of the results." tname i)
		     (member i subresults)))
	(setq i (1+ i))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 048-ab-mapcar-at-child-0-of-stock-buffers ()
  "Test mapping over abtsb-child-0."
  (let ((tname "048-ab-mapcar-at-child-0-of-stock-buffers")
	(results))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable."))

    (setq results (ab-mapcar #'abt-bump abtsb-child-0))
    (pp results)
    (should (and (message "%s(): Checking that the list of results has exactly 4 elements." tname)
		 (= 4 (length results))))
    (should (and (message "%s(): Checking that the results start at abtsb-child-0." tname)
		 (eq abtsb-child-0 (car results))))
    (should (and (message "%s(): Checking that the result for abtsb-child-0 is 1." tname)
		 (= 1 (cadr results))))
    (message "%s(): FYI (cddr results) = %S." tname (cddr results))
    (should (and (message "%s(): Checking that it has exactly two [grand]child results" tname)
		 (= 2 (length (cddr results)))))

    (setq subresults (cdr results))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is in the results." tname b)
			 (assoc b subresults))))
	  (list abtsb-grandchild-00 abtsb-grandchild-01))

    (setq results (ab-flatten results))
    (should (and (message "%s(): Checking that there are exactly 3 results." tname)
		 (= 3 (/ (length results) 2))))
    (mapc (lambda (n)
	    (should (and (message "%s(): Checking that n is among the results." tname n)
			 (member n results))))
	  (list 2 3))))

(ert-deftest 049-ab-mapcar-at-child-1-of-stock-buffers ()
  "Test mapping over abtsb-child-0."
  (let ((tname "049-ab-mapcar-at-child-1-of-stock-buffers")
	(results))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable."))

    (setq results (ab-mapcar #'abt-bump abtsb-child-1))
    (pp results)
    (should (and (message "%s(): Checking that the length of the results is 2." tname)
		 (= 2 (length results))))
    (should (and (message "%s(): Checking that there are no [grand]child results." tname)
		 (null (cddr results))))))

(ert-deftest 050-ab-mapcar-at-child-2-of-stock-buffers ()
  "Test mapping over abtsb-child-0."
  (let ((tname "050-ab-mapcar-at-child-2-of-stock-buffers")
	(subresults))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable."))

    (setq results (ab-mapcar #'abt-bump abtsb-child-2))
    (pp results)
    (should (and (message "%s(): Checking the abtsb-child-2 is at the top of the results." tname)
		 (eq abtsb-child-2 (car results))))
    (should (and (message "%s(): Checking that the result for abtsb-child-2 is 1." tname)
		 (= 1 (cadr results))))
    (should (and (message "%s(): Checking that the length of the results is 3." tname)
		 (= 3 (length results))))

    (setq subresults (cdr results))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that »%S« is among the result." tname b)
			 (assoc b subresults))))
	  (list abtsb-grandchild-20))))

(ert-deftest 051-ab-mapcar-at-grandchild-01-of-stock-buffers ()
  "Test mapping over abtsb-grandchild-01."
  (let ((tname "051-ab-mapcar-at-grandchild-01-of-stock-buffers")
	(results))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable."))

    (setq results (ab-mapcar #'abt-bump abtsb-grandchild-01))
    (should (and (message "%s(): Checking that abtsb-grandchild-01 is at the top of the results." tname)
		 (eq abtsb-grandchild-01 (car results))))
    (should (and (message "%s(): Checking that there are the right number of results." tname)
		 (= 2 (length results))))
    (should (and (message "%s(): Checking that the result for abtsb-grandchild-01 is 1." tname)
		 (= 1 (cadr results))))))

(ert-deftest 052-ab-mapcar-at-grandchild-20-of-stock-buffers ()
  "Test mapping over abtsb-grandchild-02."
  (let ((tname "052-ab-mapcar-at-grandchild-20-of-stock-buffers")
	(results))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable."))

    (setq results (ab-mapcar #'abt-bump abtsb-grandchild-20))
    (pp results)
    (should (and (message "%s(): Checking that abtsb-grandchild-20 is at the top of the results." tname)
		 (eq abtsb-grandchild-20 (car results))))
    (should (and (message "%s(): Checking that there are the right number of results." tname)
		 (= 2 (length results))))
    (should (and (message "%s(): Checking that the result for abtsb-grandchild-20 is 1." tname)
		 (= 1 (cadr results))))))


(ert-deftest 053-ab-mapc-at-root ()
  "Test that after mapping at the root of stock buffers alpha is 6."
  (let ((tname "053-ab-mapc-at-root"))
    (abt-stock-buffers)
    (message "%s():     5." tname)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable."))
    (message "%s():     8." tname)
    (ab-mapc #'abt-bump abtsb-root)
    (message "%s():     10." tname)
    (message "%s(): FYI alpha is %d." tname (with-current-buffer abtsb-root alpha))
    (should (and (message "%s(): Checking that alpha is 7." tname)
		 (= 7 (with-current-buffer abtsb-root alpha))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 054-ab-mapc-at-child-0 ()
  "Test that after mapping at child 0 of the root of stock buffers alpha is 2."
  (let ((tname "054-ab-mapc-at-child-0"))
    (abt-stock-buffers)
    (with-current-buffer abtsb-child-0
      (ab-defvar alpha 0 "A variable."))
    (ab-mapc #'abt-bump abtsb-child-0)
    (should (and (message "%s(): Checking that after mapping over abtsb-child-0 alpha is 3." tname)
		 (message "%s():     It's %d." tname (with-current-buffer abtsb-child-0 alpha))
		 (= 3 (with-current-buffer abtsb-root alpha))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 055-ab-mapc-at-child-1 ()
  "Test that after mapping at child 1 of the root of stock buffers alpha is 0."
  (let ((tname "055-ab-mapc-at-child-1"))
    (abt-stock-buffers)
    (with-current-buffer abtsb-child-1
      (ab-defvar alpha 0 "A variable.")
      (ab-mapc #'abt-bump))
    (should (and (message "%s(): Checking that after mapping over abtsb-child-0 alpha is 1." tname)
		 (= 1 (with-current-buffer abtsb-root alpha))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 056-ab-mapc-at-grandchild-00 ()
  "Test that after mapping at grandchild 0 of the root of stock buffers alpha is 0."
  (let ((tname "056-ab-mapc-at-grandchild-00"))
    (abt-stock-buffers)
    (with-current-buffer abtsb-grandchild-00
      (ab-defvar alpha 0 "A variable.")
      (ab-mapc #'abt-bump abtsb-grandchild-00))
    (should (and (message "%s(): Checking that after mapping over abtsb-grandchild-00 alpha is 1." tname)
		 (= 1 (with-current-buffer abtsb-root alpha))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 057-ab-sort-buffers ()
  "Test that sorting buffers works if all buffers in the current affiliation are file buffers."
  (let ((tname "057-ab-sort-buffers")
	(i 0)
	(buffer))
    (while (< i 5)
      (abt-stock-buffers)
      (setq buffer (abt-random-element (ab-descendents abtsb-root)))
      (with-current-buffer buffer
	(ab-sort-buffers))
      (should (and (message "%s(): Checking that M-x ab-sort-buffers sorts at implicitly %S when all buffers are file buffers." tname buffer)
		   (abt-sorted-p (with-current-buffer buffer ab-children))))
      (setq i (1+ i))
      (abt-clean-up-stock-buffers))

    (while (< i 5)
      (abt-stock-some-file-buffers 'all)
      (setq buffer (abt-random-element (ab-descendents abtsb-root)))
      (ab-sort-buffers buffer)
      (should (and (message "%s(): Checking that M-x ab-sort-buffers sorts at explicitly %S when all buffers are file buffers." tname buffer)
		   (abt-sorted-p (with-current-buffer buffer ab-children))))
      (setq i (1+ i))
      (abt-clean-up-stock-buffers))))

(ert-deftest 058-ab-result-for-buffer ()
  "Test that (ab-result-for-buffer) returns the right value."
  (let ((tname "058-ab-result-for-buffer")
	(results)
	(result)
	(i 0)
	(buffers)
	(buffer))
    (abt-stock-buffers)
    (setq buffers (ab-descendents abtsb-root))
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable."))
    (setq results (ab-mapcar #'abt-bump abtsb-root))
    (pp results)
    (while (< i 5)
      (setq buffer (abt-random-element buffers))
      (setq result (ab-result-for-buffer buffer results))
      (should (and (message "%s(): Checking that the result for %S is an natural number." tname result)
		   (natnump result)))
      (should (and (message "%s(): Checking that that result is <= 7." tname)
		   (<= result 7)))
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 059-ab-mapc-root ()
  "Test that (ab-mapc) affects the entire affiliation hierarchy."
  (let ((tname "059-ab-mapc-root")
	(i 0)
	(buffer))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable called alpha."))
    (ab-mapc #'abt-bump abtsb-root)
    (while (< i 5)
      (setq buffer (abt-random-element (ab-descendents abtsb-root)))
      (should (and (message "%s(): Checking that alpha has value 7 with an explicit buffer." tname)
		   (= 7 (with-current-buffer buffer alpha))))
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)

    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A variable called alpha.")
      (ab-mapc #'abt-bump))
    (while (< i 5)
      (setq buffer (abt-random-element (ab-descendents abtsb-root)))
      (should (and (message "%s(): Checking that alpha has value 7 with an implicit buffer." tname)
		   (= 7 (with-current-buffer buffer alpha))))
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 060-ab-mapc-other-implicit ()
  "Test that (ab-mapc) affects only parts of the affiliation hierarchy."
  (let ((tname "060-ab-mapc-other-implicit"))
    (abt-stock-buffers)
    (with-current-buffer abtsb-child-0
      (ab-defvar alpha 0 "A docstring.")
      (ab-mapc #'abt-bump)
      (should (and (message "%s(): Checking that alpha has been bumped to 3." tname)
		   (= 3 alpha))))
    (with-current-buffer abtsb-child-1
      (ab-mapc #'abt-bump)
      (should (and (message "%s(): Checking that alpha has been bumped to 4." tname)
		   (= 4 alpha))))
    (with-current-buffer abtsb-child-2
      (ab-mapc #'abt-bump)
      (should (and (message "%s(): Checking that alpha has been bumped to 6." tname)
		   (= 6 alpha))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 061-ab-mapc-other-explicit ()
  "Test that (ab-mapc) works when the buffer is explicit."
  (let ((tname "061-ab-mapc-other-explicit"))
    (abt-stock-buffers)
    (with-current-buffer abtsb-child-0
      (ab-defvar alpha 0 "A docstring."))
    (ab-mapc #'abt-bump abtsb-child-0)
    (with-current-buffer abtsb-child-0
      (should (and (message "%s(): Checking that alpha has been bumped to 3." tname)
		   (= 3 alpha))))
    (ab-mapc #'abt-bump abtsb-child-1)
    (with-current-buffer abtsb-child-1
      (should (and (message "%s(): Checking that alpha has been bumped to 4." tname)
		   (= 4 alpha))))
    (ab-mapc #'abt-bump abtsb-child-2)
    (with-current-buffer abtsb-child-2
      (should (and (message "%s(): Checking that alpha has been bumped to 6." tname)
		   (= 6 alpha))))
    (abt-clean-up-stock-buffers)))

(ert-deftest 062-ab-mapc-root-return ()
  "Test that (ab-mapc) returns the right value when mapping over the entire affiliation hierarchy."
  (let ((tname "062-ab-mapc-root-return")
	(ret)
	(ri))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A docstring.")
      (pp
       (setq ret (ab-mapc #'abt-bump))))
    (should (and (message "%s(): Checking that the return value has length 4." tname)
		 (= 4 (length ret))))
    (should (and (message "%s(): Checking that the car of the return value is abtsb-root." tname)
		 (eq abtsb-root (car ret))))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is an index into (cdr ret)." tname b)
			 (setq ri (assoc b (cdr ret)))))
	    (cond ((eq b abtsb-child-0)
		   (should (and (message "%s(): Checking that the length of %S is 3." tname ri)
				(= 3 (length ri))))
		   (mapc (lambda (bb)
			   (should (and (message "%s(): Checking that %S is an index into (cdr ri)." tname bb)
					(assoc bb (cdr ri)))))
			 (list abtsb-grandchild-00 abtsb-grandchild-01)))
		  ((eq b abtsb-child-1)
		   (should (and (message "%s(): Checking that the length of %S is 1." tname ri)
				(= 1 (length ri)))))
		  ((eq b abtsb-child-2)
		   (should (and (message "%s(): Checking that the length of %S is 2." tname ri)
				(= 2 (length ri))))
		   (should (and (message "%s(): Checking that %S is an index into (cdr %S)." tname abtsb-grandchild-20 ri)
				(assoc abtsb-grandchild-20 (cdr ri)))))
		  (t
		   (should (and (message "%s(): Impossible condition." tname)
				())))))
	  (list abtsb-child-0 abtsb-child-1 abtsb-child-2))
    (abt-clean-up-stock-buffers)))

(ert-deftest 063-ab-mapc-other-return ()
  "Test that (ab-mapc) returns the right value when mapping over only parts of the affiliation hierarchy."
  (let ((tname "063-ab-mapc-other-return")
	(ret))
    (abt-stock-buffers)
    (with-current-buffer abtsb-root
      (ab-defvar alpha 0 "A docstring."))

    (setq ret (ab-mapc #'abt-bump abtsb-child-0))
    (pp ret)
    (should (and (message "%s(): Checking that alpha is now 3." tname)
		 (with-current-buffer abtsb-root (= 3 alpha))))
    (should (and (message "%s(): Checking the length of %S is 3." tname ret)
		 (= 3 (length ret))))
    (should (and (message "%s(): Checking that the car is %S." tname abtsb-child-0)
		 (eq abtsb-child-0 (car ret))))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is an index of the cdr." tname b)
			 (assoc b (cdr ret)))))
	  (list abtsb-grandchild-00 abtsb-grandchild-01))

    (setq ret (ab-mapc #'abt-bump abtsb-child-1))
    (pp ret)
    (should (and (message "%s(): Checking that alpha is now 4." tname)
		 (with-current-buffer abtsb-root (= 4 alpha))))
    (should (and (message "%s(): Checking the length of %S is 1." tname ret)
		 (= 1 (length ret))))
    (should (and (message "%s(): Checking that the car is %S." tname abtsb-child-1)
		 (eq abtsb-child-1 (car ret))))

    (setq ret (ab-mapc #'abt-bump abtsb-child-2))
    (pp ret)
    (should (and (message "%s(): Checking that alpha is now 6." tname)
		 (with-current-buffer abtsb-root (= 6 alpha))))
    (should (and (message "%s(): Checking the length of %S is 2." tname ret)
		 (= 2 (length ret))))
    (should (and (message "%s(): Checking that the car is %S." tname abtsb-child-2)
		 (eq abtsb-child-2 (car ret))))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is an index of the cdr." tname b)
			 (assoc b (cdr ret)))))
	  (list abtsb-grandchild-20))
    (abt-clean-up-stock-buffers)))

(ert-deftest 064-ab-next-sibling ()
  "Test that ab-next-sibling really returns the next sibling
if there is one."
  (let ((tname "064-ab-next-sibling"))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (mapc (lambda (bi)
	    (let ((buffer (car bi))
		  (expected-sibling (cdr bi)))
	      (should (and (message "%s(): Checking that the next sibling of %S is %S." tname buffer expected-sibling)
			   (eq expected-sibling (ab-next-sibling buffer))))))
	  (list (cons abtsb-root nil)
		(cons abtsb-child-0 abtsb-child-1)
		(cons abtsb-child-1 abtsb-child-2)
		(cons abtsb-child-2 nil)
		(cons abtsb-grandchild-00 abtsb-grandchild-01)
		(cons abtsb-grandchild-01 nil)
		(cons abtsb-grandchild-20 nil)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 065-ab-switch-to-root-buffer ()
  "Test that switching to the root buffer really does that."
  (let ((tname "065-ab-switch-to-root-buffer")
	(msg))
    (abt-stock-buffers)
    (mapc (lambda (b)
	    (switch-to-buffer b)
	    (should (and (message "%s(): Checking that switching to %S worked." tname b)
		 (eq b (current-buffer))))
	    (ab-switch-to-root-buffer)
	    (should (and (message "%s(): Checking that switching to root worked." tname)
			 (eq abtsb-root (current-buffer)))))
	  (ab-descendents abtsb-root))

    (switch-to-buffer abtsb-other-buffer)
    (should (and (message "%s(): Checking that switching to another buffer worked." tname)
		 (eq abtsb-other-buffer (current-buffer))))
    (setq msg (ab-switch-to-root-buffer))
    (should (and (message "%s(): Checking trying to switch to the root buffer" tname)
		 (message "%s():     from another buffer has no effect." tname)
		 (eq abtsb-other-buffer (current-buffer))))
    (should (and (message "%s(): Checking that M-x ab-switch-to-root-buffer returned the right message." tname)
		 ;; MAINTENANCE This must match the message M-x ab-switch-to-root-buffer
		 (string-equal msg "ab-switch-to-root-buffer(): You're not in an affiliation.")))
    (abt-clean-up-stock-buffers)))

(ert-deftest 066-ab-last-child ()
  "Test that (ab-last-child) returns the last child both implicitly and explicitly."
  (let ((tname "066-ab-last-child")
	(buffer-child-map))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq buffer-child-map (list (cons abtsb-root abtsb-child-2)
				 (cons abtsb-child-0 abtsb-grandchild-01)
				 (cons abtsb-child-1 nil)
				 (cons abtsb-child-2 abtsb-grandchild-20)
				 (cons abtsb-grandchild-00 nil)
				 (cons abtsb-grandchild-01 nil)
				 (cons abtsb-grandchild-20 nil)))
    (mapc (lambda (bci)
	    (let ((buffer (car bci))
		  (expected-last-child (cdr bci)))
	      (should (and (message "%s(): Checking that the last child of %S explicitly is %S." tname buffer expected-last-child)
			   (eq expected-last-child (ab-last-child buffer))))
	      (should (and (message "%s(): Checking that the last child of %S implicitly is %S." tname buffer expected-last-child)
			   (eq expected-last-child (with-current-buffer buffer
						     (ab-last-child)))))))
	  buffer-child-map)
    (abt-clean-up-stock-buffers)))

(ert-deftest 067-ab-switch-to-last-buffer ()
  "Test that switching to the last buffer really does that."
  (let ((tname "067-ab-switch-to-last-buffer")
	(last-buffer))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq last-buffer abtsb-grandchild-20)
    (mapc (lambda (b)
	    (switch-to-buffer b)
	    (should (and (message "%s(): Checking that switching to %S worked." tname b)
			 (eq b (current-buffer))))
	    (ab-switch-to-last-buffer)
	    (should (and (message "%s(): Checking that switching to the last buffer worked." tname)
			 (eq last-buffer (current-buffer)))))
	  (ab-descendents abtsb-root))

    (switch-to-buffer abtsb-other-buffer)
    (should (and (message "%s(): Checking that switching to another buffer worked." tname)
		 (eq abtsb-other-buffer (current-buffer))))
    (setq msg (ab-switch-to-last-buffer))
    (should (and (message "%s(): Checking that trying to switch to the last buffer" tname)
		 (message "%s():     from another buffer has no effect." tname)
		 (eq abtsb-other-buffer (current-buffer))))
    (should (and (message "%s(): Checking that M-x ab-switch-to-last-buffer returned the right message." tname)
		 ;; MAINTENANCE This must match the message M-x ab-switch-to-last-buffer
		 (string-equal "ab-switch-to-last-buffer(): You're not in an affiliation." msg)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 068-ab-view-descendents ()
  "Test that DESCRIBE THE TEST HERE."
  ;; HEREHERE This is cruft, just here to help visualize the data.
  (let ((tname "068-ab-view-descendents"))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (pp (ab-descendents abtsb-root))
    (ab-next-buffer 1 abtsb-root)
    (abt-clean-up-stock-buffers)))

(ert-deftest 069-ab-next-buffer-1 ()
  "Test that (ab-next-buffer) can traverse all the buffers
of the current affiliation one at a time."
  (let ((tname "069-ab-next-buffer-1")
	(buffers)
	(buffer-order)
	(buffer-info)
	(this-buffer)
	(expected-next-buffer)
	(proposed-next-buffer)
	(i 0)
	(max 14))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq buffers (ab-descendents abtsb-root))
    (setq buffer-order (list
			(cons abtsb-root	  abtsb-child-0)
			(cons abtsb-child-0	  abtsb-grandchild-00)
			(cons abtsb-grandchild-00 abtsb-grandchild-01)
			(cons abtsb-grandchild-01 abtsb-child-1)
			(cons abtsb-child-1	  abtsb-child-2)
			(cons abtsb-child-2	  abtsb-grandchild-20)
			(cons abtsb-grandchild-20 abtsb-root)))
    (setq this-buffer (abt-random-element buffers))
    (message "%s(): FYI Starting at %S." tname this-buffer)
    (while (< i max)
      (setq buffer-info (assoc this-buffer buffer-order))
      (setq expected-next-buffer (cdr buffer-info))
      (set-buffer this-buffer)
      (setq proposed-next-buffer (ab-next-buffer 1 this-buffer))
      (should (and (message "%s(): Checking that %S is the same as %S." tname expected-next-buffer proposed-next-buffer)
		 (eq expected-next-buffer proposed-next-buffer)))
      (setq this-buffer proposed-next-buffer)
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 070-ab-next-buffer-5 ()
  "Test that (ab-next-buffer) can traverse all the buffers
of the current affiliation 5 at a time."
  (let ((tname "070-ab-next-buffer-5")
	(buffers)
	(buffer-order)
	(buffer-info)
	(this-buffer)
	(expected-next-buffer)
	(proposed-next-buffer)
	(i 0)
	(max 14))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq buffers (ab-descendents abtsb-root))
    (setq buffer-order (list
			(cons abtsb-root	  abtsb-child-2)
			(cons abtsb-child-0	  abtsb-grandchild-20)
			(cons abtsb-grandchild-00 abtsb-root)
			(cons abtsb-grandchild-01 abtsb-child-0)
			(cons abtsb-child-1	  abtsb-grandchild-00)
			(cons abtsb-child-2	  abtsb-grandchild-01)
			(cons abtsb-grandchild-20 abtsb-child-1)))
    (setq this-buffer (abt-random-element buffers))
    (message "%s(): FYI Starting at %S." tname this-buffer)
    (while (< i max)
      (setq buffer-info (assoc this-buffer buffer-order))
      (setq expected-next-buffer (cdr buffer-info))
      (set-buffer this-buffer)
      (setq proposed-next-buffer (ab-next-buffer 5 this-buffer))
      (should (and (message "%s(): Checking that %S is the same as %S." tname expected-next-buffer proposed-next-buffer)
		 (eq expected-next-buffer proposed-next-buffer)))
      (setq this-buffer proposed-next-buffer)
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 071-ab-switch-to-next-buffer-1 ()
  "Test that (ab-switch-to-next-buffer) can traverse all the buffers
of the current affiliation one at a time."
  (let ((tname "071-ab-switch-to-next-buffer-1")
	(buffers)
	(buffer-order)
	(buffer-info)
	(this-buffer)
	(expected-next-buffer)
	(proposed-next-buffer)
	(i 0)
	(max 14))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq buffers (ab-descendents abtsb-root))
    (setq buffer-order (list
			(cons abtsb-root	  abtsb-child-0)
			(cons abtsb-child-0	  abtsb-grandchild-00)
			(cons abtsb-grandchild-00 abtsb-grandchild-01)
			(cons abtsb-grandchild-01 abtsb-child-1)
			(cons abtsb-child-1	  abtsb-child-2)
			(cons abtsb-child-2	  abtsb-grandchild-20)
			(cons abtsb-grandchild-20 abtsb-root)))
    (setq this-buffer (abt-random-element buffers))
    (message "%s(): FYI Starting at %S." tname this-buffer)
    (while (< i max)
      (setq buffer-info (assoc this-buffer buffer-order))
      (setq expected-next-buffer (cdr buffer-info))
      (switch-to-buffer this-buffer)
      (setq proposed-next-buffer (ab-next-buffer 1 this-buffer))
      (should (and (message "%s(): Checking that %S is the same as %S." tname expected-next-buffer proposed-next-buffer)
		 (eq expected-next-buffer proposed-next-buffer)))
      (setq this-buffer proposed-next-buffer)
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 072-ab-switch-to-next-buffer-3 ()
  "Test that (ab-next-buffer) can traverse all the buffers
of the current affiliation 3 at a time."
  (let ((tname "072-ab-switch-to-next-buffer-3")
	(buffers)
	(buffer-order)
	(buffer-info)
	(this-buffer)
	(expected-next-buffer)
	(proposed-next-buffer)
	(i 0)
	(max 14))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq buffers (ab-descendents abtsb-root))
    (setq buffer-order (list
			(cons abtsb-root	  abtsb-grandchild-01)
			(cons abtsb-child-0	  abtsb-child-1)
			(cons abtsb-grandchild-00 abtsb-child-2)
			(cons abtsb-grandchild-01 abtsb-grandchild-20)
			(cons abtsb-child-1	  abtsb-root)
			(cons abtsb-child-2	  abtsb-child-0)
			(cons abtsb-grandchild-20 abtsb-grandchild-00)))
    (setq this-buffer (abt-random-element buffers))
    (message "%s(): FYI Starting at %S." tname this-buffer)
    (while (< i max)
      (setq buffer-info (assoc this-buffer buffer-order))
      (setq expected-next-buffer (cdr buffer-info))
      (switch-to-buffer this-buffer)
      (setq proposed-next-buffer (ab-next-buffer 3))
      (should (and (message "%s(): Checking that %S is the same as %S." tname expected-next-buffer proposed-next-buffer)
		 (eq expected-next-buffer proposed-next-buffer)))
      (setq this-buffer proposed-next-buffer)
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 073-ab-previous-buffer-1 ()
  "Test that ab-previous-buffer can traverse
the entire affiliation one buffer at a time."
  (let ((tname "073-ab-previous-buffer-1")
	(buffers)
	(buffer-order)
	(buffer-info)
	(this-buffer)
	(expected-previous-buffer)
	(proposed-previous-buffer)
	(i 0)
	(max 14))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq buffers (ab-descendents abtsb-root))
    (setq buffer-order (list
			(cons abtsb-root	  abtsb-grandchild-20)
			(cons abtsb-child-0	  abtsb-root)
			(cons abtsb-grandchild-00 abtsb-child-0)
			(cons abtsb-grandchild-01 abtsb-grandchild-00)
			(cons abtsb-child-1	  abtsb-grandchild-01)
			(cons abtsb-child-2	  abtsb-child-1)
			(cons abtsb-grandchild-20 abtsb-child-2)))
    (setq this-buffer (abt-random-element buffers))
    (message "%s(): FYI Starting at %S." tname this-buffer)
    (while (< i max)
      (setq buffer-info (assoc this-buffer buffer-order))
      (setq expected-previous-buffer (cdr buffer-info))
      (set-buffer this-buffer)
      (setq proposed-previous-buffer (ab-previous-buffer 1 this-buffer))
      (should (and (message "%s(): Checking that %S is the same as %S." tname expected-previous-buffer proposed-previous-buffer)
		 (eq expected-previous-buffer proposed-previous-buffer)))
      (setq this-buffer proposed-previous-buffer)
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 074-ab-previous-buffer-5 ()
  "Test that ab-previous-buffer can traverse
the entire affiliation 5 buffers at a time."
  (let ((tname "074-ab-previous-buffer-5")
	(buffers)
	(buffer-order)
	(buffer-info)
	(this-buffer)
	(expected-previous-buffer)
	(proposed-previous-buffer)
	(i 0)
	(max 14))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq buffers (ab-descendents abtsb-root))
    (setq buffer-order (list
			(cons abtsb-root	  abtsb-grandchild-00)
			(cons abtsb-child-0	  abtsb-grandchild-01)
			(cons abtsb-grandchild-00 abtsb-child-1)
			(cons abtsb-grandchild-01 abtsb-child-2)
			(cons abtsb-child-1	  abtsb-grandchild-20)
			(cons abtsb-child-2	  abtsb-root)
			(cons abtsb-grandchild-20 abtsb-child-0)))
    (setq this-buffer (abt-random-element buffers))
    (message "%s(): FYI Starting at %S." tname this-buffer)
    (while (< i max)
      (setq buffer-info (assoc this-buffer buffer-order))
      (setq expected-previous-buffer (cdr buffer-info))
      (set-buffer this-buffer)
      (setq proposed-previous-buffer (ab-previous-buffer 5 this-buffer))
      (should (and (message "%s(): Checking that %S is the same as %S." tname expected-previous-buffer proposed-previous-buffer)
		 (eq expected-previous-buffer proposed-previous-buffer)))
      (setq this-buffer proposed-previous-buffer)
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 075-ab-switch-to-previous-buffer-1 ()
  "Test that ab-previous-buffer can traverse
the entire affiliation one buffer at a time."
  (let ((tname "075-ab-switch-to-previous-buffer-1")
	(buffers)
	(buffer-order)
	(buffer-info)
	(this-buffer)
	(expected-previous-buffer)
	(proposed-previous-buffer)
	(i 0)
	(max 14))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq buffers (ab-descendents abtsb-root))
    (setq buffer-order (list
			(cons abtsb-root	  abtsb-grandchild-20)
			(cons abtsb-child-0	  abtsb-root)
			(cons abtsb-grandchild-00 abtsb-child-0)
			(cons abtsb-grandchild-01 abtsb-grandchild-00)
			(cons abtsb-child-1	  abtsb-grandchild-01)
			(cons abtsb-child-2	  abtsb-child-1)
			(cons abtsb-grandchild-20 abtsb-child-2)))
    (setq this-buffer (abt-random-element buffers))
    (message "%s(): FYI Starting at %S." tname this-buffer)
    (while (< i max)
      (setq buffer-info (assoc this-buffer buffer-order))
      (setq expected-previous-buffer (cdr buffer-info))
      (switch-to-buffer this-buffer)
      (setq proposed-previous-buffer (ab-previous-buffer 1 this-buffer))
      (should (and (message "%s(): Checking that %S is the same as %S." tname expected-previous-buffer proposed-previous-buffer)
		 (eq expected-previous-buffer proposed-previous-buffer)))
      (setq this-buffer proposed-previous-buffer)
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 076-ab-switch-to-previous-buffer-3 ()
  "Test that ab-previous-buffer can traverse
the entire affiliation 3 buffers at a time."
  (let ((tname "076-ab-switch-to-previous-buffer-3")
	(buffers)
	(buffer-order)
	(buffer-info)
	(this-buffer)
	(expected-previous-buffer)
	(proposed-previous-buffer)
	(i 0)
	(max 14))
    (abt-stock-buffers)
    (ab-sort-buffers abtsb-root)
    (setq buffers (ab-descendents abtsb-root))
    (setq buffer-order (list
			(cons abtsb-root	  abtsb-child-1)
			(cons abtsb-child-0	  abtsb-child-2)
			(cons abtsb-grandchild-00 abtsb-grandchild-20)
			(cons abtsb-grandchild-01 abtsb-root)
			(cons abtsb-child-1	  abtsb-child-0)
			(cons abtsb-child-2	  abtsb-grandchild-00)
			(cons abtsb-grandchild-20 abtsb-grandchild-01)))
    (setq this-buffer (abt-random-element buffers))
    (message "%s(): FYI Starting at %S." tname this-buffer)
    (while (< i max)
      (setq buffer-info (assoc this-buffer buffer-order))
      (setq expected-previous-buffer (cdr buffer-info))
      (switch-to-buffer this-buffer)
      (setq proposed-previous-buffer (ab-previous-buffer 3 this-buffer))
      (should (and (message "%s(): Checking that %S is the same as %S." tname expected-previous-buffer proposed-previous-buffer)
		 (eq expected-previous-buffer proposed-previous-buffer)))
      (setq this-buffer proposed-previous-buffer)
      (setq i (1+ i)))
    (abt-clean-up-stock-buffers)))

(ert-deftest 077-ab-disconnect-trivial ()
  "Test that, in an affiliation with only a root node, disconnecting the root node
destroys the affiliation, but leaves the buffer there."
  (let ((tname "077-ab-disconnect-trivial")
	(root)
	)
    ;; (error "077-ab-disconnect-trivial is not yet implemented." tname)
    (with-current-buffer (setq root (get-buffer-create "root-077"))
      (ab-init)
      (should (and (message "%s(): Checking that %S is indeed in an affiliation." tname root)
		   (ab-buffer-in-affiliation-p root)))
      (ab-defvar alpha 1 "Docstring for alpha.")
      (ab-defvar beta 2 "Docstring for beta.")
      (ab-defvar gamma 4 "Docstring for gamma.")
      (mapc (lambda (v)
	      (should (and (message "%s(): Checking that %S is indeed a buffer-local variable." tname v)
			   (and (boundp v)
				(local-variable-p v)))))
	    (list 'alpha 'beta 'gamma))
      (message "%s(): Disconnecting." tname)
      (ab-disconnect)
      (should-not (and (message "%s(): Checking that %S is no longer in an affiliation." tname root)
		       (ab-buffer-in-affiliation-p root)))
      (should (and (message "%s(): Checking that root is still alive." tname)
		   (buffer-live-p root)))
      (should (and (message "%s(): Checking that C-xk is mapped to M-x kill-buffer again.." tname)
		   (eq 'kill-buffer (key-binding "\C-xk"))))
      (mapc (lambda (v)
	      (should-not (and (message "%s(): Checking that %S is indeed a buffer-local variable." tname v)
			       (local-variable-exists-p root v))))
	    (list 'alpha 'beta 'gamma)))
    (abt-clean-buffers root)
    ))

(ert-deftest 078-ab-disconnect-parent-child-disconnect-parent ()
  "Test that disconnecting the parent in an affiliation with only parent and child
\(1) destroys the affiliation and
\(2) leaves both buffers alive."
  (let ((tname "078-ab-disconnect-parent-child")
	(parent (get-buffer-create "078-parent"))
	(child (get-buffer-create "078-child"))
	)
    ;; (error "078-ab-disconnect-parent-child is not yet implemented." tname)
    (with-current-buffer parent
      (ab-init)
      (ab-defvar alpha 1 "Docstring for alpha.")
      (ab-defvar beta 3 "Docstring for beta.")
      (ab-defvar gamma 9 "Docstring for gamma.")
      (ab-new-buffer child))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is in an affiliation." tname b)
			 (ab-buffer-in-affiliation-p b)))
	    (mapc (lambda (v)
		    (should (and (message "%s(): Checking that %S is indeed a buffer-local variable." tname v)
				 (local-variable-exists-p b v))))
		  (list 'alpha 'beta 'gamma)))
	  (list parent child))
    (message "%s(): Disconnecting the parent." tname)
    (ab-disconnect parent)
    (mapc (lambda (b)
	    (should-not (and (message "%s(): Checking that %S is no longer in an affiliation." tname b)
			     (ab-buffer-in-affiliation-p b)))
	    (should (and (message "%s(): Checking that C-xk is mapped to M-x kill-buffer again.." tname)
			 (eq 'kill-buffer (key-binding "\C-xk"))))
	    (mapc (lambda (v)
		    (should-not (and (message "%s(): Checking that %S is no longer a buffer-local variable." tname v)
				     (and (boundp v)
					  (local-variable-p v)))))
		  (list 'alpha 'beta 'gamma)))
	  (list parent child))

    ))

(ert-deftest 079-ab-disconnect-parent-child-disconnect-child ()
  "Test that disconnecting the child in an affiliation with only parent and child
\(1) destroys the affiliation at the child, but not at the parent and
\(2) leaves both buffers alive."
  (let ((tname "079-ab-disconnect-parent-child")
	(parent (get-buffer-create "079-parent"))
	(child (get-buffer-create "079-child")))
    ;; (error "079-ab-disconnect-parent-child is not yet implemented." tname)
    (with-current-buffer parent
      (ab-init)
      (ab-defvar alpha 1 "Docstring for alpha.")
      (ab-defvar beta 3 "Docstring for beta.")
      (ab-defvar gamma 9 "Docstring for gamma.")
      (ab-new-buffer child))
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is in an affiliation." tname b)
			 (ab-buffer-in-affiliation-p b)))
	    (mapc (lambda (v)
		    (should (and (message "%s(): Checking that %S is indeed a buffer-local variable." tname v)
				 (local-variable-exists-p b v))))
		  (list 'alpha 'beta 'gamma)))
	  (list parent child))
    (message "%s(): Disconnecting the child." tname)
    (ab-disconnect child)
    (mapc (lambda (b)
	    (should (and (message "%s(): Checking that %S is still live." tname b)
			 (buffer-live-p b))))
	  (list parent child))
    (mapc (lambda (v)
	    (should (and (message "%s(): Checking that %S is still a buffer-local variable in %S." tname v parent)
			 (local-variable-exists-p parent v))))
	  (list 'alpha 'beta 'gamma))
    (should-not (and (message "%s(): Checking that %S is no longer in an affiliation." tname child)
		     (ab-buffer-in-affiliation-p child)))
    (with-current-buffer child
      (should (and (message "%s(): Checking that C-xk is mapped to M-x kill-buffer again." tname)
		   (eq 'kill-buffer (key-binding "\C-xk")))))
    (mapc (lambda (v)
	    (should-not (and (message "%s(): Checking that %S is no longer a buffer-local variable." tname v)
			     (local-variable-exists-p child v))))
	  (list 'alpha 'beta 'gamma))
    (abt-clean-buffers parent child)
    ))

(ert-deftest 080-ab-disconnect-stock-buffers ()
  "Test a variety of disconnection scenarios with the stock-buffers."
  (let* ((tname "080-ab-disconnect-stock-buffers")
	 (test-template
	  (lambda (btd otbs)
	    (let ((buffer-to-disconnect)
		  (other-disconnected-buffers)
		  (all-disconnected-buffers)
		  (remaining-buffers))
	      (setq remaining-buffers (ab-descendents abtsb-root))
	      (with-current-buffer abtsb-root
		(ab-defvar alpha 1 "Docstring for alpha")
		(ab-defvar beta 4 "Docstring for beta")
		(ab-defvar gamma 16 "Docstring for gamma"))
	      (progn (setq buffer-to-disconnect btd)
		     (setq other-disconnected-buffers otbs)
		     (setq all-disconnected-buffers (append (list buffer-to-disconnect) other-disconnected-buffers))
		     (mapc (lambda (b)
			     (setq remaining-buffers (delete b remaining-buffers)))
			   all-disconnected-buffers))

	      (message "%s(): " tname)
	      (message "%s(): Going to test exhaustively what happens when you disconnect %S from the stock-buffers." tname btd)
	      (message "%s(): This should also disconnect the following buffers:" tname)
	      (mapc (lambda (b)
		      (message "%s():     %S" tname b))
		    otbs)
	      (message "%s(): The following buffers, however, should remain intact." tname)
	      (mapc (lambda (b)
		      (message "%s():     %S" tname b))
		    remaining-buffers)
	      (message "%s(): " tname)

	      (ab-disconnect buffer-to-disconnect)
	      (mapc (lambda (b)
		      (should (and (message "%s(): Checking that %S is still live." tname b)
				   (buffer-live-p b)))
		      (should-not (and (message "%s(): Checking that %S is no longer in an affiliation." tname b)
				       (ab-buffer-in-affiliation-p b)))
		      (with-current-buffer b
			(should (and (message "%s(): Checking that C-xk is mapped to M-x kill-buffer again." tname)
				     (eq 'kill-buffer (key-binding "\C-xk")))))

		      (mapc (lambda (v)
			      (should-not (and (message "%s(): Checking that %S is no longer local in %S." tname v b)
					       (local-variable-exists-p b v))))
			    (list 'alpha 'beta 'gamma)))
		    all-disconnected-buffers)
	      (mapc (lambda (b)
		      (should (and (message "%s(): Checking that %S is still in an affiliation." tname b)
				   (ab-buffer-in-affiliation-p b)))
		      (mapc (lambda (v)
			      (should (and (message "%s(): Checking that %S is still local in %S." tname v b)
					   (local-variable-exists-p b v))))
			    (list 'alpha 'beta 'gamma)))
		    remaining-buffers)
	      (abt-clean-up-stock-buffers))))
	 )
    ;; (error "080-ab-disconnect-stock-buffers is not yet implemented." tname)
    (message "################################################################")
    (abt-stock-buffers)
    (funcall test-template abtsb-child-0 (list abtsb-grandchild-00 abtsb-grandchild-01))
    (message "################################################################")
    (abt-stock-buffers)
    (funcall test-template abtsb-child-1 ())
    (message "################################################################")
    (abt-stock-buffers)
    (funcall test-template abtsb-grandchild-20 ())
    (message "################################################################")
    (abt-stock-buffers)
    (funcall test-template abtsb-root (list abtsb-child-0
					    abtsb-child-1
					    abtsb-child-2
					    abtsb-grandchild-00
					    abtsb-grandchild-01
					    abtsb-grandchild-20))
    ))


;;
;; Run the tests.
;;
(unless noninteractive
  (ert t))


(provide 'affiliated-buffers-test)
;;; affiliated-buffers-test.el ends here

