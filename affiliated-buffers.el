;;; affiliated-buffers.el --- a package for managing buffers affiliated with another buffer -*- lexical-binding: t -*-

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
;; Version: 0.02
;; Keywords: 

;;; Commentary:

;; Several projects that I've worked on have needed the idea of affiliated buffers.
;; That is, buffers that are somehow conceptually connected.
;; For example, tar-mode has a tar archive and buffers for individual entries.
;; The buffers for individual entries may come and go,
;; but they still need a connection to the archive.
;; After all, that's where they get saved.

;; An affiliation is a hierarchy of buffers.
;; It has a root buffer, and it's members may have children
;; that are also in the affiliation.

;; The root buffer is roughly the buffer that's "in charge";
;; it contains the information that is affiliation wide.
;; For example, the variables that are meaningful throughout the affiliation.
;; (There may be other such generic information, but I don't what it might be yet.)

;; There may be more than one affiliation in a single instance of emacs, but
;; (1) their roots are all different buffers, and
;; (2) their members belong to only one affiliation.

;; If your affiliation is loosely defined,
;; then the root might just be an arbitrarily chosen buffer
;; with children but no grandchildren.
;; Two members of the affiliation are implicitly connected
;; through the root of the affiliation.
;; Any stronger connection is the responsibility of the user,
;; although there are functions for sorting and navigating
;; within the affiliation.

;;; Documentation:

;; (ab-init) creates an affiliation, by marking the current buffer
;; with data that denotes it as the root buffer.
;; You may call (ab-init) in multiple buffers to create multiple affiliations.
;; (ab-init) is idempotent;
;; calling it twice in the same buffer has no further effect.

;; Thereafter, other buffers may be added to the affiliation
;; with (ab-new-buffer BUFFER-OR-NAME) called in the buffer
;; that will be the parent of BUFFER-OR-NAME.
;; If the given buffer does not exist, then it is created first.

;; The members of the affiliation can communicate with variables
;; that are "scoped to the affiliation".
;; Use (ab-defvar VARIABLE VALUE DOCSTRING) to create such a variable;
;; Use (ab-setq) for setting the variables' values.
;; (This scoping is not strictly true.
;; Emacs provides no natural such scoping.
;; It is implemented via buffer-local variables
;; that are coordinated by (ab-defvar) and (ab-setq).
;; Calling one of these functions on an existing variable
;; (1) will make that variable buffer-local
;; (2) will set that variable throughout the affiliation.)

;; (ab-kill-buffer &optional BUFFER-OR-NAME) kills the given BUFFER-OR-NAME
;; and removes it and optionally its descendents from the affiliation.
;; (ab-new-buffer) automatically maps C-xk to (ab-kill-buffer).
;; If the buffer is not a member of an affiliation,
;; then (ab-kill-buffer) does nothing.
;; (ab-disconnect &optional BUFFER-OR-NAME) removes the given BUFFER-OR-NAME
;; and all of its descendents from its affiliation.
;; That is, it is removed from its parent's list of children
;; and all affiliation related variables are killed.
;; Once a buffer has been disconnected from its affiliation
;; there is no way to find it or manage it within the affiliation again.
;; (Of course, there are other ways to find it, (buffer-list), for example.)
;; (ab-disconnect) also remaps C-xk to its original value.
;; CAUTION: (ab-disconnect) does not restore variables.
;; If you've set a buffer via (ab-defvar) or (ab-setq),
;; then disconnecting simply kills that variable.

;; As a general rule, a function or command will error out
;; if the current buffer is not in an affiliation.
;; This is basically to avoid inappropriate uses of these functions.

;; * Here's the full API presented by functional area:

;; ** Affiliation management
;;    ab-defvar (name &optional value docstring)
;;    ab-disconnect (&optional buffer-or-name)
;;    ab-files ()
;;    ab-init ()
;;    ab-kill-buffer (kill-descendents-too &optional buffer-or-name)
;;    ab-new-buffer (buffer-or-name)
;;    ab-next-buffer (arg &optional buffer)
;;    ab-next-sibling (buffer)
;;    ab-previous-buffer (arg &optional buffer)
;;    ab-setq (var value)

;; ** Affiliation access
;;    ab-children (&optional buffer-or-name)
;;    ab-descendents (&optional buffer depth)
;;    ab-last-buffer ()
;;    ab-last-child (&optional buffer-or-name)
;;    ab-leaves (&optional buffer-or-name)
;;    ab-modified-buffers ()
;;    ab-modified-files ()
;;    ab-variables ()

;; ** Predicates
;;    ab-affiliate-p (buffer-or-name)
;;    ab-buffer-in-affiliation-p (&optional buffer-or-name)
;;    ab-leaf-p (&optional buffer-or-name)
;;    ab-root-p ()

;; ** Operating on an affiliation
;;    ab-mapc (function &optional buffer-or-name)
;;    ab-mapcar (function &optional buffer-or-name)
;;    ab-result-for-buffer (buffer results)
;;    ab-save-affiliation (&optional buffer)
;;    ab-save-buffer (&optional buffer-or-name)
;;    ab-sort-buffers (&optional buffer-or-name)

;; ** Navigation
;;    ab-switch-to-last-buffer ()
;;    ab-switch-to-next-buffer (arg)
;;    ab-switch-to-previous-buffer (arg)
;;    ab-switch-to-root-buffer ()

;; * Here's the full API presented alphabetically:

;; ab-affiliate-p (buffer-or-name)
;; ab-buffer-in-affiliation-p (&optional buffer-or-name)
;; ab-children (&optional buffer-or-name)
;; ab-defvar (name &optional value docstring)
;; ab-descendents (&optional buffer depth)
;; ab-disconnect (&optional buffer-or-name)
;; ab-files ()
;; ab-init ()
;; ab-kill-buffer (kill-descendents-too &optional buffer-or-name)
;; ab-last-buffer ()
;; ab-last-child (&optional buffer-or-name)
;; ab-leaf-p (&optional buffer-or-name)
;; ab-leaves (&optional buffer-or-name)
;; ab-mapc (function &optional buffer-or-name)
;; ab-mapcar (function &optional buffer-or-name)
;; ab-modified-buffers ()
;; ab-modified-files ()
;; ab-new-buffer (buffer-or-name)
;; ab-next-buffer (arg &optional buffer)
;; ab-next-sibling (buffer)
;; ab-previous-buffer (arg &optional buffer)
;; ab-result-for-buffer (buffer results)
;; ab-root-buffer (&optional buffer)
;; ab-root-p ()
;; ab-save-affiliation (&optional buffer)
;; ab-save-buffer (&optional buffer-or-name)
;; ab-setq (var value)
;; ab-sort-buffers (&optional buffer-or-name)
;; ab-switch-to-last-buffer ()
;; ab-switch-to-next-buffer (arg)
;; ab-switch-to-previous-buffer (arg)
;; ab-switch-to-root-buffer ()
;; ab-variables ()

;;; Code:


;;
;; Development
;;


;;
;; Generic functions
;;
(defun ab-flatten (l)
  "Return the list of atoms of the given List.
If the given List is NIL, then return NIL.
If it's an atom then just return a list containing that atom.
This will signal an error on a raw cons cell, e.g. (0 . 1)."
  (let ((fname "ab-flatten"))
    (cond ((null l)
	   nil)
	  ((atom l)
	   (list l))
	  ((listp l)
	   (append (ab-flatten (car l)) (ab-flatten (cdr l)))))))


;;
;; Dependencies
;;


;;
;; Vars
;;

(defvar ab-initialized nil
  "Variable that holds status about whether the current buffer is initialized.")
(make-variable-buffer-local 'ab-initialized)

(defvar ab-children ()
  "A buffer-local variable holding the list of affiliated buffers
that are children of the current buffer.")
(make-variable-buffer-local 'ab-children)

(defvar ab-variables-registry ()
  "A buffer-local variable holding the list of variables
defined in the current buffer and all of its affiliates,
both ancestors and descendents.")
(make-variable-buffer-local 'ab-variables-registry)

(defvar ab-parent ()
  "If non-NIL this buffer-local variable is a buffer,
the parent buffer of the current buffer's affiliation.
By traversing parents you can find the root buffer
of the current buffers' affiliation.")
(make-variable-buffer-local 'ab-parent)

(defvar ab-protected-variables (list 'ab-initialized 'ab-children 'ab-variables-registry 'ab-parent 'ab-protected-variables)
  "List of variables that cannot be set with (ab-setq)
or defined by (ab-defvar).")

(defvar ab-isearch-history ()
  "History for the ab-isearch-forward/backward commands.")

;;
;; Library
;;

(defun ab-init ()
  "Establish a structure for managing affiliated buffers.
You can only execute this in the root buffer;
it will error out in any child buffers.
Running this twice has no effect."
  (let ((fname "ab-init"))
    (cond ((and (local-variable-p ab-parent)
		(not (null ab-parent)))
	   (error "%s(): You're not in the root buffer »%S« of the affiliation for »%S«." fname (ab-root-buffer (current-buffer)) (current-buffer)))
	  ((ab-buffer-in-affiliation-p (current-buffer))
	   (message "%s(): This buffer »%s« has already been initialized." fname (current-buffer)))
	  (t
	   (setq-local ab-children ())
	   (setq-local ab-variables-registry ())
	   (setq-local ab-parent ())
	   (setq-local ab-variables-registry ())
	   (or (local-variable-p 'ab-variables-registry)
	       (error "%s(): ab-variables-registry is not a local variable." fname))
	   (local-set-key "\C-xk" 'ab-kill-buffer)
	   (setq-local ab-initialized t)))))

(defun ab-new-buffer (buffer-or-name)
  "Affiliate a buffer with the current buffer and return the new buffer.
If BUFFER-OR-NAME is a buffer, then
    1. if that buffer is live, then affiliate it with the current buffer;
    2. if that buffer is not live, then error out.
otherwise, if buffer-or-name is a name
then create a new buffer with that name and
affiliate that new buffer with the current buffer."
  (let ((fname "ab-new-buffer")
	(parent (current-buffer)))
    (if (stringp buffer-or-name) (setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (error "%s(): proposed buffer »%s« is not a buffer." fname buffer-or-name))
    (unless (buffer-live-p buffer-or-name)
      (error "%s(): proposed buffer »%s« is not live." fname buffer-or-name))
    ;; (if (ab-buffer-in-affiliation-p buffer-or-name)
    ;;	(error "%s(): buffer %s is already in an affiliation." fname (buffer-name buffer-or-name)))

    (cond ((ab-affiliate-p buffer-or-name)
	   (message "%s(): »%S« is already affiliated with the current buffer »%s«."
		    fname buffer-or-name (current-buffer))
	   buffer-or-name)
	  ((ab-buffer-in-affiliation-p buffer-or-name)
	   (message "%s(): buffer %s is already in an affiliation." fname (buffer-name buffer-or-name))
	   (sit-for 0.5)
	   (message "%s(): The root is »%S«." fname (with-current-buffer buffer-or-name (ab-root-buffer)))
	   (sit-for 0.5)
	   nil)
	  (t
	   ;; (ab-root-buffer)
	   (mapc (lambda (v)
		   (let ((value (eval v)))
		     (with-current-buffer buffer-or-name
		       (set (make-local-variable v) value))))
		 (with-current-buffer (ab-root-buffer)
		   ab-variables-registry))))
    (with-current-buffer buffer-or-name
      (setq-local ab-parent parent)
      (local-set-key "\C-xk" 'ab-kill-buffer))
    (push buffer-or-name ab-children)
    buffer-or-name))

(defun ab-buffer-in-affiliation-p (&optional buffer-or-name)
  "Return non-NIL if BUFFER-OR-NAME is in an affiliation."
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-buffer-in-affiliation-p"))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (error "%s(): Proposed buffer %S is not a buffer." fname buffer-or-name))
    (if (buffer-live-p buffer-or-name)
	(with-current-buffer buffer-or-name
	  (and (boundp 'ab-parent)
	       (local-variable-p 'ab-parent)))
      nil)))

(defmacro ab--defvar (name value &optional docstring)
  "The internal workings of (ab-defvar). Don't invoke this directly."
  (let ((fname "ab--defvar"))
    (declare (docstring 3) (indent 2))
    `(progn
       (cond ((and ab-initialized
		   (null ab-parent))
	      (let ((var (make-local-variable (quote ,name))))
		(cond ((member var ab-protected-variables)
		       (error "%s(): »%S« is a protected variable." ,fname var))
		    (t
		     (set var ,value)
		     (put var 'variable-documentation ,docstring)
		     (mapc (lambda (b)
			     (with-current-buffer b
			       (set
				(make-local-variable (quote ,name))
				,value)
			       (if ,docstring
				   (put var 'variable-documentation ,docstring))))
			   (ab-descendents))
		     (ab--defvar-register-maybe var)))))
	     (t
	      (error "%s(): You called (ab--defvar) directly, didn't you?" ,fname))))))

(defun ab--defvar-register-maybe (var)
  "Register the given VARiable. Return the variable."
  (let ((fname "ab--defvar-register-maybe"))
    (cond ((and ab-initialized
		(null ab-parent))
	   (unless (member var ab-variables-registry)
	     (push var ab-variables-registry))
	   var)
	  (t
	   (error "%s(): You're not in the root buffer of an affiliation." fname)))))

(defmacro ab-defvar (name &optional value docstring)
    "Define the given SYMBOL as a variable in the current and all affiliated buffers.
Set its VALUE if one is given; it's NIL otherwise.
Associate the given DOCSTRING with the newly defined variables.

Note that this defines SYMBOL as a buffer-local variable in all the buffers;
the value is the same in all affiliated buffers.

CAVEAT: This is a macro that calls a macro,
so if VALUE is actually a calculation, then that calculation is run twice.
"
    (let ((fname "ab-defvar"))
      (declare (docstring 3) (indent 2))
      `(progn
	 (cond ((null ab-parent)
		(ab--defvar ,name ,value ,docstring))
	       (t
		(with-current-buffer (ab-root-buffer)
		  (ab--defvar ,name ,value ,docstring)))))))

(defmacro ab-setq (var value)
  "Set the given VAR to the given VALUE and return the new value.

CAVEAT: This is a macro that calls a macro,
so if VALUE is actually a calculation, then that calculation is run twice.
"
  (let ((fname "ab-setq"))
    `(progn (ab-defvar ,var ,value)
	    ,value)))

(defun ab-root-buffer (&optional buffer)
  "Return the root buffer for the affiliation that includes the given BUFFER.
Return NIL if BUFFER is not in an affiliation."
  (unless buffer (setq buffer (current-buffer)))
  (let ((fname "ab-root-buffer"))
    (unless (bufferp buffer)
      (error "%s(): Proposed buffer »%S« is not a buffer." fname buffer))
    (with-current-buffer buffer
      (cond ((and ab-initialized
		  (boundp 'ab-parent)
		  (null ab-parent))
	     (current-buffer))
	    (ab-parent
	     (ab-root-buffer ab-parent))
	    (t
	     nil)))))

(defun ab-leaves (&optional buffer-or-name)
  "Return a list of leaves in the current affiliation."
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-leaves")
	(leaves ()))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (error "%s(): Proposed buffer »%S« is not a buffer or string." fname buffer-or-name))
    (mapc (lambda (b)
	    (unless (with-current-buffer b ab-children)
	      (push b leaves)))
	  (ab-descendents buffer-or-name))
    leaves))

(defun ab-descendents (&optional buffer depth)
  "Return a list of all the descendents of the given BUFFER.
If no buffer is given, then use the current buffer.
The depth parameter is strictly for internal use.

The list is ordered by depth-first search.
That is, start with the given buffer, then move to its first child,
and the first child, of that, etc. until you've exhausted
all 'first' descendents.
Then move on to the next sibling, traverse its children and so on."
  (unless depth (setq depth 0))
  (unless buffer (setq buffer (current-buffer)))
  (let ((fname "ab-descendents"))
    (unless (bufferp buffer)
      (error "%s(): Proposed buffer »%S« is not a buffer." fname buffer))
    (if (buffer-live-p buffer)
	(with-current-buffer buffer
	  (unless (local-variable-p 'ab-parent)
	    (error "%s(): You're not in a buffer »%S« with affiliations." fname buffer))
	  (cond (ab-children
		 (append (list buffer)
			 (ab-descendents (car ab-children) (1+ depth))
			 (apply #'append
				(mapcar (lambda (b)
					  (ab-descendents b (1+ depth)))
					(cdr ab-children)))))
		(t (list buffer))))
      ())))

(defun ab-disconnect (&optional buffer-or-name)
  "Remove the given BUFFER-OR-NAME from its affiliation.
That is, (1) remove it from its parent's list of children,
\(2) remove all information connecting to the affiliation
from it and all of its descendents.
This includes all of the affiliation's variables,
all the parent/child relationships and
remapping \C-xk bask to kill buffer.

If BUFFER-OR-NAME is not given then use the current buffer."
  ;; Implement this by looping until its list of children is empty.
  ;; For each iteration of the loop,
  ;; map over the children, pruning the leaves.
  ;; No, you can just loop over the descendents.
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-disconnect")
	(descendents)
	(local-variables-registry))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (signal 'wrong-type-error (list buffer-or-name)))
    (unless (ab-buffer-in-affiliation-p buffer-or-name)
      (error "%s(): You're not in an affiliation." fname))
    (setq descendents (ab-descendents buffer-or-name))
    (with-current-buffer (ab-root-buffer buffer-or-name)
      (setq local-variables-registry ab-variables-registry))
    (with-current-buffer buffer-or-name
      (if (buffer-live-p ab-parent)
	  (with-current-buffer ab-parent
	    (setq ab-children (delete buffer-or-name ab-children)))))
    (mapc (lambda (b)
	    (with-current-buffer b
	      (local-unset-key "\C-xk")
	      (mapc #'makunbound local-variables-registry)
	      (mapc #'makunbound (list 'ab-parent
				       'ab-children
				       'ab-variables-registry
				       'ab-initialized))))
	  descendents)))

(defun ab-variables ()
  "Return the list of variables (as symbols) for the current affiliation."
  (let ((fname "ab-variables"))
    (unless (ab-buffer-in-affiliation-p (current-buffer))
      (error "%s(): »%S« is not in an affiliation." fname (current-buffer)))
    (with-current-buffer (ab-root-buffer)
      ab-variables-registry)))

(defun ab-modified-buffers ()
  "Return a list of the modified buffers in the current affiliation."
  (let ((fname "ab-modified-buffers")
	(results ()))
    (unless (ab-buffer-in-affiliation-p (current-buffer))
      (error "%s(): »%S« is not in an affiliation." fname (current-buffer)))
    (mapc (lambda (b)
	    (if (buffer-modified-p b)
		 (push b results)))
	  (ab-descendents (ab-root-buffer)))
    results))

(defun ab-files ()
  "Return the list of buffers in the current affiliation that contain files.
Note that this returns a list of /buffers/, not the files themselves."
  (let ((fname "ab-files")
	(results ()))
    (unless (ab-buffer-in-affiliation-p (current-buffer))
      (error "%s(): »%S« is not in an affiliation." fname (current-buffer)))
    (mapc (lambda (b)
	    (if (buffer-file-name b)
		(push b results)))
	  (ab-descendents (ab-root-buffer)))))

(defun ab-modified-files ()
  "Return the list of buffers in the current affiliation that are modified and contain files."
  (let ((fname "ab-modified-files")
	(results ()))
    (unless (ab-buffer-in-affiliation-p (current-buffer))
      (error "%s(): »%S« is not in an affiliation." fname (current-buffer)))
    (mapc (lambda (b)
	    (if (and (buffer-file-name b)
		     (buffer-modified-p b))
		(push b results)))
	  (ab-descendents (ab-root-buffer)))
    results))

(defun ab-root-p ()
  "Return non-NIL if the current buffer is the root of the current affiliation."
  ;; It doesn't really make sense to ask this about an arbitrary buffer
  ;; since that doesn't shed light on which affiliation
  ;; that buffer would the root of.
  (let ((fname "ab-root-p"))
    (unless (ab-buffer-in-affiliation-p (current-buffer))
      (error "%s(): »%S« is not in an affiliation." fname (current-buffer)))
    (if (null ab-parent)
	t
      nil)))

(defun ab-leaf-p (&optional buffer-or-name)
  "Return non-NIL if the given BUFFER-OR-NAME has no children.
If BUFFER-OR-NAME is not given, the use the current buffer."
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-leaf-p")
	(ret t))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (with-current-buffer buffer-or-name (ab-buffer-in-affiliation-p buffer-or-name))
      (error "%s(): »%S« is not in an affiliation." fname buffer-or-name))
    (unless (buffer-live-p buffer-or-name)
      (signal 'wrong-type-error (list buffer-or-name)))

    (catch 'done
      (mapc (lambda (b)
	      (cond ((buffer-live-p b)
		     (setq ret nil)
		     (throw 'done nil))
		    (t t)))
	    (with-current-buffer buffer-or-name ab-children)))
    ret))

(defun ab-affiliate-p (buffer-or-name)
  "Return non-NIL if BUFFER-OR-NAME is in the same affiliation as the current buffer."
  (let ((fname "ab-affiliate-p"))
    (if (stringp buffer-or-name)
      (setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (error "%s(): Argument »%S« is neither a buffer nor string." fname buffer-or-name))
    (cond ((and (buffer-live-p buffer-or-name)
	     (ab-buffer-in-affiliation-p buffer-or-name))
	   (member (current-buffer) (ab-descendents (ab-root-buffer buffer-or-name))))
	  (t
	   nil))))

(defun ab-children (&optional buffer-or-name)
  "Return the children of the given BUFFER-OR-NAME.
If BUFFER-OR-NAME is not given, then use the current buffer."
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-children"))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (error "%s(): Proposed buffer »%S« is not a buffer." fname buffer-or-name))
    (unless (ab-buffer-in-affiliation-p buffer-or-name)
      (error "%s(): Proposed buffer »%S« is not in an affiliation." fname buffer-or-name))
    (with-current-buffer buffer-or-name ab-children)))

(defun ab-mapcar (function &optional buffer-or-name)
  "Apply the given FUNCTION to BUFFER-OR-NAME and all of its descendents.
FUNCTION takes the buffer as an argument.
It can access all of the variables in the current affiliation
defined by (ab-defvar).
If BUFFER is not given, then use the current buffer.
The hierarchy of descendents is traversed depth first,
but function should not depend on the order.
Ancestors are well-defined, but next and previous may not be.

The return value reflects the structure of the affiliation
with the return values of FUNCTION associated with the corresponding buffer.
Here's an example of a return value from the automated tests:
    (#<buffer abtsb-root> 31
	      (#<buffer abtsb-child-2> 17
			(#<buffer abtsb-grandchild-20> 18))
	      (#<buffer abtsb-child-1> 19)
	      (#<buffer abtsb-child-0> 20
			(#<buffer abtsb-grandchild-01> 21)
			(#<buffer abtsb-grandchild-00> 22)))
"
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-mapcar")
	(results ()))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (error "%s(): Proposed buffer »%S« is not a buffer." fname buffer-or-name))
    (unless (ab-buffer-in-affiliation-p buffer-or-name)
      (error "%s(): Proposed buffer »%S« is not in an affiliation." fname buffer-or-name))
    (setq results (list buffer-or-name
			(funcall function buffer-or-name)))
    (append results (mapcar (lambda (b)
			      (ab-mapcar function b))
			    (ab-children buffer-or-name)))))

(defun ab-result-for-buffer (buffer results)
  "Return the result for the given BUFFER from the given RESULTS,
where RESULTS is a return value from (ab-mapcar)."
  (let ((fname "ab-result-for-buffer")
	(flat-results (ab-flatten results))
	(results-alist ())
	(this-buffer)
	(result)
	(buffer-ct 1))
    (unless (bufferp buffer)
      (signal 'wrong-type-error (list buffer)))
    (unless (= 0 (mod (length flat-results) 2))
      (error "%s(): Proposed result set »%S« is not the return value from (ab-mapcar)." fname (pp results)))
    (while flat-results
      (unless (bufferp (setq this-buffer (pop flat-results)))
	(error "%s(): Proposed buffer (number %d), »%S«, is not a buffer." fname buffer-ct buffer))
      (setq buffer-ct (1+ buffer-ct))
      (setq result (pop flat-results))
      (push (cons this-buffer result) results-alist))
    (cdr (assoc buffer results-alist))))

(defun ab-mapc (function &optional buffer-or-name)
  "Map over the descendents of BUFFER-OR-NAME, invoking FUNCTION in each buffer.
If the optional BUFFER-OR-NAME is not given, then use the current buffer.
Return the hierarchy over which the map was applied."
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-mapc")
	(results (list buffer-or-name)))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (error "%s(): Proposed buffer-or-name »%S« is not a buffer." fname buffer-or-name))
    (funcall function buffer-or-name)
    (setq results (list buffer-or-name))
    (mapc (lambda (b)
	    (setq results
		  (append results
			  (list (ab-mapc function b)))))
	  (ab-children buffer-or-name))
    results))

(defun ab-next-buffer (arg &optional buffer)
  "Return the ARGth next buffer following BUFFER in the current affiliation.
The buffers of the affiliation are treated like a ring,
so that the root of the affiliation follows its last buffer.
If BUFFER is not given, then use the current buffer;
this is really only for internal use as (ab-next-buffer) recurses on ARG."
  (unless buffer (setq buffer (current-buffer)))
  (let ((fname "ab-next-buffer")
	(buffers)
	(i 0)
	(target-idx 0)
	(buffer-ct 0))
    ;; (ab-descendents) very nicely organizes the buffers in just the right order.
    (setq buffers (ab-descendents (ab-root-buffer buffer)))
    (setq buffer-ct (length buffers))
    (while (and (< i buffer-ct)
		(not (eq buffer (nth i buffers))))
      (setq i (1+ i)))
    (setq target-idx (mod (+ i arg) buffer-ct))
    (nth target-idx buffers)))

(defun ab-next-sibling (buffer)
  "Return the next sibling of BUFFER in the current affiliation."
  (let ((fname "ab-next-sibling")
	(siblings)
	(this-sibling))
    (unless (bufferp buffer)
      (signal 'wrong-type-error (list buffer)))
    (unless (ab-buffer-in-affiliation-p buffer)
      (error "%s(): Buffer %S is not in an affiliation." fname buffer))
    (cond ((with-current-buffer buffer ab-parent)
	   (with-current-buffer
	       (with-current-buffer buffer ab-parent)
	     (setq siblings (copy-sequence ab-children)))
	   (catch 'found-it
	     (while siblings
	       (if (and (setq this-sibling (pop siblings))
			(eq this-sibling buffer))
		   (throw 'found-it t))))
	   (pop siblings))
	  (t				;The root buffer.
	   nil))))

(defun ab-previous-buffer (arg &optional buffer)
  "Return the ARGth buffer preceding the given BUFFER."
  (unless buffer (setq buffer (current-buffer)))
  (let ((fname "ab-previous-buffer"))
    (unless (bufferp buffer)
      (error "%s(): The proposed buffer %S is not a buffer." fname buffer))
    (unless (ab-buffer-in-affiliation-p buffer)
      (error "%s(): You can't navigate an affiliation if you're not in one." fname))
    (ab-next-buffer (- arg) buffer)))

(defun ab-last-buffer ()
  "Return the last buffer in the current affiliation."
  (let ((fname "ab-last-buffer")
	(last-child))
    (unless (ab-buffer-in-affiliation-p (current-buffer))
      (error "%s(): You're not in an affiliation." fname))
    (cond ((setq last-child (ab-last-child (ab-root-buffer)))
	   (while (with-current-buffer last-child ab-children)
	     (setq last-child (ab-last-child last-child)))
	   last-child)
	  (t
	   (ab-root-buffer)))))

(defun ab-last-child (&optional buffer-or-name)
  "Return the last child (however they happen to be ordered) of BUFFER-OR-NAME.
If no BUFFER-OR-NAME is given, then use the current buffer."
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-last-child"))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (error "%s(): Proposed buffer »%S« is not a buffer." fname buffer-or-name))
    (unless (ab-buffer-in-affiliation-p buffer-or-name)
      (error "%s(): Proposed buffer %S is not in an affiliation." fname buffer-or-name))
    (with-current-buffer buffer-or-name
      (car (last ab-children)))))


;;
;; Commands
;;
(defun ab-save-affiliation (&optional buffer)
  "Save all the files in the affiliation that contains BUFFER.

Your application may have its own concept of saving, and, if so,
it should implement that concept on its own.
tar-mode is an example of a mode that has it's own concept of saving."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (let ((fname "ab-save-affiliation")
	(root-buffer)
	(descendents)
	)
    ;; (error "%s(): not yet implemented." fname)
    (unless (bufferp buffer)
      (error "%s(): Proposed buffer »%S« is not a buffer." fname buffer))
    (unless (ab-buffer-in-affiliation-p buffer)
      (error "%s(): Proposed buffer »%S« is not in an affiliation." fname buffer))
    (ab-mapc (lambda (b)
	       (with-current-buffer b
		 (if (buffer-file-name)
		     (save-buffer))))
	     (ab-root-buffer buffer))
    ))

(defun ab-kill-buffer (kill-descendents-too &optional buffer-or-name)
  "Kill the affiliated BUFFER-OR-NAME and possibly of its descendents.
The buffer is no longer known as a child of its parent.
With a prefix arg also kill all descendents.
If that buffer is not affiliated (directly or indirectly) with the current buffer,
then error out."
  ;; Here's the strategy:
  ;; 0. Establish the contract.
  ;; 1. If the buffer is not in an affiliation, then do nothing.
  ;; 2. Otherwise, if kill-descendents-too is set, then just kill all the buffers...
  ;; 2a. ...and leave.
  ;; Otherwise,
  ;; 3. Find the variables of interest.
  ;; 4. Find the descendents of the buffer.
  ;; 5. For each descendent, remove it from its parent's children.
  ;; 6. Unbind the variables in each descendent.
  ;; 7. Unbind the variables ab-parent/children in each descendent.
  ;; 8. Actually kill the buffer.
  (interactive "P")
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-kill-buffer")
	(descendents)
	(variables))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (get-buffer-create buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (error "%s(): Proposed buffer »%S« is not a buffer." fname buffer-or-name))
    (cond ((and (ab-buffer-in-affiliation-p buffer-or-name)
		kill-descendents-too)
	   (mapc (lambda (b)
		   (kill-buffer b))
		 (ab-descendents buffer-or-name)))
	  ((ab-buffer-in-affiliation-p buffer-or-name)
	   (with-current-buffer (ab-root-buffer buffer-or-name)
	     (setq variables (ab-variables)))
	   (setq descendents (ab-descendents buffer-or-name))
	   (mapc (lambda (b)
		   (cond ((eq b buffer-or-name)
			  (with-current-buffer buffer-or-name
			    (if ab-parent
				(with-current-buffer ab-parent
				  (setq ab-children (delete b ab-children))))))
			 (t
			  (with-current-buffer b
			    (makunbound 'ab-parent)
			    (makunbound 'ab-children)
			    (mapc (lambda (v)
				    (cond ((boundp v)
					   (makunbound v))
					  (t t)))
				  variables)))))
		 descendents)
	   (kill-buffer buffer-or-name))
	  (t nil))))

(defun ab-sort-buffers (&optional buffer-or-name)
  "Sort the hierarchy of the affiliation of BUFFER by the buffers' names.
If BUFFER is not given, then use the current buffer.
CAVEATS: (1) This does not affect any buffers added to the affiliation
             after this is called.
         (2) Buffers with similar names may not sort the way you want.
             For example, bob, bob<1> and bob<2>.
             Since there's no way to predict just how they should be ordered,
             it's up to the user to find better names."
  (interactive)
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "ab-sort-buffers"))
    (if (stringp buffer-or-name)
	(setq buffer-or-name (find-file-noselect buffer-or-name)))
    (unless (bufferp buffer-or-name)
      (message "%s(): Proposed buffer »%S« is not a buffer." fname buffer-or-name))
    ;; This could be an application of (ab-mapc).
    ;; Would that be more natural?
    (mapc (lambda (b)
	    (with-current-buffer b
	      (setq ab-children (sort ab-children (lambda (l r) (string-lessp (buffer-name l) (buffer-name r)))))))
	  (ab-descendents (ab-root-buffer buffer-or-name)))))

(defun ab-switch-to-previous-buffer (arg)
  "Switch to the ARGth previous buffer in the current affiliation.
If arg is 0, then don't change buffers;
if arg is negative, then switch to the -ARGth next buffer.
Note that if the affiliation is not sorted the way you want it,
then this will probably do things you don't want."
  (interactive "p")
  (let ((fname "ab-switch-to-previous-buffer"))
    (unless (ab-buffer-in-affiliation-p (current-buffer))
      (error "%s(): You're not in an affiliation." fname))
    (ab-switch-to-next-buffer (- arg))))

(defun ab-switch-to-next-buffer (arg)
  "Switch to the ARGth next buffer in the current affiliation.
If arg is 0, then don't change buffers;
if arg is negative, then switch to the -ARGth previous buffer.
Note that if the affiliation is not sorted the way you want it,
then this will probably do things you don't want."
  (interactive "p")
  (let ((fname "ab-switch-to-next-buffer"))
    (unless (ab-buffer-in-affiliation-p (current-buffer))
      (error "%s(): You can't navigate an affiliation when you're not in one." fname))
    (unless (integerp arg)
      (signal 'wrong-type-error (list arg)))
    (switch-to-buffer (ab-next-buffer arg))))

(defun ab-switch-to-root-buffer ()
  "Switch to the root buffer in the current affiliation."
  (interactive)
  (let ((fname "ab-switch-to-root-buffer"))
    (cond ((ab-buffer-in-affiliation-p (current-buffer))
	   (switch-to-buffer (ab-root-buffer)))
	  (t
	   ;; MAINTAINANCE This message is used in the tests.
	   (message "%s(): You're not in an affiliation." fname)))))

(defun ab-switch-to-last-buffer ()
  "Switch to the last buffer in the current affiliation.
The meaning of 'last' is defined by the fact
that mapping over the affiliation is performed depth-first."
  (interactive)
  (let ((fname "ab-switch-to-last-buffer")
	(last-buffer))
    (cond ((ab-buffer-in-affiliation-p (current-buffer))
	   (setq last-buffer (ab-last-buffer))
	   (switch-to-buffer last-buffer))
	  (t
	   ;; MAINTAINANCE This message is used in the tests.
	   (message "%s(): You're not in an affiliation." fname)))))

(defun ab-occur (regexp &optional nlines)
  "Show all lines in all the buffers in the current affiliation
containing a match for REGEXP.
This function acts on multiple buffers; otherwise, it is exactly like
`occur'.
See also `multi-occur'."
  (interactive (occur-read-primary-args))
  (occur-1 regexp nlines (ab-descendents (ab-root-buffer))))

(defun ab-search-forward (string)
  "Search forward through the current affiliation for the given STRING.
The search begins at point in the current buffer.
This is not like isearch-forward since it has to switch-buffers.
The usual caveats for the order of buffers in the affiliation apply;
see `affiliated-buffers-mode'."
  (interactive (list (read-from-minibuffer
		      "Search string? "
		      (car ab-isearch-history)
		      nil
		      nil
		      ab-isearch-history
		      'inherit-input-method)))
  (let ((fname "ab-search-forward")
	(buffers (ab-descendents (ab-root-buffer)))
	(root (ab-root-buffer))
	(start)
	(end)
	)
    ;; (error "%s(): not yet implemented." fname)
    (cond ((ab-root-p)
	   t)
	  (t
	   (while (not (eq (car buffers) root))
	     (setq buffers (cdr buffers)))))
    (catch 'done
      (mapc (lambda (b)
	      (switch-to-buffer b)
	      (cond ((eq b (car buffers))
		     t)
		    (t
		     (goto-char (point-min))))
	      (while (search-forward string (point-max) t)
		(setq start (match-beginning 0))
		(setq end (match-end 0))
		(add-face-text-property start end 'highlight)
		(cond ((y-or-n-p "Continue? ")
		       (add-face-text-property start end 'default))
		      (t
		       (add-face-text-property start end 'default)
		       (throw 'done t)))))
	    buffers))
    ))

(defun ab-query-replace (from-string to-string) ; &optional delimited start end)
  "Replace some occurrences of FROM-STRING with TO-STRING
in the current affiliation.
As each match is found, the user must type a character saying
what to do with it."
  (interactive
   ;; Taken from M-x query-replace
   (let ((common
	  (query-replace-read-args
	   (concat "Query replace"
		   (if current-prefix-arg
		       (if (eq current-prefix-arg '-) " backward" " word")
		     "")
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common)) ; (nth 2 common)
     ;; ;; These are done separately here
     ;; ;; so that command-history will record these expressions
     ;; ;; rather than the values they had this time.
     ;; (if (and transient-mark-mode mark-active)
     ;;     (region-beginning))
     ;; (if (and transient-mark-mode mark-active)
     ;;     (region-end))
     ;; (nth 3 common))))
     ))
  (let ((fname "ab-query-replace")
	(previous-buffer nil)
	(previous-position (point))
	(root-buffer (ab-root-buffer))
	(start)
	(end)
	)
    ;; (error "%s(): not yet implemented." fname)
    (while (or (null previous-buffer)
	       (not (eq (current-buffer) root-buffer)))
      (while (search-forward from-string (point-max) t)
	(setq start (match-beginning 0))
	(setq end  (match-end 0))
	;; Face: highlight (match-beginning 0) (match-end 0)
	;; (add-face-text-property start end 'highlight) (sit-for 0.5)
	;; (when query-replace-lazy-highlight
	;;     (setq isearch-lazy-highlight-last-string nil))
	;; Taken from M-x replace-match.
	(replace-highlight start end
			   start end
			   from-string
			   nil
			   nil
			   nil
			   nil)
	(if (called-interactively-p 'interactive)
	    (if (y-or-n-p (format "Query replacing %s with %s." from-string to-string))
		(replace-match to-string))
	  (replace-match to-string))
	;; Restore face
	(add-face-text-property start end 'default) (sit-for 0.5)
	)
      (setq previous-position (match-end 0))
      (setq previous-buffer (current-buffer))
      (ab-switch-to-next-buffer 1)
      (goto-char (point-min)))
    ;; We may have changed buffers after the last match, so go back.
    (switch-to-buffer previous-buffer)
    (goto-char previous-position)
    ))

(defun ab-query-replace-regexp (regexp to-string) ; &optional delimited  start end backward)
  "Replace some things after point matching REGEXP with TO-STRING
in the current book.
As each match is found, the user must type a character saying
what to do with it."
  (interactive
   ;; Taken from M-x query-replace-regexp
   (let ((common
	  (query-replace-read-args
	   (concat "Query replace"
		   (if current-prefix-arg
		       (if (eq current-prefix-arg '-) " backward" " word")
		     "")
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common)
	   )
	   ;; ;; These are done separately here
	   ;; ;; so that command-history will record these expressions
	   ;; ;; rather than the values they had this time.
	   ;; (if (and transient-mark-mode mark-active)
	   ;;     (region-beginning))
	   ;; (if (and transient-mark-mode mark-active)
	   ;;     (region-end))
	   ;; (nth 3 common))
     ))
  (let ((fname "ab-query-replace-regexp")
	(previous-buffer nil)
	(previous-position (point))
	(root-buffer (ab-root-buffer))
	(start)
	(end)
	)
    ;; (error "%s(): not yet implemented." fname)
    (while (or (null previous-buffer)
	       (not (eq (current-buffer) root-buffer)))
	(while (re-search-forward regexp (point-max) t)
	  (setq start (match-beginning 0))
	  (setq end  (match-end 0))
	  ;; Face: highlight (match-beginning 0) (match-end 0)
	  ;; (add-face-text-property start end 'highlight)
	  ;; Taken from M-x replace-match.
	  (replace-highlight start end
			     start end
			     from-string
			     nil
			     nil
			     nil
			     nil)
	  (if (called-interactively-p)
	      (if (y-or-n-p (format "Query replacing %s with %s." regexp to-string))
		  (replace-match to-string))
	    (replace-match to-string))
	  ;; Restore face
	  (add-face-text-property start end 'default)
	  )
	(setq previous-position (match-end 0))
	(setq previous-buffer (current-buffer))
	(ab-switch-to-next-buffer 1)
	(goto-char (point-min)))
    ;; We may have changed buffers after the last match, so go back.
    (switch-to-buffer previous-buffer)
    (goto-char previous-position)
    ))

(defun ab-search-backward (string)
  "Search backward for the given STRING.
See `ab-search-forward' for information
on how this differs from `isearch-backward'."
  (interactive (list (read-from-minibuffer
		      "Search string? "
		      (car ab-isearch-history)
		      nil
		      nil
		      ab-isearch-history
		      'inherit-input-method)))
  (let ((fname "ab-search-backward")
	(buffers (ab-descendents (ab-root-buffer)))
	(root (ab-root-buffer))
	(start)
	(end)
	)
    ;; (error "%s(): not yet implemented." fname)
    (setq buffers (nreverse buffers))
    (while (not (eq (car buffers) (current-buffer)))
      (setq buffers (cdr buffers)))
    ;; (setq buffers (nreverse buffers))
    (catch 'done
      (mapc (lambda (b)
	      (switch-to-buffer b)
	      (cond ((eq b (car buffers))
		     t)
		    (t
		     (goto-char (point-max))))
	      (message "%s(): Searching in %S." fname b) (sit-for 1.0)
	      (while (search-backward string (point-min) t)
		(setq start (match-beginning 0))
		(setq end (match-end 0))
		(add-face-text-property start end 'highlight)
		(cond ((y-or-n-p "Continue? ")
		       (add-face-text-property start end 'default))
		      (t
		       (add-face-text-property start end 'default)
		       (throw 'done t)))))
	    buffers))
    ))


;;
;; Mode definition (if appropriate)
;;
(setq affiliated-buffers-mode-map (make-sparse-keymap))
(define-key affiliated-buffers-mode-map "\C-cr"    'ab-switch-to-root-buffer) ;✓
(define-key affiliated-buffers-mode-map "\C-cn"    'ab-switch-to-next-buffer) ;✓
(define-key affiliated-buffers-mode-map "\C-cp"    'ab-switch-to-previous-buffer) ;✓
(define-key affiliated-buffers-mode-map "\C-c%"    'ab-query-replace) ;✓
(define-key affiliated-buffers-mode-map "\C-c\M-%" 'ab-query-replace-regexp) ;✓
(define-key affiliated-buffers-mode-map "\C-cs"    'ab-save-affiliation) ;✓
(define-key affiliated-buffers-mode-map "\C-co"    'ab-occur) ;✓
(define-key affiliated-buffers-mode-map "\C-c\C-s" 'ab-search-forward) ;✓
(define-key affiliated-buffers-mode-map "\C-c\C-r" 'ab-search-backward) ;✓

(define-minor-mode affiliated-buffers-mode
  "A minor mode to edit all the files of an affiliation at once.

key         binding
---         -------
C-c r       ab-switch-to-root-buffer
C-c n       ab-switch-to-next-buffer
C-c p       ab-switch-to-previous-buffer
C-c %       ab-query-replace
C-c M-%     ab-query-replace-regexp
C-c s       ab-save-affiliation
C-c o       ab-occur
C-c C-s     ab-search-forward
C-c C-r     ab-search-backward

NOTES
\(1) This assumes that you've already arranged
    for the current buffer to be the root of an affiliation;
    see example/book.tex for an example of how to do that.
\(2) This minor mode offers navigation among the buffers
    of an affiliation.
    If the affiliation has not been constructed in the right order,
    then that navigation may not be predictable.
    M-x ab-sort-buffers performs some rudimentary sorting,
    but probably doesn't do what you want.
    Similar comments may apply to searching and replacing.
\(3) Constructive comments are welcome.
"
  :init-value nil
  :lighter " AB"
  :keymap affiliated-buffers-mode-map
  :global nil
  :group 'affiliated-buffers-mode
  :version "0.02"
  (let ((fname "affiliated-buffers-mode"))
    (if (eq (current-buffer) (ab-root-buffer))
	(mapc (lambda (b)
		(cond ((eq b (ab-root-buffer))
		       t)
		      (t
		       (with-current-buffer b
			 (affiliated-buffers-mode)))))
	      (ab-descendents)))
    ))


(provide 'affiliated-buffers)
;;; affiliated-buffers.el ends here
