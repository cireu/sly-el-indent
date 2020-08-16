;;; sly-el-indent.el --- Use sly-cl-indent to indent Elisp  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/cireu/sly-el-indent
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-and-compile
  (or (require 'sly-cl-indent nil t)
      (require 'sly-cl-indent "lib/sly-cl-indent")))

(eval-when-compile (require 'cl))

(defun sly-el-indent-ht-map (fun table)
  "Map FUN over TABLE, return a list of results.

Sign: (All (K V T) (-> (-> K V T) (HashTable K V) (Listof T)))"
  (let (result)
    (maphash (lambda (k v)
               (push (funcall fun k v) result))
             table)
    result))

(defun sly-el-indent-common-lisp-get-indentation-hack (name &optional full)
  "Retrieves the indentation information for NAME.

Like `sly--lisp-indent-get-indentation' but try to search property
`lisp-indent-function' of NAME when in emacs-lisp-mode."
  (let ((method
         (or
          ;; From style
          (when sly-common-lisp-style
            (gethash name (sly--lisp-indent-active-style-methods)))
          ;; From global settings.
          (get name 'sly-common-lisp-indent-function)
          (get name 'common-lisp-indent-function)
          ;; Emacs Lisp specificaiton
          (when (derived-mode-p 'emacs-lisp-mode)
            (pcase (get name 'lisp-indent-function)
              (`defun (sly--lisp-indent-get-indentation 'defun))
              ((and it (pred functionp))
               ;; HACK: Indent function using `lisp-indent-function'
               ;; only accept 2 params, the indent-point and state.
               ;; So we need to wrap old function with lambda.
               ;; But we cannot return a function directly, so we create
               ;; a gensym and assign the wrapped function
               ;; to the function slot of gensym.
               (let ((f (make-symbol "indent-func")))
                 (fset f (lambda (_ state indent-point &rest _rest)
                           (funcall it indent-point state)))
                 f))
              (some some)))
          ;; From system derived information.
          (let ((system-info (gethash name sly-common-lisp-system-indentation)))
            (if (not (cdr system-info))
                (caar system-info)
              (let ((guess nil)
                    (guess-n 0)
                    (package (sly--lisp-indent-symbol-package full)))
                (cl-dolist (info system-info guess)
                  (let* ((pkgs (cdr info))
                         (n (length pkgs)))
                    (cond ((member package pkgs)
                           ;; This is it.
                           (cl-return (car info)))
                          ((> n guess-n)
                           ;; If we can't find the real thing, go with the one
                           ;; accessible in most packages.
                           (setf guess (car info)
                                 guess-n n)))))))))))
    (if (eq 'as (car-safe method))
        (sly--lisp-indent-get-indentation (cadr method))
      method)))

(advice-add #'sly--lisp-indent-get-indentation  :override
            #'sly-el-indent-common-lisp-get-indentation-hack)

(defun sly-el-indent-common-lisp-indent-loop-macro-1 (parse-state indent-point)
  "Like `sly--lisp-indent-loop-macro-1' but handle `cl-loop'."
  (catch 'return-indentation
    (save-excursion
      ;; Find first clause of loop macro, and use it to establish
      ;; base column for indentation
      (goto-char (sly--lisp-indent-parse-state-start parse-state))
      (let ((loop-start-column (current-column)))
        (when (looking-at (rx "(cl-loop"))
          (forward-word 1))
        (sly--lisp-indent-loop-advance-past-keyword-on-line)

        (when (eolp)
          (forward-line 1)
          (end-of-line)
          ;; If indenting first line after "(loop <newline>"
          ;; cop out ...
          (if (<= indent-point (point))
              (throw 'return-indentation
                (+ loop-start-column
                   sly-lisp-loop-clauses-indentation)))
          (back-to-indentation))

        (let* ((case-fold-search t)
               (loop-macro-first-clause (point))
               (previous-expression-start
                (sly--lisp-indent-parse-state-prev parse-state))
               (default-value (current-column))
               (loop-body-p nil)
               (loop-body-indentation nil)
               (indented-clause-indentation (+ 2 default-value)))
          ;; Determine context of this loop clause, starting with the
          ;; expression immediately preceding the line we're trying to indent
          (goto-char previous-expression-start)

          ;; Handle a body-introducing-clause which ends a line specially.
          (if (looking-at sly--common-lisp-body-introducing-loop-macro-keyword)
              (let ((keyword-position (current-column)))
                (setq loop-body-p t)
                (setq loop-body-indentation
                      (if (sly--lisp-indent-loop-advance-past-keyword-on-line)
                          (current-column)
                        (back-to-indentation)
                        (if (/= (current-column) keyword-position)
                            (+ 2 (current-column))
                          (+ sly-lisp-loop-body-forms-indentation
                             (if sly-lisp-loop-indent-body-forms-relative-to-loop-start
                                 loop-start-column
                               keyword-position))))))

            (back-to-indentation)
            (if (< (point) loop-macro-first-clause)
                (goto-char loop-macro-first-clause))
            ;; If there's an "and" or "else," advance over it.
            ;; If it is alone on the line, the next "cond" will treat it
            ;; as if there were a "when" and indent under it ...
            (let ((exit nil))
              (while (and (null exit)
                          (looking-at sly--common-lisp-prefix-loop-macro-keyword))
                (if (null (sly--lisp-indent-loop-advance-past-keyword-on-line))
                    (progn (setq exit t)
                           (back-to-indentation)))))

            ;; Found start of loop clause preceding the one we're
            ;; trying to indent. Glean context ...
            (cond
              ((looking-at "(")
               ;; We're in the middle of a clause body ...
               (setq loop-body-p t)
               (setq loop-body-indentation (current-column)))
              ((looking-at sly--common-lisp-body-introducing-loop-macro-keyword)
               (setq loop-body-p t)
               ;; Know there's something else on the line (or would
               ;; have been caught above)
               (sly--lisp-indent-loop-advance-past-keyword-on-line)
               (setq loop-body-indentation (current-column)))
              (t
               (setq loop-body-p nil)
               (if (or (looking-at sly--common-lisp-indenting-loop-macro-keyword)
                       (looking-at sly--common-lisp-prefix-loop-macro-keyword))
                   (setq default-value (+ 2 (current-column))))
               (setq indented-clause-indentation (+ 2 (current-column)))
               ;; We still need loop-body-indentation for "syntax errors" ...
               (goto-char previous-expression-start)
               (setq loop-body-indentation (current-column)))))

          ;; Go to first non-blank character of the line we're trying
          ;; to indent. (if none, wind up poised on the new-line ...)
          (goto-char indent-point)
          (back-to-indentation)
          (cond
            ((looking-at "(")
             ;; Clause body ...
             loop-body-indentation)
            ((or (eolp) (looking-at ";"))
             ;; Blank line.  If body-p, indent as body, else indent as
             ;; vanilla clause.
             (if loop-body-p
                 loop-body-indentation
               (or (and (looking-at ";") (sly--lisp-indent-trailing-comment))
                   default-value)))
            ((looking-at sly--common-lisp-indent-indented-loop-macro-keyword)
             indented-clause-indentation)
            ((looking-at sly--common-lisp-indent-clause-joining-loop-macro-keyword)
             (let ((stolen-indent-column nil))
               (forward-line -1)
               (while (and (null stolen-indent-column)
                           (> (point) loop-macro-first-clause))
                 (back-to-indentation)
                 (if (and (< (current-column) loop-body-indentation)
                          (looking-at "\\(#?:\\)?\\sw"))
                     (progn
                       (if (looking-at sly--lisp-indent-loop-macro-else-keyword)
                           (sly--lisp-indent-loop-advance-past-keyword-on-line))
                       (setq stolen-indent-column (current-column)))
                   (forward-line -1)))
               (or stolen-indent-column default-value)))
            (t default-value)))))))

(defun sly-el-indent-cl-loop (&rest args)
  "Indent `cl-loop' properly"
  (cl-letf (((symbol-function #'sly--lisp-indent-loop-macro-1)
             #'sly-el-indent-common-lisp-indent-loop-macro-1))
           (apply #'sly--lisp-indent-loop args)))

(defun sly-el-indent-make-spec ()
  (let ((result (make-hash-table :test #'eq)))
    (dolist (fun '(
                   ;; Copy from `cl'
                   remprop
                   getf
                   tailp
                   list-length
                   nreconc
                   revappend
                   concatenate
                   subseq
                   random-state-p
                   make-random-state
                   signum
                   isqrt
                   lcm
                   gcd
                   notevery
                   notany
                   every
                   some
                   mapcon
                   mapl
                   maplist
                   map
                   equalp
                   coerce
                   tree-equal
                   nsublis
                   sublis
                   nsubst-if-not
                   nsubst-if
                   nsubst
                   subst-if-not
                   subst-if
                   subsetp
                   nset-exclusive-or
                   set-exclusive-or
                   nset-difference
                   set-difference
                   nintersection
                   intersection
                   nunion
                   union
                   rassoc-if-not
                   rassoc-if
                   assoc-if-not
                   assoc-if
                   member-if-not
                   member-if
                   merge
                   stable-sort
                   search
                   mismatch
                   count-if-not
                   count-if
                   count
                   position-if-not
                   position-if
                   position
                   find-if-not
                   find-if
                   find
                   nsubstitute-if-not
                   nsubstitute-if
                   nsubstitute
                   substitute-if-not
                   substitute-if
                   substitute
                   delete-duplicates
                   remove-duplicates
                   delete-if-not
                   delete-if
                   remove-if-not
                   remove-if
                   replace
                   fill
                   reduce
                   compiler-macroexpand
                   define-compiler-macro
                   assert
                   check-type
                   typep
                   deftype
                   defstruct
                   letf*
                   letf
                   rotatef
                   shiftf
                   remf
                   psetf
                   the
                   locally
                   multiple-value-setq
                   multiple-value-bind
                   symbol-macrolet
                   macrolet
                   progv
                   psetq
                   do-all-symbols
                   do-symbols
                   do*
                   do
                   loop
                   return-from
                   return
                   block
                   etypecase
                   typecase
                   ecase
                   case
                   load-time-value
                   eval-when
                   destructuring-bind
                   gentemp
                   pairlis
                   acons
                   subst
                   adjoin
                   copy-list
                   ldiff
                   list*
                   tenth
                   ninth
                   eighth
                   seventh
                   sixth
                   fifth
                   fourth
                   third
                   endp
                   rest
                   second
                   first
                   svref
                   copy-seq
                   evenp
                   oddp
                   minusp
                   plusp
                   floatp-safe
                   declaim
                   proclaim
                   nth-value
                   multiple-value-call
                   multiple-value-apply
                   multiple-value-list
                   values-list
                   values
                   pushnew
                   decf
                   incf
                   flet
                   flet*
                   labels
                   tagbody
                   ;; `defmethod' and `defgeneric' should follow CL convention.
                   defmethod
                   defgeneric))
      (let ((new (intern (format "cl-%s" fun))))
        (puthash new `(as ,fun) result)))
    ;; `if' in Emacs Lisp accepts rest parameters to be the else-form
    (puthash 'if 2 result)
    ;; `with-output-to-string' should indent like `progn' in Elisp.
    (puthash 'with-output-to-string '(as progn) result)
    ;; `lisp-indent-loop' cannot recognize `cl-loop' properly
    (puthash 'cl-loop 'sly-el-indent-cl-loop result)
    ;; Elisp specific cl extensions
    (dolist (sym '(letf letf* cl-letf cl-letf*))
      (puthash sym '(as let) result))
    (dolist (sym '(callf cl-callf))
      (puthash sym 2 result))
    (dolist (sym '(callf2 cl-callf2))
      (puthash sym 3 result))
    (dolist (sym '(defun* cl-defun cl-defmacro defmacro*
                   defsubst cl-defsubst defsubst*))
      (puthash sym '(as defun) result))
    (sly-el-indent-ht-map #'list result)))

;; Create Style
(sly--lisp-indent-add-style "emacs-lisp" "basic"
                       '((lisp-lambda-list-keyword-alignment t))
                       (sly-el-indent-make-spec)
                       nil
                       "\
Emacs Lisp specific indent rules.

Indentation for subroutines in `cl-lib' will use corresponding
subroutines in CL.

The style also enable `lisp-lambda-list-keyword-alignment' for better indent
style.")

;;;###autoload
(defun sly-el-indent-setup ()
  "Enable style in current buffer."
  (interactive)
  (setq-local lisp-indent-function #'common-lisp-indent-function)
  (sly-common-lisp-set-style "emacs-lisp"))

(provide 'sly-el-indent)

;; Local Variables:
;; coding: utf-8
;; End:
