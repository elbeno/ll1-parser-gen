;;; Copyright 2012 Ben Deane

;;; This file is part of the common lisp package com.elbeno.ll1-parser-gen.

;;; The package is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; The package is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with the package.  If not, see <http://www.gnu.org/licenses/>.

;;;; lexer.lisp

(in-package #:com.elbeno.ll1-parser-gen)

;; does a string contain a character?
(defun containsp (s c)
  (find c s :test #'eql))

;; split a string into a list of strings
;; by a set of delimiters
(defun split-string-acc (acc n str delims)
  (cond ((eql n (length str))
	 (if (eql n 0)
	     acc
	     (push str acc)))
	((containsp delims (char str n))
	 (if (eql n 0)
	     (split-string-acc acc 0 (subseq str (1+ n)) delims)
	     (split-string-acc (push (subseq str 0 n) acc) 0 (subseq str (1+ n)) delims)))
	(t (split-string-acc acc (1+ n) str delims))))

(defun split-string (str &optional (delims " "))
  (nreverse (split-string-acc () 0 str delims)))

;; lookup a token
;; and convert the value to a canonical one
(defun token-lookup (val table)
  (multiple-value-bind (tok tokp) (gethash val table)
    (if (null tokp)
        (values nil val)
        (if (null (cdr tok))
            (values (car tok) val)
            (values (car tok) (cadr tok))))))

;; hash the token values
(defun add-tokens-to-hashtable (ht tokt)
  (if (null tokt)
      ht
      (progn
        (setf (gethash (caar tokt) ht) (cdar tokt))
        (add-tokens-to-hashtable ht (cdr tokt)))))

(defun make-token-hashtable (tokt)
  (let ((ht (make-hash-table :test #'equalp)))
    (add-tokens-to-hashtable ht tokt)))

;; make a simple lexer closure
;; for lists
(defun make-lexer (lexeme-list token-table)
  (let ((ht (make-token-hashtable token-table)))
    #'(lambda ()
        (let ((val (pop lexeme-list)))
          (if (null val)
              (values '$ nil)
              (token-lookup val ht))))))

;; for strings
(defun make-string-lexer (str token-table)
  (make-lexer (split-string str) token-table))

;; consume the lexemes
(defun next-token (lexer)
  (funcall lexer))
