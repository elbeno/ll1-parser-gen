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

;;;; parser-gen.lisp

(in-package #:com.elbeno.ll1-parser-gen)

;; symbols start, epsilon and $ are reserved
;; meaning the start symbol, the empty production
;; and the end respectively

;; a grammar is specified as follows:
;; e.g. START -> X
;;      X -> abc
;;      X -> fgh
;;      Y -> ba
;; '((start ((x)))
;;   (x ((a b c) (f g h)))
;;   (y ((b a)))
;;  )

;; some useful helper functions

(defun flatten (l)
  (if (listp l)
      (reduce #'append (mapcar #'flatten l))
      (list l)))

;; is a symbol a terminal?
;; just flatten the whole grammar and remove the non-terminals
;; then test for membership of the remainder
(defun terminalp (symbol grammar)
  (or (eq symbol '$)
      (let* ((nonterms (loop for production in grammar collect (car production)))
             (terms (remove-if #'(lambda (x) (member x nonterms))
                               (flatten (loop for production in grammar collect (cdr production))))))
        (member symbol terms))))

;; get the list of productions for a non-terminal
(defun production-list (symbol grammar)
  (cond ((null grammar)	nil)
        ((eq symbol (caar grammar)) (cadar grammar))
        (t (production-list symbol (cdr grammar)))))

;; e derives e (of course)
;; a symbol derives e if any of its productions do
;; a production derives e if all of its terms do
(defun derives-e (symbol grammar)
  (cond ((null grammar) nil)
        ((eq symbol 'epsilon) '(epsilon))
        ((eq symbol (caar grammar))
         (some #'(lambda (s) (derives-e s grammar)) (cadar grammar)))
        ((listp symbol)
         (every #'(lambda (s) (derives-e s grammar)) symbol)) ; symbol here is a production RHS
        (t (derives-e symbol (cdr grammar)))))

;; contruction of the first sets is easy

;; first set of a terminal x is just (x)
;; first set of a nonterminal is the union of the first sets of its productions
;; first set of a production is the first set of its first element (excluding e)
;; plus the first set of the next element if the first element derives e, and so on
;; plus e iff the last element derives e also
(defun make-first-set (symbol grammar)
  (remove-duplicates
   (cond ((terminalp symbol grammar) (list symbol))
         ((atom symbol)
          (reduce #'append (mapcar #'(lambda (x) (make-first-set x grammar)) (production-list symbol grammar))))
         (t (let ((f (make-first-set (car symbol) grammar))) ; symbol here is a production RHS
              (if (null (cdr symbol))
                  f
                  (if (derives-e (car symbol) grammar)
                      (append (remove-if #'(lambda (x) (eq x 'epsilon)) f) (make-first-set (cdr symbol) grammar))
                      f)))))))

(defun make-first-set-excluding-epsilon (symbol grammar)
  (remove-if #'(lambda (x) (eq x 'epsilon)) (make-first-set symbol grammar)))

;; construction of the follows sets is a little trickier
;; and requires some more helper functions

;; take a list of productions and return any containing the given symbol
(defun get-productions-for (symbol prods)
  (remove-if-not #'(lambda (x) (if (atom x)
                                   (eq x symbol)
                                   (find symbol x))) prods))

;; now do that for the whole grammar
(defun get-productions-containing (symbol grammar)
  (if (null grammar)
      nil
      (let ((prods (get-productions-for symbol (cadar grammar)))
            (rest (get-productions-containing symbol (cdr grammar))))
        (if (null prods)
            rest
            (cons (cons (caar grammar) prods) rest)))))

;; get a symbol following a given one in a single list
(defun get-symbol-following (symbol prod)
  (if (or (atom prod) (null prod) (null (cdr prod)))
      nil
      (if (eq symbol (car prod))
          (cadr prod)
          (get-symbol-following symbol (cdr prod)))))

;; now do that over a list of lists
(defun get-symbols-following (symbol prod)
  (if (null prod)
      nil
      (let ((h (get-symbol-following symbol (cadar prod)))
            (rest (get-symbols-following symbol (cdr prod))))
        (if (null h)
            rest
            (cons h rest)))))

;; find all productions of the form A -> aBb (where B = symbol)
;; and return the union of the first sets of b
;; excluding e
(defun get-first-sets-for-follows (symbol grammar)
  (let ((l (get-symbols-following symbol (get-productions-containing symbol grammar))))
    (remove-if #'(lambda (x) (eq x 'epsilon))
               (reduce #'append (mapcar #'(lambda (x) (make-first-set x grammar)) l)))))

;; return the last element of a production that does not derive e
(defun symbol-at-end (symbol prod grammar)
  (cond ((atom prod) (eq symbol prod))
        ((null prod) nil)
        (t (or (and (eq symbol (car prod))
                    (every #'(lambda (x) (derives-e x grammar)) (cdr prod)))
               (symbol-at-end symbol (cdr prod) grammar)))))

;; take a list of productions for a nonterminal and return the nonterminal
;; if any production contains the given symbol at the end
;; or everything after the symbol derives e
(defun get-head-for (symbol productions grammar)
  (let ((s (some #'(lambda (x) (symbol-at-end symbol x grammar)) (cadr productions))))
    (if (null s)
        nil
        (car productions))))

;; now do that for the whole grammar
(defun get-heads-for (symbol grammar)
  (remove-if #'null (mapcar #'(lambda (x) (get-head-for symbol x grammar)) grammar)))

;; find all productions of the form A -> aB
;; and return the union of the follows sets of A
(defun get-follows-sets-for-follows (symbol grammar)
  (let ((l (get-heads-for symbol grammar)))
    (reduce #'append (mapcar #'(lambda (x) (make-follows-set x grammar)) l))))

;; follows set of START is $
;; follows set of a symbol B in a production A -> aBb is everything in first(b) except e
;; follows set of a symbol B in a production A -> aB is everything in follows(A)
;; this also applies if A->aBb and b ->* e
(defun make-follows-set (symbol grammar)
  (cond ((eq symbol 'start) '($))
        (t (append (get-first-sets-for-follows symbol grammar) (get-follows-sets-for-follows symbol grammar)))))

;; make parse table entries from a first list
(defun make-parse-table-entry (term-in-first productionRHS)
  (if (eq term-in-first 'epsilon)
      nil
      (cons term-in-first (list productionRHS))))

;; make parse table entries for a single production
(defun make-parse-table-entries-for-production (symbol productionRHS grammar)
  (let* ((firsts (make-first-set productionRHS grammar))
         (entries (remove-if #'null (mapcar #'(lambda (x) (make-parse-table-entry x productionRHS)) firsts))))
    (if (find 'epsilon firsts)
        (let ((follows (remove-if-not #'(lambda (x) (terminalp x grammar)) (make-follows-set symbol grammar))))
          (append entries (remove-if #'null (mapcar #'(lambda (x) (make-parse-table-entry x productionRHS)) follows))))
        entries)))

;; make a list of parse table entries for a given nonterminal
(defun make-parse-table-line (symbol grammar)
  (cons symbol (reduce #'append (mapcar #'(lambda (x) (make-parse-table-entries-for-production symbol x grammar)) (production-list symbol grammar)))))

;; finally, we can make the complete parse table
;; which tells us which production to apply from each state for each terminal
(defun make-parse-table (grammar)
  (let ((nonterms (loop for production in grammar collect (car production))))
    (mapcar #'(lambda (x) (make-parse-table-line x grammar)) nonterms)))

;; parsing algorithm
(defun parse (lexer grammar)
  (let ((stack '(start $))
        (parse-table (make-parse-table grammar)))
    (multiple-value-bind (tok val) (next-token lexer)
      (consume stack lexer grammar parse-table tok val))))

(defun match-production (token prodlist)
  (if (null prodlist)
      nil
      (let ((prod (car prodlist)))
        (if (eq token (car prod))
            (cadr prod)
            (match-production token (cdr prodlist))))))

(defun get-parse-table-production (symbol token parse-table)
  (if (null parse-table)
      nil
      (let* ((entry (car parse-table))
             (matchsym (car entry))
             (prodlist (cdr entry)))
        (if (eq matchsym symbol)
            (match-production token prodlist)
            (get-parse-table-production symbol token (cdr parse-table))))))

(defun output-terminal (token value)
  (format t "~a -> ~a ~%" token value))

(defun output-production (token prod)
  (format t "~a -> ~{~a ~} ~%" token prod))

(defun alter-stack (stack prod)
  (append prod (cdr stack)))

(defun consume (stack lexer grammar parse-table token value)
  (if (null stack)
      (format t "Finished: success")
      (if (terminalp (car stack) grammar)
          (progn
            (output-terminal token value)
            (if (eq (car stack) token)
                (multiple-value-bind (tok val) (next-token lexer)
                  (consume (cdr stack) lexer grammar parse-table tok val))
                (format t "Error: unexpected token (expecting terminal) ~a (~a)" token value)))
          (let ((prod (get-parse-table-production (car stack) token parse-table)))
            (if (null prod)
                (format t "Error: unexpected token ~a (~a)" token value)
                (progn
                  (output-production (car stack) prod)
                  (consume (alter-stack stack prod) lexer grammar parse-table token value)))))))
