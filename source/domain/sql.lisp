;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; SQL domain

;;; special slot behavior

;;; :body #t - define in macro as &body
(pushnew :sql-body hu.dwim.defclass-star:*allowed-slot-definition-properties*)

(def document sql/base ()
  ())

(def document sql/text ()
  ())

(def sql/document column ()
  ((name :type string)
   (type :type string :accessor nil)))

(def sql/document column-reference ()
  ((target :type sql/column :sql-body t)))

(def sql/document table ()
  ((name :type string)
   (columns :type sequence :sql-body #t)))

(def sql/document table-reference ()
  ((target :type sql/table :sql-body #t)))

(def sql/document select ()
  ((columns :type sequence)
   (tables :type sequence)))

#+nil(def sql/document select ()
  ((columns :type sequence :body #t)
   (tables :type sequence :body #t)))

;;;;;;
;;; Types of SQL Statements

;;;;;;
;;; DDL statement

;;;;;;
;;; SELECT statement
#|
(def sql/statement select-statement ()
  ((subquery-factoring-clauses)
   (subquery)
   (for-update-clause)))

(def sql/statement subquery ()
  ((query-block)
   ;; TODO multiple subquery and subquery operation
   (subqueries)
   ;; TODO ( subquery ) ?
   (order-by-clause)
   ))

(def sql/statement query-block ()
  ((hint)
   (select-clause)
   (from-clause)
   (where-clause)
   (hierarchical-query-clause)
   (group-by-clause)
   (having-clause)
   (model-clause)))

(def sql/statement subquery-factoring-clause ()
  ((query-name)
   (subquery)))


(def sql/statement select-clause ()
  ())

(def function make-test-document/sql-statement ()
  (sql/select-statement
      ()
      nil
      nil
      nil))
|#
;;;;;;
;;; Levy example

;;;;;;
;;; Document

(def document sqlx/base ()
  ())

(def document sqlx/column (sqlx/base)
  ((name :type string)
   (type :type string :accessor nil)))

(def document sqlx/column-reference (sqlx/base)
  ((target :type sqlx/column)))

(def document sqlx/table (sqlx/base)
  ((name :type string)
   (columns :type sequence)))

(def document sqlx/table-reference (sqlx/base)
  ((target :type sqlx/table)))

(def document sqlx/select (sqlx/base)
  ((columns :type sequence)
   (tables :type sequence)))

;;;;;;
;;; Construction

(def function make-sqlx/column (name type &key selection)
  (make-instance 'sqlx/column :name name :type type :selection selection))

(def function make-sqlx/column-reference (target &key selection)
  (make-instance 'sqlx/column-reference :target target :selection selection))

(def function make-sqlx/table (name columns &key selection)
  (make-instance 'sqlx/table :name name :columns (ll columns) :selection selection))

(def function make-sqlx/table-reference (target &key selection)
  (make-instance 'sqlx/table-reference :target target :selection selection))

(def function make-sqlx/select (columns tables &key selection)
  (make-instance 'sqlx/select :columns (ll columns) :tables (ll tables) :selection selection))

;;;;;;
;;; Construction

(def macro sqlx/column ((&key selection) name type)
  `(make-sqlx/column ,name ,type :selection ,selection))

(def macro sqlx/column-reference ((&key selection) &body target)
  `(make-sqlx/column-reference ,(first target) :selection ,selection))

(def macro sqlx/table ((&key selection) name &body columns)
  `(make-sqlx/table ,name (list-ll ,@columns) :selection ,selection))

(def macro sqlx/table-reference ((&key selection) &body target)
  `(make-sqlx/table-reference ,(first target) :selection ,selection))

(def macro sqlx/select ((&key selection) columns tables)
  `(make-sqlx/select ,columns ,tables :selection ,selection))

