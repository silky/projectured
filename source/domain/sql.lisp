;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; SQL domain

;;;;;;
;;; Types of SQL Statements

;;;;;;
;;; DDL statement

;;;;;;
;;; SELECT statement

(def sql/statement keyword ()
  ((keyword :type string)))

(def sql/statement column-reference ()
  ((expression :type string)))

(def sql/statement hint ()
  ((hint :type string)))

(def sql/statement select-clause ()
  ((hint-clause)
   (selection-mode :type sql/keyword)
   (column-references :type sequence)))

(def sql/statement table-reference ()
  ((expression :type string)))

(def sql/statement from-clause ()
  ((table-references :type sequence)))

(def sql/statement query-block ()
  (;; TODO most of them not yet supported
   (select-clause :type sql/select-clause)
   (from-clause :type sql/from-clause)
   (where-clause)
   (hierarchical-query-clause)
   (group-by-clause)
   (having-clause)
   (model-clause)
   ))

(def sql/statement subquery ()
  ((set-operation)
   (query-block :type sql/query-block)
   (subqueries :type sequence)
   ;; TODO ( subquery ) ?
   (order-by-clause)))

(def sql/statement select-statement ()
  ((subquery-factoring-clauses)
   (subquery)
   (for-update-clause)))

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

