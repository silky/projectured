;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

#|
SQL

A teljes szabvány lefedése jelenleg túl nagy munka befektetést igényelne.
A szabad szöveges szerkesztés jelenleg nem kiváltható, illetve a kiváltás teljes körűségének hasznossága nem bizonyított.
A SQL dokumentum mixálhatósága más dokumentumokkal a legfontosabb hozadék.

Ezért a fókuszban
SQL domainből csak azokat tanitjuk meg a szerkesztőnek, ami jelenleg hasznosnak ismert.
Elvárás, hogy a domain szinten nem támogatott nyelvi elemek is elérhetőek legyenek
- vegyes sql/text és sql/base (illetve bármi más

|#

;;;;;;
;;; SQL domain 

(def document sql/base ()
  ())

;; minden olyan egyébként az sql domain részét képező szöveges tartalom, amit jelenleg a szerkesztő nem ismer
;; select sum(salary), employee_id from employee group by employee_id
;; - legegyszerűbb formában
;;   (statement (text))
;; - sql statement struktúráját minimálisan már használva
;;   (statement (select-clause (text)) (from-clause (text)) (group-by (text)))
;; - amennyiben az sql domain már ismeri a használt fogalmakat
;;   (statement (select-clause (function (column-reference)) (column-reference)) (from-clause (table-reference)) (group-by (column-reference)))
;; bármilyen köztes állapotban vegyesen szerepelhet sql/domain specifikus dokumentum elem, illetve azzal együttműködő szabad text
;; idővel amúgy is kell parser, ami a text->sql projekciót képes megoldani
;; ugyanakkor a szabad szöveges bevitel helyett elsősorban egyedi gesture alapúnak kell lennie

(def document sql/text ()
  ())

;; Simple veriosn
(def sql/statement column ()
  ((name :type string)
   (type :type string :accessor nil))
  (:tree-projection :node))

(def sql/statement column-reference ()
  ((target :type sql/column))
  (:tree-projection :leaf))

(def sql/statement table ()
  ((name :type string)
   (columns :type sequence :sql-body #t))
  (:tree-projection :node))

(def sql/statement table-reference ()
  ((target :type sql/table))
  (:tree-projection :leaf))

(def sql/statement select ()
  ((columns :type sequence :iomap sequence)
   (tables :type sequence :iomap sequence))
  (:tree-projection :node))


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

