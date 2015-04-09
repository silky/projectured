;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; SQL->TREE->TEXT

;;;;;;
;;; Primitive projection

(def sql/projection sql/query-block ()
  (tree/node (:separator (text/text () (text/string " ")))
    (printer-output (select-clause-of self) recursion)
    (printer-output (from-clause-of self) recursion)))

(def sql/projection sql/select-clause ()
  (tree/node (:separator (text/text () (text/string " ")))
    (tree/leaf ()
      (sql/keyword () :keyword "SELECT"))
    (make-tree/node (as (mapcar 'output-of (va column-references-iomap)))
                    :separator (text/text () (text/string ", " :font-color *color/solarized/gray*))
                    :selection (as (nthcdr 2 (va output-selection))))))

(def sql/projection sql/from-clause ()
  (tree/node (:separator (text/text () (text/string " ")))
    (tree/leaf ()
      (sql/keyword () :keyword "FROM"))
    (make-tree/node (as (mapcar 'output-of (va table-references-iomap)))
                    :separator (text/text () (text/string ", " :font-color *color/solarized/gray*))
                    :selection (as (nthcdr 2 (va output-selection))))))

(def sql/projection sql/column-reference ()
  (tree/leaf ()
    self)
  (text/text ()
    (text/string (expression-of self))))

(def sql/projection sql/table-reference ()
  (tree/leaf ()
    self)
  (text/text ()
    (text/string (expression-of self))))

(def sql/projection sql/keyword ()
  (text/text ()
    (text/string (keyword-of self))))

;;;;;;
;;; Compound projection

;; TODO collect primitive sql projection and make compounds

(def function make-projection/sql->tree ()
  (type-dispatching
    (sql/query-block (make-projection/sql/query-block->tree/node))
    (sql/select-clause (make-projection/sql/select-clause->tree/node))
    (sql/column-reference (make-projection/sql/column-reference->tree/leaf))
    (sql/from-clause (make-projection/sql/from-clause->tree/node))
    (sql/table-reference (make-projection/sql/table-reference->tree/leaf))))

(def macro sql->tree ()
  '(make-projection/sql->tree))

(def function make-projection/sql->text ()
  (type-dispatching
    (sql/keyword (make-projection/sql/keyword->text/text))
    (sql/column-reference (make-projection/sql/column-reference->text/text))
    (sql/table-reference (make-projection/sql/table-reference->text/text))))

(def macro sql->text ()
  '(make-projection/sql->text))

;;;;;;
;;; Projection

(def projection sql?/column-reference->tree/leaf ()
  ())

(def projection sql?/select-clause->tree/node ()
  ())

;;;;;;
;;; IO map

(def iomap iomap/sql?/select-clause->tree/node ()
  ((column-references-iomap :type sequence)))

;;;;;;
;;; Construction

(def function make-projection/sql?/column-reference->tree/leaf ()
  (make-projection 'sql?/column-reference->tree/leaf))

(def function make-projection/sql?/select-clause->tree/node ()
  (make-projection 'sql?/select-clause->tree/node))

;;;;;;
;;; Construction

(def macro sql?/column-reference->tree/leaf ()
  '(make-projection/sql?/column-reference->tree/leaf))

(def macro sql?/select->tree/node ()
  '(make-projection/sql?/select-clause->tree/node))

;;;;;;
;;; Forward mapper

(def function forward-mapper/sql?/column-reference->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sql/column-reference document))
       '((the tree/leaf document)))
      (((the string (expression-of (the sql/column-reference document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the tree/leaf (printer-output (the sql/column-reference document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/sql?/select-clause->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sql/select-clause document))
       '((the tree/node document)))
      (((the sequence (column-references-of (the sql/select-clause document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((column-iomap (elt (column-references-iomap-of printer-iomap) ?index))
              (column-output (output-of column-iomap)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the tree/node (elt (the sequence document) 1))
                   (the sequence (children-of (the tree/node document)))
                   (the ,(form-type column-output) (elt (the sequence document) ,?index)))
                 ?rest
                 column-iomap)))
      (((the tree/node (printer-output (the sql/select-clause document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/sql?/column-reference->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the tree/leaf document))
       '((the sql/column-reference document)))
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (string= (expression-of printer-input) "")
           (append `((the tree/leaf (printer-output (the sql/column-reference document) ,projection ,recursion))) reference)
           `((the string (expression-of (the sql/column-reference document)))
             (the string (subseq (the string document) ,?start-index ,?end-index)))))
      (?a
       (append `((the tree/leaf (printer-output (the sql/column-reference document) ,projection ,recursion))) reference)))))

(def function backward-mapper/sql?/select-clause->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node document))
       '((the sql/select-clause document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) 1))
        (the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) ?index))
        . ?rest)
       (bind ((column-iomap (elt (column-references-iomap-of printer-iomap) ?index))
              (column-input (input-of column-iomap)))
         (values `((the sequence (column-references-of (the sql/select-clause document)))
                   (the ,(form-type column-input) (elt (the sequence document) ,?index)))
                 ?rest
                 column-iomap)))
      (?a
       (append `((the tree/node (printer-output (the sql/select-clause document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer sql?/column-reference->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/sql?/column-reference->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection)
                       (text/make-default-text (expression-of input) "enter column name" :font-color *color/solarized/content/darker* :selection (as (nthcdr 1 (va output-selection))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer sql?/select-clause->tree/node (projection recursion input input-reference)
  (bind ((column-references-iomap (as (iter (for column-index :from 0)
                                  (for column :in-sequence (column-references-of input))
                                  (collect (recurse-printer recursion column
                                                            `((elt (the sequence document) ,column-index)
                                                              (the sequence (column-references-of (the sql/select-clause document)))
                                                              ,@(typed-reference (form-type input) input-reference)))))))
         (output-selection (as (print-selection (make-iomap 'iomap/sql?/select-clause->tree/node
                                                            :projection projection :recursion recursion
                                                            :input input :input-reference input-reference
                                                            :column-references-iomap column-references-iomap)
                                                (selection-of input)
                                                'forward-mapper/sql?/select-clause->tree/node)))
         (output (as (tree/node (:selection output-selection :separator (text/text () (text/string " ")))
                       (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                         (text/text (:selection (as (nthcdr 3 (va output-selection))))
                           (text/string "SELECT" :font-color *color/solarized/blue*)))
                       (make-tree/node (as (mapcar 'output-of (va column-references-iomap)))
                                       :separator (text/text () (text/string ", " :font-color *color/solarized/gray*))
                                       :selection (as (nthcdr 2 (va output-selection))))
                       (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                         (text/text (:selection (as (nthcdr 3 (va output-selection))))
                           (text/string "FROM" :font-color *color/solarized/blue*)))))))
    (make-iomap 'iomap/sql?/select-clause->tree/node
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output
                :column-references-iomap column-references-iomap)))

;;;;;;
;;; Reader

(def reader sql?/column-reference->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (expression-of (the sql/column-reference document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))
                                  (((the tree/leaf (printer-output (the sql/column-reference document) ?projection ?recursion)) . ?rest)
                                   (make-operation/sequence/replace-range printer-input
                                                                          '((the string (expression-of (the sql/column-reference document)))
                                                                            (the string (subseq (the string document) 0 0)))
                                                                          (replacement-of operation)))))))))
    (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/sql?/column-reference->tree/leaf operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader sql?/select-clause->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/sql?/select-clause->tree/node 'backward-mapper/sql?/select-clause->tree/node)
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press #\,)
                       :domain "SQL" :description "Inserts a new column into the columns of the SQL select"
                       :operation (bind ((index (length (column-references-of printer-input))))
                                    (make-operation/sequence/replace-range printer-input
                                                                           `((the sequence (column-references-of (the sql/select-clause document)))
                                                                             (the sequence (subseq (the sequence document) ,index ,index)))
                                                                           (list (sql/column-reference (:selection '((the string (expression-of (the sql/column-reference document)))
                                                                                                                     (the string (subseq (the string document) 0 0))))
                                                                                                       :expression ""))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/sql?/select-clause->tree/node nil)
                    (make-command/nothing (gesture-of input)))))
