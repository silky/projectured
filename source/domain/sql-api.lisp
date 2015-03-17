;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;; Find a correct place somewhere else
(pushnew :iomap hu.dwim.defclass-star:*allowed-slot-definition-properties*)

(def macro nth-selection (selection index)
  `(as (nthcdr ,index (va ,selection))))

(def macro 1st-selection (selection)
  `(as (nthcdr 1 (va ,selection))))

(def macro 2nd-selection (selection)
  `(as (nthcdr 2 (va ,selection))))

(def macro 3rd-selection (selection)
  `(as (nthcdr 3 (va ,selection))))

(def macro 4th-selection (selection)
  `(as (nthcdr 4 (va ,selection))))

(def macro 5th-selection (selection)
  `(as (nthcdr 5 (va ,selection))))



;;; Generic document, make-function, macro for sql/statement
(def function preprocess-sql/statement-slots (slots)
  (iter (for slot in slots)
        (for slot-name = (first slot))
        (for type = (getf (rest slot) :type))
        (for iomap = (getf (rest slot) :iomap))
        (for sql-body = (getf (rest slot) :sql-body))
        ;; document-definition-slots - ((a) (b))
        ;; - ensure type default sql/base
        ;; TODO remove special keywords
        (collect (if (getf (rest slot) :type)
                     slot
                     (append slot (list :type 'sql/base)))
          into document-definition-slots)
        ;; make-document-function-definition-slots (a b)
        (collect slot-name
          into make-document-function-definition-slots)
        ;; make-instance-parameter-slots (:a a :b b)
        ;; - ensure type sequence lazy-list
        (appending (list (make-keyword slot-name)
                         (if (eq type 'sequence)
                             (list 'll (append '(ensure-list) (list slot-name)))
                             slot-name))
                   into make-document-instance-parameter-slots)
        ;; macro-document-definition-slots
        ;; TODO :body &body kezelése?
        (unless sql-body
          (collect slot-name
             into macro-document-definition-slots))
        ;; iomap-slots
        (when iomap
          (bind ((iomap-slot-name (format-symbol *package* "~A-IOMAP" slot-name)))
            (collect (list iomap-slot-name :type 'sequence)
              into iomap-slot-definitions)
            (appending (list (make-keyword iomap-slot-name)
                             iomap-slot-name)
              into make-iomap-parameter-slots)
            (collect (list slot-name iomap-slot-name 'sequence)
              into printer-iomap-definer)))
        (finally (return (values document-definition-slots
                                 make-document-function-definition-slots
                                 make-document-instance-parameter-slots
                                 macro-document-definition-slots
                                 iomap-slot-definitions
                                 make-iomap-parameter-slots
                                 printer-iomap-definer)))))

(def function preporcess-sql/statements-find-option (options option-keyword)
  ;; TODO multiple occurance assert
  (find-if (lambda (option) (eq option-keyword (first option))) options))

(def definer sql/statement (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'sql/base) (member 'sql/base supers))
                     supers
                     (append supers '(sql/base))))
         ;; TODO options structure
         (tree-projection-options (preporcess-sql/statements-find-option options :tree-projection))
         (printer-options (preporcess-sql/statements-find-option options :printer))
         (reader-options (preporcess-sql/statements-find-option options :reader))
         (tree-projection? (getf tree-projection-options :tree-projection))
         (tree-projection-content? (getf tree-projection-options :content))
         (tree-projection-content-type (first tree-projection-content?))
         (tree-projection-contents (rest tree-projection-content?))
         (printer? (getf printer-options :printer))
         (reader? (getf reader-options :reader))
         ;; names
         (document-name (format-symbol *package* "SQL/~A" name))
         (make-document-function-name (format-symbol *package* "MAKE-~A" document-name))
         (macro-document-name document-name)
         (tree-projection-name (format-symbol *package* "~A->TREE/~A" document-name tree-projection?))
         (iomap-name (format-symbol *package* "IOMAP/~A" tree-projection-name))
         (make-tree-projection-function-name (format-symbol *package* "MAKE-PROJECTION/~A" tree-projection-name))
         (macro-tree-projection-name tree-projection-name)
         (tree-projection-printer-name tree-projection-name)
         (tree-projection-reader-name tree-projection-name)
         (forward-mapper-function-name (format-symbol *package* "FORWARD-MAPPER/~A" tree-projection-name))
         (backward-mapper-function-name (format-symbol *package* "BACKWARD-MAPPER/~A" tree-projection-name))
         ;; KLUDGE multiple occurance?!
         (macro-sql-body-slot-name
          (first (find-if (lambda (slot) (getf (rest slot) :sql-body)) slots)))
         ((:values document-definition-slots
                   make-document-function-definition-slots
                   make-document-instance-parameter-slots
                   macro-document-definition-slots
                   iomap-slot-definitions
                   make-iomap-parameter-slots
                   printer-iomap-definer)
          (preprocess-sql/statement-slots slots)))
    (declare (ignorable reader?
                        tree-projection-reader-name
                        forward-mapper-function-name
                        backward-mapper-function-name))
    `(progn
       (def document ,document-name ,supers ,document-definition-slots ,@options)
       (def function ,make-document-function-name (,@make-document-function-definition-slots &key selection)
         (make-instance ',document-name ,@make-document-instance-parameter-slots :selection selection))
       ,(if macro-sql-body-slot-name
            `(def macro ,macro-document-name ((&key selection) ,@macro-document-definition-slots &body ,macro-sql-body-slot-name)
               `(,',make-document-function-name ,,@make-document-function-definition-slots :selection ,selection))
            `(def macro ,macro-document-name ((&key selection) ,@macro-document-definition-slots)
               `(,',make-document-function-name ,,@make-document-function-definition-slots :selection ,selection)))
       (def projection ,tree-projection-name ()
         ())
       (def iomap ,iomap-name () ,iomap-slot-definitions)
       (def function ,make-tree-projection-function-name ()
         (make-projection ',tree-projection-name))
       (def macro ,macro-tree-projection-name ()
         `(,',make-tree-projection-function-name))
       ;; TODO default
       ,(when printer?
              `(def printer ,tree-projection-printer-name (projection recursion input input-reference)
                 (bind ((self input)
                        ,@(iter (for iomap in printer-iomap-definer)
                                (for slot-name = (first iomap))
                                (for iomap-name = (second iomap))
                                (for slot-type = (third iomap))
                                (collect `(,iomap-name (as (iter (for index :from 0)
                                                                 (for instance :in-sequence (slot-value input ',slot-name))
                                                                 (collect (recurse-printer recursion instance
                                                                                           `((elt (the sequence document) ,index)
                                                                                             (the sequence (slot-value (the sql/select document) ,',slot-name))
                                                                                             ,@(typed-reference (form-type input) input-reference)))))))))
                        (output-selection (as (print-selection (make-iomap ',iomap-name
                                                                           :projection projection
                                                                           :recursion recursion
                                                                           :input input
                                                                           :input-reference input-reference
                                                                           ;; :output TODO ?!
                                                                           ,@make-iomap-parameter-slots)
                                                               (selection-of input)
                                                               ',forward-mapper-function-name)))
                        ,(when tree-projection?
                               ;; TODO
                               ;; keyword helyett lehetne valami frappánsabb is
                               ;; node-leaf automatikus kezelése
                               ;; selector szintjének automatikus kezelése
                               ;; emaitt free-text van a select-re keyword-text helyett
                               (bind ((top-tree-document (if (eq tree-projection-content-type :tree-leaf)
                                                             'tree/leaf
                                                             'tree/node)))
                                 `(output (as (,top-tree-document
                                               (:selection output-selection)
                                               ,@(iter (for tree-projection-content in tree-projection-contents)
                                                       (for content-type = (first tree-projection-content))
                                                       (for content = (second tree-projection-content))
                                                       (collect
                                                           (ecase content-type
                                                             (:default-text
                                                              `(text/make-default-text ,content
                                                                                       ,(format nil "<~A>" document-name)
                                                                                       :font-color *color/solarized/content/darker*
                                                                                       :selection (1st-selection output-selection)))
                                                             (:keyword-text
                                                              `(text/make-simple-text ,content
                                                                                      :font-color *color/solarized/blue*
                                                                                      :selection (1st-selection output-selection)))
                                                             (:free-text
                                                              `(tree/leaf (:selection (2nd-selection output-selection))
                                                                 (text/text (:selection (3rd-selection output-selection))
                                                                   (text/string ,content :font-color *color/solarized/blue*))))
                                                             (:tree-node
                                                              (bind ((slot-definition (find-if (lambda (slot) (eq content (first slot))) slots))
                                                                     (slot-iomap-definition (find-if (lambda (slot) (eq content (first slot))) printer-iomap-definer))
                                                                     (slot-type (getf (rest slot-definition) :type))
                                                                     (iomap-name (second slot-iomap-definition)))
                                                                (if (eq slot-type 'sequence)
                                                                    `(make-tree/node (as (mapcar 'output-of (va ,iomap-name)))
                                                                                     :separator (text/text () (text/string ", " :font-color *color/solarized/gray*))
                                                                                     :selection (2nd-selection output-selection))
                                                                    `(tree/leaf ()
                                                                       (text/make-simple-text "TODO"
                                                                                              :font-color *color/solarized/red*
                                                                                              :selection (1st-selection output-selection)))))))))))))))
                   (declare (ignorable self))
                   ;; iomap confusion
                   (make-iomap ',iomap-name
                               :projection projection
                               :recursion recursion
                               :input input
                               :input-reference input-reference
                               :output output ;; TODO ?!
                               ,@make-iomap-parameter-slots)))))))


;;;;;;
;;; Complex version

#|
;; a nem kikommentezettekkel felül lehet csapni az alapimplementációt

#+nil(def sql/statement keyword ()
  ((keyword :type string))
  (:tree-projection :leaf :content (:keyword-text (keyword-of self)))
  (:printer :default))

(def sql/statement select ()
  ((columns :type sequence :iomap sequence)
   (tables :type sequence :iomap sequence))
  (:tree-projection :node :content (:tree-node (:free-text "SELECT") (:tree-node columns))) ;; TODO (colums-of self) kellene, de így egyszerűbb az iomap előtalálása
  (:printer :default))

#+nil(def sql/statement column ()
  ((name :type string)
   (type :type string :accessor nil))
  (:tree-projection :node))

(def sql/statement column-reference ()
  ((target :type sql/column))
  (:tree-projection :leaf :content (:tree-leaf (:default-text (name-of (target-of self)))))
  (:printer :default)
  (:reader :default))

#+nil(def sql/statement table-reference ()
  ((target :type sql/table))
  (:tree-projection :leaf :content ((:default-text (name-of (target-of self)))))
  (:printer :default))

#+nil(def function make-test-document/sql ()
  (sql/column-reference ()
    (sql/column () "name" "varchar")))

|#
