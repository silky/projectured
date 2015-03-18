;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(def macro nth-selection (selection index)
  (if (= index 0)
      selection
      `(as (nthcdr ,index (va ,selection)))))

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

;;; Generic slots behavior resolver for document, make-function, macro definitions
(def function preprocess-sql/document-slots (slots)
  (assert (>= 1 (count-if (lambda (slot) (getf (rest slot) :body)) slots)))
  (iter (for slot in slots)
        (with macro-sql-body-slot = (awhen (first (find-if (lambda (slot) (getf (rest slot) :body)) slots))
                                           (list :body it)))
        (for slot-name = (first slot))
        (for type = (getf (rest slot) :type))
        (for body = (getf (rest slot) :body))
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
        (unless body
          (collect slot-name
             into macro-document-definition-slots))
        (finally (return (values document-definition-slots
                                 make-document-function-definition-slots
                                 make-document-instance-parameter-slots
                                 macro-document-definition-slots
                                 macro-sql-body-slot)))))

(def definer sql/document (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'sql/base) (member 'sql/base supers))
                     supers
                     (append supers '(sql/base))))
         ;; names
         (document-name (format-symbol *package* "SQL/~A" name))
         (make-document-function-name (format-symbol *package* "MAKE-~A" document-name))
         (macro-document-name document-name)
         ((:values document-definition-slots
                   make-document-function-definition-slots
                   make-document-instance-parameter-slots
                   macro-document-definition-slots
                   macro-sql-body-slot)
          (preprocess-sql/document-slots slots)))
    (declare (ignorable macro-sql-body-slot-name))
    `(progn
       (def document ,document-name ,supers ,document-definition-slots ,@options)
       (def function ,make-document-function-name (,@make-document-function-definition-slots &key selection)
         (make-instance ',document-name ,@make-document-instance-parameter-slots :selection selection))
       (def macro ,macro-document-name ((&key selection) ,@macro-document-definition-slots ,@macro-sql-body-slot)
               `(,',make-document-function-name ,,@make-document-function-definition-slots :selection ,selection)))))

(def function preprocess-projection-template-iomap-slots (input-document-name iomap-options)
  (iter (for slot-name in (rest iomap-options))
        (assert (find-slot input-document-name slot-name))
        (bind ((iomap-slot-name (format-symbol *package* "~A-IOMAP" slot-name)))
          (collect (list iomap-slot-name :type 'sequence)
            into iomap-slot-definitions)
          (appending (list (make-keyword iomap-slot-name)
                           iomap-slot-name)
                     into make-iomap-parameter-slots)
          (collect (list slot-name iomap-slot-name 'sequence)
            into printer-iomap-definer))
        (finally (return (values iomap-slot-definitions
                                 make-iomap-parameter-slots
                                 printer-iomap-definer)))))

(def definer projection-template (input-document-name output-document-name &rest options)
  (bind ((projection-name (format-symbol *package* "~A->~A" input-document-name output-document-name))
         ;; TODO assert for protected domains
         (iomap-options (find-if (lambda (option) (eq :iomap (first option))) options))
         (projection-options (find-if (lambda (option) (eq :projection (first option))) options))
         (projection-contents (rest projection-options))
         (printer-options (find-if (lambda (option) (eq :printer (first option))) options))
         (iomap-name (format-symbol *package* "IOMAP/~A" projection-name))
         (make-projection-function-name (format-symbol *package* "MAKE-PROJECTION/~A" projection-name))
         (macro-projection-name projection-name)
         ((:values iomap-slot-definitions
                   make-iomap-parameter-slots
                   printer-iomap-definer)
          (preprocess-projection-template-iomap-slots input-document-name iomap-options)))
    (declare (ignorable projection-options
                        make-iomap-parameter-slots
                        printer-iomap-definer))
    `(progn
       (def projection ,projection-name ()
         ())
       (def iomap ,iomap-name () ,iomap-slot-definitions)
       #+nil,(when iomap-options
              `(def iomap ,iomap-name () ,iomap-slot-definitions))
       (def function ,make-projection-function-name ()
         (make-projection ',projection-name))
       (def macro ,macro-projection-name ()
         `(,',make-projection-function-name))
       ;; printer
       ,(when (and printer-options projection-options)
              (bind ((forward-mapper-function-name (format-symbol *package* "FORWARD-MAPPER/~A" projection-name)))
                `(def printer ,projection-name (projection recursion input input-reference)
                   (bind (#+nil(self input)
                          ,@(iter (for iomap in printer-iomap-definer)
                                  (for slot-name = (first iomap))
                                  (for iomap-name = (second iomap))
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
                          (output
                           (as ,(first (labels ((recurse (instance)
                                                (etypecase instance
                                                  ((or number string symbol pathname function sb-sys:system-area-pointer)
                                                   instance)
                                                  #+nil(document/sequence
                                                           (make-document/sequence (iter (for element :in-sequence instance)
                                                                                         (collect (recurse element)))
                                                                                   :selection (recurse (selection-of instance))))
                                                  (sequence
                                                   (coerce (iter (for element :in-sequence instance)
                                                                 (collect (recurse element)))
                                                           (type-of instance)))
                                                  #+nil(template-slot
                                                        (output-of (recurse-printer recursion (slot-value input (name-of instance)) nil)))
                                                  (standard-object
                                                   (bind ((class (class-of instance))
                                                          (copy (allocate-instance class))
                                                          (slots (class-slots class)))
                                                     (iter (for slot :in slots)
                                                           (when (slot-boundp-using-class class instance slot)
                                                             (setf (slot-value-using-class class copy slot) (recurse (slot-value-using-class class instance slot)))))
                                                     copy)))))
                                       (recurse projection-contents))))))
                     ;; iomap confusion
                     (make-iomap ',iomap-name
                                 :projection projection
                                 :recursion recursion
                                 :input input
                                 :input-reference input-reference
                                 :output output ;; TODO ?!
                                 ,@make-iomap-parameter-slots)))))
       )))




#|
;;; uncomment for redefine the basic implementation

#+nil(def projection-template sql/select tree/node
  (:iomap columns tables)
  (:projection (free/text "SELECT") (tree/node (:separator " ,") (columns-of self)))
  (:printer :default))

(def projection-template sql/select tree/node
  (:iomap columns tables)
  (:projection (tree/node (:selection output-selection :separator (text/text () (text/string " ")))
                 (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                   (text/text (:selection (as (nthcdr 3 (va output-selection))))
                     (text/string "SELECT" :font-color *color/solarized/blue*)))
                 (make-tree/node (as (mapcar 'output-of (va columns-iomap)))
                                 :separator (text/text () (text/string ", " :font-color *color/solarized/gray*))
                                 :selection (as (nthcdr 2 (va output-selection))))
                 (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                   (text/text (:selection (as (nthcdr 3 (va output-selection))))
                     (text/string "FROM" :font-color *color/solarized/blue*)))
                 (make-tree/node (as (mapcar 'output-of (va tables-iomap)))
                                 :separator (text/text () (text/string ", " :font-color *color/solarized/gray*))
                                 :selection (as (nthcdr 2 (va output-selection))))))
  (:printer :default))

#+nil(def projection-template sql/column-reference tree/leaf
  (:projection (reference/text (name-of (target-of self))))
  (:printer :default))

(def projection-template sql/column-reference tree/leaf
  (:projection (tree/leaf (:selection output-selection)
                 (text/make-default-text (name-of (target-of input)) "enter column name" :font-color *color/solarized/content/darker* :selection (as (nthcdr 1 (va output-selection))))))
  (:printer :default))

;; 

|#

#+nil(def definer sql/statement (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'sql/base) (member 'sql/base supers))
                     supers
                     (append supers '(sql/base))))
         ;; TODO options structure
         
         ;; names
         (document-name (format-symbol *package* "SQL/~A" name))
         (make-document-function-name (format-symbol *package* "MAKE-~A" document-name))
         (macro-document-name document-name)
         (tree-projection-name (format-symbol *package* "~A->TREE/~A" document-name tree-projection?))
         (iomap-name (format-symbol *package* "IOMAP/~A" tree-projection-name))
         (make-tree-projection-function-name (format-symbol *package* "MAKE-PROJECTION/~A" tree-projection-name))
         (macro-tree-projection-name tree-projection-name)
         
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