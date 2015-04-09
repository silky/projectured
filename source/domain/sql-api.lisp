;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(def logger sql-projection ())

(def document sql/base ()
  ())

(def macro nth-selection (selection index)
  (if (<= index 1)
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

(def function preprocess-sql/statement-slots (slots)
  (iter (for slot in slots)
        (for slot-name = (first slot))
        (for type = (getf (rest slot) :type))
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
                             #+nil(list 'll slot-name)
                             slot-name))
                   into make-document-instance-parameter-slots)
        ;; macro-document-definition-slots
        (collect slot-name
          into macro-document-definition-slots)
        ;; macro-make-document-function-call-slots
        (appending (list (make-keyword slot-name)
                         slot-name)
                   into macro-make-document-function-call-slots)
        (finally (return (values document-definition-slots
                                 make-document-function-definition-slots
                                 make-document-instance-parameter-slots
                                 macro-document-definition-slots
                                 macro-make-document-function-call-slots)))))

(def definer sql/statement (name supers slots &rest options)
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
                   macro-make-document-function-call-slots)
          (preprocess-sql/statement-slots slots)))
    `(progn
       (def document ,document-name ,supers ,document-definition-slots ,@options)
       (def function ,make-document-function-name (&key ,@make-document-function-definition-slots selection)
         (make-instance ',document-name ,@make-document-instance-parameter-slots :selection selection))
       (def macro ,macro-document-name ((&key selection) &key ,@macro-document-definition-slots)
         `(,',make-document-function-name ,,@macro-make-document-function-call-slots :selection ,selection)))))

(def function preprocess-sql/projection-slots (input-document-name)
  (sql-projection.debug "preprocess-sql/projection-slots - ~A" input-document-name)
  (iter (for slot in (class-direct-slots (find-class input-document-name)))
        (for slot-definition-type = (slot-definition-type slot))
        (for slot-name = (slot-definition-name slot))
        (sql-projection.debug "~A - ~A - ~A" slot slot-name slot-definition-type)
        (when (eq slot-definition-type 'sequence)
          (bind ((iomap-slot-name (format-symbol *package* "~A-IOMAP" slot-name)))
            (collect (list iomap-slot-name :type 'sequence)
              into iomap-slot-definitions)
            (appending (list (make-keyword iomap-slot-name)
                             iomap-slot-name)
                       into make-iomap-parameter-slots)
            (collect (list slot-name iomap-slot-name 'sequence)
              into printer-iomap-definer)))
        (when iomap-slot-definitions
          (sql-projection.debug "~A" iomap-slot-definitions)
          (sql-projection.debug "~A" make-iomap-parameter-slots)
          (sql-projection.debug "~A" printer-iomap-definer))
        (finally (return (values iomap-slot-definitions
                                 make-iomap-parameter-slots
                                 printer-iomap-definer)))))

(def definer sql/projection (input-document-name (&rest options) &rest projections)
  (sql-projection.debug "sql/projection - ~A - ~A" input-document-name options)
  (iter (for projection :in projections)
        (for output-document-name = (first projection)) ;; KLUDGE macroexpansion?
        (sql-projection.debug "~A" projection)
        (sql-projection.debug "~A" output-document-name)
        (appending
         (bind ((projection-name (format-symbol *package* "~A->~A" input-document-name output-document-name))
                ;; TODO assert for protected domains
                (iomap-name (format-symbol *package* "IOMAP/~A" projection-name))
                (make-projection-function-name (format-symbol *package* "MAKE-PROJECTION/~A" projection-name))
                (macro-projection-name projection-name)
                (projection-contents projection) ;; currently are same
                (forward-mapper-function-name (format-symbol *package* "FORWARD-MAPPER/~A" projection-name)) ;; should be simplified
                (backward-mapper-function-name (format-symbol *package* "BACKWARD-MAPPER/~A" projection-name)) ;; should be simplified
                ((:values iomap-slot-definitions
                          make-iomap-parameter-slots
                          printer-iomap-definer)
                 (preprocess-sql/projection-slots input-document-name)))
           (declare (ignorable make-iomap-parameter-slots
                               printer-iomap-definer))
           `((def projection ,projection-name ()
               ())
             (def iomap ,iomap-name () ,iomap-slot-definitions)
             (def function ,make-projection-function-name ()
               (make-projection ',projection-name))
             (def macro ,macro-projection-name ()
               `(,',make-projection-function-name))
             (def function ,forward-mapper-function-name (printer-iomap reference)
               (declare (ignorable printer-iomap reference))
               (sql-projection.debug "FORWARD-MAPPER ~A" ',projection-name))
             (def function ,backward-mapper-function-name (printer-iomap reference)
               (declare (ignorable printer-iomap reference))
               (sql-projection.debug "BACKWARD-MAPPER ~A" ',projection-name))
             (def printer ,projection-name (projection recursion input input-reference)
               (declare (ignorable projection recursion input input-reference))
               (sql-projection.debug "PRINTER ~A" ',projection-name)
               (bind ((self input))
                 (declare (ignorable self))
                 (bind (,@(iter (for iomap in printer-iomap-definer)
                                (for slot-name = (first iomap))
                                (for iomap-name = (second iomap))
                                (collect `(,iomap-name (as (iter (for index :from 0)
                                                                 (for instance :in-sequence (slot-value input ',slot-name))
                                                                 (collect (recurse-printer recursion instance
                                                                                           `((elt (the sequence document) ,index)
                                                                                             (the sequence (slot-value (the ,',input-document-name document) ,',slot-name))
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
                         (as ,(labels ((recurse (instance level)
                                                (etypecase instance
                                                  ((or symbol number string pathname function sb-sys:system-area-pointer)
                                                   (progn
                                                     (sql-projection.debug "KEEP-AS ~A" instance)
                                                     instance))
                                                  #+nil(document/sequence
                                                           (make-document/sequence (iter (for element :in-sequence instance)
                                                                                         (collect (recurse element)))
                                                                                   :selection (recurse (selection-of instance))))
                                                  (sequence
                                                   (progn
                                                     (sql-projection.debug "SEQUENCE ~A" instance)
                                                     (coerce (iter (for element :in-sequence instance)
                                                                   (for index :from 0)
                                                                   (with is-document? =
                                                                         (awhen (find-class (first instance) nil)
                                                                           ;; TODO selection is exists in the class definition and handled by macro
                                                                           ;; document is not work - for example text/string macro has no selection parameter
                                                                           #+nil(subtypep it (find-class 'document))
                                                                           (or (subtypep it (find-class 'tree/base))
                                                                               (subtypep it (find-class 'text/text)))))
                                                                   #+nil(with document-parameters =
                                                                              (when is-document?
                                                                                (second instance)))
                                                                   #+nil(with document-parameters-selection =
                                                                              (if (getf document-parameters :selection)
                                                                                  document-parameters
                                                                                  (append (list :selection 'output-selection) document-parameters )))
                                                                   (collect (cond
                                                                              ((and is-document?
                                                                                    (= 1 index)
                                                                                    (not (getf element :selection)))
                                                                               (recurse (append (list :selection `(nth-selection output-selection ,level)) element) level))
                                                                              ((and is-document?
                                                                                    (< 1 index))
                                                                               (recurse element (+ 1 level)))
                                                                              (t (recurse element level)))))
                                                             (type-of instance))))
                                                  #+nil(template-slot
                                                        (output-of (recurse-printer recursion (slot-value input (name-of instance)) nil)))
                                                  (standard-object
                                                   (progn
                                                     (sql-projection.debug "OBJECT ~A" instance)
                                                     (bind ((class (class-of instance))
                                                            (copy (allocate-instance class))
                                                            (slots (class-slots class)))
                                                       (iter (for slot :in slots)
                                                             (when (slot-boundp-using-class class instance slot)
                                                               (setf (slot-value-using-class class copy slot) (recurse (slot-value-using-class class instance slot) level))))
                                                       copy))))))
                                      (sql-projection.debug "~A" projection-contents)
                                      (recurse projection-contents 1)))))
                   ;; iomap confusion
                   (make-iomap ',iomap-name
                               :projection projection
                               :recursion recursion
                               :input input
                               :input-reference input-reference
                               :output output ;; TODO ?!
                               ,@make-iomap-parameter-slots))))
             (def reader ,projection-name (projection recursion input printer-iomap)
               (declare (ignorable projection recursion input printer-iomap))
               #+nil(sql-projection.debug "READER ~A" ',projection-name)
               (bind ((self input))
                 (declare (ignorable self))
                 (merge-commands (command/read-backward recursion input printer-iomap ',backward-mapper-function-name nil)
                                 (make-command/nothing (gesture-of input)))))
             ))
         into result)
        (finally (return (cons 'progn result)))))

