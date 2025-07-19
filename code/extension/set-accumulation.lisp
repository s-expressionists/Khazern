(cl:in-package #:khazern-extension)

(defconstant +default-vector-size+ 16)

(defun parse-vector-type (type)
  (flet ((normalize-type (head)
           (if (or (null head)
                   (eq (car head) '*))
               t
               (car head)))
         (normalize-length (head)
           (if (or (null head)
                   (eq (car head) '*))
               +default-vector-size+
               (car head))))
  (etypecase type
    (symbol
      (ecase type
        (vector
         (values t +default-vector-size+))
        (bit-vector
         (values 'bit +default-vector-size+))
        (string
         (values 'character +default-vector-size+))
        (base-string
         (values 'base-char +default-vector-size+))))
    (cons
     (ecase (first type)
       (array
        (when (and (cddr type)
                   (cdr (third type)))
          (error "Cannot accumulate to a multidimensional array."))
        (values (normalize-type (cdr type))
                (normalize-length (caddr type))))
       (vector
        (values (normalize-type (cdr type))
                (normalize-length (cddr type))))
       (bit-vector
        (values 'bit
                (normalize-length (cdr type))))
       (string
        (values 'character
                (normalize-length (cdr type))))
       (base-string
        (values 'base-char
                (normalize-length (cdr type)))))))))

(defclass vector-accumulation-scope (khazern::sequence-accumulation-scope) ())

(defmethod khazern::make-accumulation-scope
    ((client extension-client) name type (category (eql :sequence)) references)
  (declare (ignore references))
  (cond ((subtypep type 'list)
         (call-next-method))
        ((subtypep type 'vector)
         (multiple-value-bind (element-type length)
             (parse-vector-type type)
           (let ((instance (make-instance 'vector-accumulation-scope)))
             (setf (values (khazern::accum-ref instance) (khazern::accum-var instance))
                   (khazern:add-simple-binding instance
                                               :var name
                                               :type type
                                               :form `(make-array ,length
                                                                  :fill-pointer 0
                                                                  :adjustable t
                                                                  :element-type ',element-type)
                                               :accumulation-category category))
             instance)))))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :collect))
     &key)
    (khazern:parse-accumulation client
                      (make-instance 'khazern::accumulation-clause :reference :collect)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:collect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :collecting))
     &key)
  (khazern:parse-accumulation client
                      (make-instance 'khazern::accumulation-clause :reference :collect)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:collect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :append))
     &key)
  (khazern:parse-accumulation client
                      (make-instance 'khazern::accumulation-clause :reference :append)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:collect nil :append nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :appending))
     &key)
  (khazern:parse-accumulation client
                      (make-instance 'khazern::accumulation-clause :reference :append)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:collect nil :append nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nconc))
     &key)
  (khazern:parse-accumulation client
                      (make-instance 'khazern::accumulation-clause :reference :nconc)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:collect nil :nconc nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nconcing))
     &key)
  (khazern:parse-accumulation client
                      (make-instance 'khazern::accumulation-clause :reference :nconc)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:collect nil :nconc nil)))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance vector-accumulation-scope) (reference (eql :collect))
     name)
  (let ((var (khazern:var-spec (khazern::accum-var instance))))
    (values `((,name (value)
                (vector-push-extend value ,var)))
            `((inline ,name)))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance vector-accumulation-scope) (reference (eql :append))
     name)
  (let ((collect-name (khazern::accumulation-scope-reference instance :collect)))
      (values `((,name (value)
                  (map nil #',collect-name value)))
              `((inline ,name)))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance vector-accumulation-scope) (reference (eql :nconc))
     name)
  (let ((collect-name (khazern::accumulation-scope-reference instance :collect)))
      (values `((,name (value)
                  (map nil #',collect-name value)))
              `((inline ,name)))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance khazern::list-accumulation-scope)
     (reference (eql :append)) name)
  (let ((list-name (gensym (symbol-name reference)))
        (collect-name (khazern::accumulation-scope-reference instance :collect)))
    (multiple-value-bind (definitions declarations)
        (call-next-method client instance reference list-name)
      (values (list* `(,name (value)
                        (cond ((consp value)
                               (,list-name value))
                              (value
                               (map nil #',collect-name value))))
                     definitions)
              (list* `(inline ,name)
                     declarations)))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance khazern::list-accumulation-scope)
     (reference (eql :nconc)) name)
  (let ((list-name (gensym (symbol-name reference)))
        (collect-name (khazern::accumulation-scope-reference instance :collect)))
    (multiple-value-bind (definitions declarations)
        (call-next-method client instance reference list-name)
      (values (list* `(,name (value)
                        (cond ((consp value)
                               (,list-name value))
                              (value
                               (map nil #',collect-name value))))
                     definitions)
              (list* `(inline ,name)
                     declarations)))))

(defclass set-accumulation-clause (khazern::accumulation-clause) ())

(defmethod khazern:preposition-names
    ((client extension-client) (clause set-accumulation-clause))
  (values '(:key :test)
          '()
          '()))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance set-accumulation-clause) name)
  (setf (args instance) (nconc (args instance) (list name (khazern:parse-token)))))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :adjoin)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :adjoin)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:adjoin nil)))
 
(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :adjoining)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :adjoin)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:adjoin nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :union)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :union)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:adjoin nil :union nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :unioning)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :union)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:adjoin nil :union nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nunion)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :nunion)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:adjoin nil :nunion nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :nunioning)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :nunion)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:adjoin nil :nunion nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :disjoin)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :disjoin)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:disjoin nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :disjoining)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :disjoin)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:disjoin nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :intersection))
     &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :intersect)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:intersect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :intersecting)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :intersect)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:intersect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nintersection))
     &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :nintersect)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:nintersect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :nintersecting)) &key)
  (khazern:parse-accumulation client
                      (make-instance 'set-accumulation-clause :reference :nintersect)
                      :default-type-spec 'list
                      :parse-type-spec t
                      :accumulation-category :sequence
                      :accumulation-references '(:nintersect nil)))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance khazern::list-accumulation-scope)
     (reference (eql :adjoin)) name)
  (let ((head (khazern::accumulation-scope-reference instance :head))
        (tail (khazern::accumulation-scope-reference instance :tail)))
    (values `((,name (value &rest args)
                (unless (apply #'member value (cdr ,head) args)
                  (rplacd ,tail
                          (setq ,tail (cons value nil))))))
            `((inline ,name)))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance vector-accumulation-scope)
     (reference (eql :adjoin)) name)
  (let ((var (khazern::accum-ref instance)))
    (values `((,name (value &rest args)
                (unless (apply #'find value ,var args)
                  (vector-push-extend value ,var))))
            `((inline ,name)))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance khazern::sequence-accumulation-scope)
     (reference (eql :union)) name)
  (let ((adjoin-name (khazern::accumulation-scope-reference instance :adjoin)))
    (values `((,name (value &rest args)
                (map nil (lambda (item)
                           (apply #',adjoin-name item args))
                     value)))
            `((inline ,name)))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance khazern::sequence-accumulation-scope)
     (reference (eql :nunion)) name)
  (let ((adjoin-name (khazern::accumulation-scope-reference instance :adjoin)))
    (values `((,name (value &rest args)
                (map nil (lambda (item)
                           (apply #',adjoin-name item args))
                     value)))
            `((inline ,name)))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance khazern::list-accumulation-scope)
     (reference (eql :nunion)) name)
  (let ((head (khazern::accumulation-scope-reference instance :head))
        (tail (khazern::accumulation-scope-reference instance :tail))
        (seq-name (gensym (symbol-name reference))))
    (multiple-value-bind (definitions declarations)
        (call-next-method client instance reference seq-name)
      (values (list* `(,name (value &rest args)
                        (cond ((consp value)
                               (tagbody
                                next
                                  (when value
                                    (cond ((apply #'member (car value) (cdr ,head) args)
                                           (setq value (cdr value)))
                                          (t
                                           (rplacd ,tail
                                                   (setq ,tail value))
                                           (setq value (cdr value))
                                           (rplacd ,tail nil)))
                                    (go next))))
                              (value
                               (apply #',seq-name value args))))
                     definitions)
              declarations))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance khazern::list-accumulation-scope)
     (reference (eql :disjoin)) name)
  (let ((head (khazern::accumulation-scope-reference instance :head))
        (tail (khazern::accumulation-scope-reference instance :tail)))
    (values `((,name (value &rest args)
                (rplacd ,head
                        (apply #'delete value (cdr ,head) args))
                       (setq ,tail (last ,head))))
            `((inline ,name)))))
