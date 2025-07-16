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

(defclass vector-accumulation-scope (khazern::accumulation-scope) ())

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
  (khazern::parse-accumulation-clause client :sequence :collect 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :collecting))
     &key)
  (khazern::parse-accumulation-clause client :sequence :collect 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :append))
     &key)
  (khazern::parse-accumulation-clause client :sequence :append 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :appending))
     &key)
  (khazern::parse-accumulation-clause client :sequence :append 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nconc))
     &key)
  (khazern::parse-accumulation-clause client :sequence :nconc 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nconcing))
     &key)
  (khazern::parse-accumulation-clause client :sequence :nconc 'list))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) instance name type (category (eql :sequence)) (reference (eql :collect))
     symbol
     &key tail &allow-other-keys)
  (cond ((subtypep type 'list)
         (call-next-method))
        (t
         (values `((,symbol (value)
                     (vector-push-extend value ,name)))
                 `((inline ,symbol))))))

(defclass set-accumulation-clause (khazern::accumulation-clause)
  ((%args :accessor args
          :initform nil)))

(defmethod khazern:preposition-names
    ((client extension-client) (clause set-accumulation-clause))
  (values '(:key :test)
          '()
          '()))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance set-accumulation-clause) name)
  (setf (args instance) (nconc (args instance) (list name (khazern:parse-token)))))
  
(defun parse-set-accumulation (client reference)
  (let ((instance
          (make-instance 'set-accumulation-clause
                         :start khazern:*start*
                         :reference reference
                         :form (khazern:parse-token)
                         :accum-var (khazern:parse-into
                                     :accumulation-category :sequence
                                     :accumulation-references (list reference nil))
                         :end khazern:*index*)))
    (khazern:parse-prepositions client instance)
    instance))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :adjoin)) &key)
  (parse-set-accumulation client :adjoin))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :adjoining)) &key)
  (parse-set-accumulation client :adjoin))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :union)) &key)
  (parse-set-accumulation client :union))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :unioning)) &key)
  (parse-set-accumulation client :union))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nunion)) &key)
  (parse-set-accumulation client :union))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :nunioning)) &key)
  (parse-set-accumulation client :union))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :disjoin)) &key)
  (parse-set-accumulation client :disjoin))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :disjoining)) &key)
  (parse-set-accumulation client :disjoin))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :intersection))
     &key)
  (parse-set-accumulation client :intersect))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :intersecting)) &key)
  (parse-set-accumulation client :intersect))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nintersection))
     &key)
  (parse-set-accumulation client :intersect))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :nintersecting)) &key)
  (parse-set-accumulation client :intersect))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) name type (category (eql :list)) (reference (eql :adjoin)) symbol
     &key head tail &allow-other-keys)
  (values `((,symbol (value &rest args)
              (rplacd ,head
                      (apply #'adjoin value (cdr ,head) args))
                     (when (eq ,head ,tail)
                       (setq ,tail (cdr ,head)))))
          `((inline ,symbol))))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) name type (category (eql :list)) (reference (eql :union)) symbol
     &key head tail &allow-other-keys)
  (values `((,symbol (value &rest args)
              (tagbody
               next
                 (when value
                   (rplacd ,head
                           (apply #'adjoin (car value) (cdr ,head) args))
                   (when (eq ,head ,tail)
                     (setq ,tail (cdr ,head)))
                   (go next)))))
          nil))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) name type (category (eql :list)) (reference (eql :disjoin)) symbol
     &key head tail &allow-other-keys)
  (values `((,symbol (value &rest args)
              (rplacd ,head
                      (apply #'delete value (cdr ,head) args))
              (setq ,tail (last ,head))))
          `((inline ,symbol))))

(defmethod khazern:body-forms ((clause set-accumulation-clause))
  `((,(khazern::accumulation-reference (khazern::var-spec (khazern::accum-var clause))
                                       (khazern::reference clause))
     ,(khazern::it-form (khazern::form clause)) ,@(args clause))))
