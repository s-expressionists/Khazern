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
    ((client extension-client) (instance vector-accumulation-scope) (reference (eql :collect))
     name)
  (let ((var (khazern:var-spec (khazern::accum-var instance))))
    (values `((,name (value)
                (vector-push-extend value ,var)))
            `((inline ,name)))))

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

(defmethod khazern::make-accumulation-clause
    ((client extension-client) (category (eql :sequence)) (reference (eql :adjoin))
     &key start end var form)
  (make-instance 'set-accumulation-clause
                 :start start
                 :end end
                 :accum-var var
                 :form form
                 :reference reference))

(defmethod khazern::make-accumulation-clause
    ((client extension-client) (category (eql :sequence)) (reference (eql :union))
     &key start end var form)
  (make-instance 'set-accumulation-clause
                 :start start
                 :end end
                 :accum-var var
                 :form form
                 :reference reference))

(defmethod khazern::make-accumulation-clause
    ((client extension-client) (category (eql :sequence)) (reference (eql :nunion))
     &key start end var form)
  (make-instance 'set-accumulation-clause
                 :start start
                 :end end
                 :accum-var var
                 :form form
                 :reference reference))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :adjoin)) &key)
  (khazern::parse-accumulation-clause client :sequence :adjoin 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :adjoining)) &key)
  (khazern::parse-accumulation-clause client :sequence :adjoin 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :union)) &key)
  (khazern::parse-accumulation-clause client :sequence :union 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :unioning)) &key)
  (khazern::parse-accumulation-clause client :sequence :union 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nunion)) &key)
  (khazern::parse-accumulation-clause client :sequence :nunion 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :nunioning)) &key)
  (khazern::parse-accumulation-clause client :sequence :nunion 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :disjoin)) &key)
  (khazern::parse-accumulation-clause client :sequence :disjoin 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :disjoining)) &key)
  (khazern::parse-accumulation-clause client :sequence :disjoin 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :intersection))
     &key)
  (khazern::parse-accumulation-clause client :sequence :intersect 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :intersecting)) &key)
  (khazern::parse-accumulation-clause client :sequence :intersect 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nintersection))
     &key)
  (khazern::parse-accumulation-clause client :sequence :intersect 'list))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :nintersecting)) &key)
  (khazern::parse-accumulation-clause client :sequence :intersect 'list))

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
    ((client extension-client) (instance khazern::list-accumulation-scope)
     (reference (eql :union)) name)
  (let ((head (khazern::accumulation-scope-reference instance :head))
        (tail (khazern::accumulation-scope-reference instance :tail)))
    (values `((,name (value &rest args)
                (tagbody
                 next
                   (when value
                     (unless (apply #'member (car value) (cdr ,head) args)
                       (rplacd ,tail
                          (setq ,tail (cons (car value) nil))))
                     (setq value (cdr value))
                     (go next)))))
            nil)))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) (instance khazern::list-accumulation-scope)
     (reference (eql :nunion)) name)
  (let ((head (khazern::accumulation-scope-reference instance :head))
        (tail (khazern::accumulation-scope-reference instance :tail)))
    (values `((,name (value &rest args)
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
                     (go next)))))
            nil)))

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

(defmethod khazern:body-forms ((clause set-accumulation-clause))
  `((,(khazern::accumulation-reference (khazern::var-spec (khazern::accum-var clause))
                                       (khazern::reference clause))
     ,(khazern::it-form (khazern::form clause)) ,@(args clause))))
