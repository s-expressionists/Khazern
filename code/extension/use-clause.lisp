(cl:in-package #:khazern-extension)

(defclass use-region () ())

(defclass use-clause (khazern::binding-clause khazern::parallel-superclause use-region)
  ())

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:body-region) (keyword (eql :use)) &key)
  (let ((instance (make-instance 'use-clause :start khazern::*start*)))
    (setf (khazern::subclauses instance)
          (khazern::parse-conjunctive-clauses client instance
                                              :name (prog1
                                                        (khazern:parse-token :type'khazern:simple-var)
                                                      (khazern:parse-token :keywords '(:=))))
          (khazern::end instance) khazern::*index*)
    instance))

(defclass use-subclause (khazern:clause)
  ((%accum-var :reader accum-var
               :initarg :accum-var)))

(defmethod khazern::map-variables progn (function (clause use-subclause))
  (khazern:map-variables function (accum-var clause)))

(defun parse-use-subclause (ref name &rest args)
  (let ((var (apply #'khazern::parse-into args)))
    (setf (khazern::accumulation-references var)
          (list ref name))
    (make-instance 'use-subclause
                   :start khazern:*start*
                   :accum-var var
                   :end khazern:*index*)))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :collect)) &key name)
  (parse-use-subclause :collect name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :collecting)) &key name)
  (parse-use-subclause :collect name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :nconc)) &key name)
  (parse-use-subclause :nconc name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :nconcing)) &key name)
  (parse-use-subclause :nconc name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :append)) &key name)
  (parse-use-subclause :append name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :appending)) &key name)
  (parse-use-subclause :append name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :adjoin)) &key name)
  (parse-use-subclause :adjoin name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :adjoining)) &key name)
  (parse-use-subclause :adjoin name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :union)) &key name)
  (parse-use-subclause :union name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :unioning)) &key name)
  (parse-use-subclause :union name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :disjoin)) &key name)
  (parse-use-subclause :disjoin name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :disjoining)) &key name)
  (parse-use-subclause :disjoin name
                       :accumulation-category :sequence
                       :default-type-spec 'list
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :maximize)) &key name)
  (parse-use-subclause :max name
                       :accumulation-category :extremum
                       :default-type-spec 'real
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :maximizing)) &key name)
  (parse-use-subclause :max name
                       :accumulation-category :extremum
                       :default-type-spec 'real
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :minimize)) &key name)
  (parse-use-subclause :min name
                       :accumulation-category :extremum
                       :default-type-spec 'real
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :minimizing)) &key name)
  (parse-use-subclause :min name
                       :accumulation-category :extremum
                       :default-type-spec 'real
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :count)) &key name)
  (parse-use-subclause :count name
                       :accumulation-category :summation
                       :default-type-spec 'fixnum
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :counting)) &key name)
  (parse-use-subclause :count name
                       :accumulation-category :summation
                       :default-type-spec 'fixnum
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :sum)) &key name)
  (parse-use-subclause :count name
                       :accumulation-category :summation
                       :default-type-spec 'number
                       :parse-type-spec t))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :summing)) &key name)
  (parse-use-subclause :count name
                       :accumulation-category :summation
                       :default-type-spec 'number
                       :parse-type-spec t))
