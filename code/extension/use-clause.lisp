(cl:in-package #:khazern-extension)

(defclass use-region () ())

(defclass use-clause (khazern::binding-clause khazern::parallel-superclause use-region)
  ())

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:body-region) (keyword (eql :use)) &key)
  (let ((instance (make-instance 'use-clause :start khazern::*start*)))
    (setf (khazern::subclauses instance)
          (khazern::parse-conjunctive-clauses client instance)
          (khazern::end instance) khazern::*index*)
    instance))

(defclass use-subclause (khazern:clause)
  ((%accum-var :reader accum-var
               :initarg :accum-var)))

(defmethod khazern::map-variables progn (function (clause use-subclause))
  (khazern:map-variables function (accum-var clause)))

(defun parse-use-subclause (ref &rest args)
  (let ((var (apply #'khazern::parse-into args)))
    (khazern:parse-token :keywords '(:via))
    (setf (khazern::accumulation-references var)
          (list ref (khazern:parse-token :type 'khazern:simple-var)))
    (make-instance 'use-subclause
                   :start khazern:*start*
                   :accum-var var
                   :end khazern:*index*)))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :collect)) &key)
  (parse-use-subclause :collect :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :collecting)) &key)
  (parse-use-subclause :collect :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :nconc)) &key)
  (parse-use-subclause :nconc :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :nconcing)) &key)
  (parse-use-subclause :nconc :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :append)) &key)
  (parse-use-subclause :append :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :appending)) &key)
  (parse-use-subclause :append :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :adjoin)) &key)
  (parse-use-subclause :adjoin :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :adjoining)) &key)
  (parse-use-subclause :adjoin :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :union)) &key)
  (parse-use-subclause :union :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :unioning)) &key)
  (parse-use-subclause :union :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :disjoin)) &key)
  (parse-use-subclause :disjoin :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :disjoining)) &key)
  (parse-use-subclause :disjoin :accumulation-category :sequence))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :maximize)) &key)
  (parse-use-subclause :max :accumulation-category :extremum :default-type-spec 'real))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :maximizing)) &key)
  (parse-use-subclause :max :accumulation-category :extremum :default-type-spec 'real))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :minimize)) &key)
  (parse-use-subclause :min :accumulation-category :extremum :default-type-spec 'real))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :minimizing)) &key)
  (parse-use-subclause :min :accumulation-category :extremum :default-type-spec 'real))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :count)) &key)
  (parse-use-subclause :count :accumulation-category :summation :default-type-spec 'fixnum))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :counting)) &key)
  (parse-use-subclause :count :accumulation-category :summation :default-type-spec 'fixnum))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :sum)) &key)
  (parse-use-subclause :count :accumulation-category :summation :default-type-spec 'number))

(defmethod khazern:parse-clause
    ((client extension-client) (region use-region) (keyword (eql :summing)) &key)
  (parse-use-subclause :count :accumulation-category :summation :default-type-spec 'number))
