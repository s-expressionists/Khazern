(cl:in-package #:khazern-extension)

(defclass accum-region () ())

(defclass accum-clause (khazern::binding-clause khazern::parallel-superclause accum-region)
  ())

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:body-region) (keyword (eql :accum)) &key)
  (let ((instance (make-instance 'accum-clause :start khazern::*start*)))
    (setf (khazern::subclauses instance)
          (khazern::parse-conjunctive-clauses client instance)
          (khazern::end instance) khazern::*index*)
    instance))

(defclass accum-subclause (khazern:clause)
  ((%accum-var :reader accum-var
               :initarg :accum-var)))

(defmethod khazern::map-variables progn (function (clause accum-subclause))
  (khazern:map-variables function (accum-var clause)))

(defun parse-accum-subclause (ref &rest args)
  (let ((var (apply #'khazern::parse-into args)))
    (khazern:parse-token :keywords '(:using))
    (setf (khazern::accumulation-references var)
          (list ref (khazern:parse-token :type 'khazern:simple-var)))
    (make-instance 'accum-subclause
                   :start khazern:*start*
                   :accum-var var
                   :end khazern:*index*)))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :collect)) &key)
  (parse-accum-subclause :collect :accumulation-category :list))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :collecting)) &key)
  (parse-accum-subclause :collect :accumulation-category :list))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :nconc)) &key)
  (parse-accum-subclause :nconc :accumulation-category :list))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :nconcing)) &key)
  (parse-accum-subclause :nconc :accumulation-category :list))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :append)) &key)
  (parse-accum-subclause :append :accumulation-category :list))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :appending)) &key)
  (parse-accum-subclause :append :accumulation-category :list))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :adjoin)) &key)
  (parse-accum-subclause :adjoin :accumulation-category :list))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :adjoining)) &key)
  (parse-accum-subclause :adjoin :accumulation-category :list))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :maximize)) &key)
  (parse-accum-subclause :max :accumulation-category :extremum :default-type-spec 'real))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :maximizing)) &key)
  (parse-accum-subclause :max :accumulation-category :extremum :default-type-spec 'real))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :minimize)) &key)
  (parse-accum-subclause :min :accumulation-category :extremum :default-type-spec 'real))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :minimizing)) &key)
  (parse-accum-subclause :min :accumulation-category :extremum :default-type-spec 'real))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :count)) &key)
  (parse-accum-subclause :count :accumulation-category :summation :default-type-spec 'fixnum))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :counting)) &key)
  (parse-accum-subclause :count :accumulation-category :summation :default-type-spec 'fixnum))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :sum)) &key)
  (parse-accum-subclause :count :accumulation-category :summation :default-type-spec 'number))

(defmethod khazern:parse-clause
    ((client extension-client) (region accum-region) (keyword (eql :summing)) &key)
  (parse-accum-subclause :count :accumulation-category :summation :default-type-spec 'number))
