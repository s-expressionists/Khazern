(cl:in-package #:khazern-extension)

(defclass set-accumulation-clause (khazern::accumulation-clause)
  ((%args :accessor args
          :initform nil)))

(defmethod khazern:preposition-names ((client extension-client) (clause set-accumulation-clause))
  (values '(:key :test)
          '()
          '()))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance set-accumulation-clause) name)
  (setf (args instance) (nconc (args instance) (list name (khazern:parse-token)))))
  
(defun parse-set-accumulation (client reference)
  (let ((instance (make-instance 'set-accumulation-clause
                                 :start khazern:*start*
                                 :reference reference
                                 :form (khazern:parse-token)
                                 :accum-var (khazern:parse-into :accumulation-category :list
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
