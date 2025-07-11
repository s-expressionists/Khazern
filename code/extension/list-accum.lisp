(cl:in-package #:khazern-extension)

(defclass adjoin-clause (khazern::accumulation-clause)
  ((%args :accessor args
          :initform nil)))

(defun parse-adjoin (client)
  (let ((instance (make-instance 'adjoin-clause
                 :start khazern:*start*
                 :form (khazern:parse-token)
                 :accum-var (khazern:parse-into :accumulation-category :list
                                                :accumulation-references '(:adjoin nil))
                 :end khazern:*index*)))
    (khazern:parse-prepositions client instance)
    instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance adjoin-clause) name)
  (setf (args instance) (nconc (args instance) (list name (khazern:parse-token)))))
  
(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :adjoin)) &key)
  (parse-adjoin client))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :adjoining)) &key)
  (parse-adjoin client))

(defmethod khazern:preposition-names ((client extension-client) (clause adjoin-clause))
  (values '(:key :test)
          '()
          '()))

(defmethod khazern::accumulation-scope-functions
    ((client extension-client) name type (category (eql :list)) (reference (eql :adjoin)) symbol
     &key head tail &allow-other-keys)
  `((,symbol (value &rest args)
      (rplacd ,head
              (apply #'adjoin value (cdr ,head) args))
      (when (eq ,head ,tail)
        (setq ,tail (cdr ,head))))))

(defmethod khazern:body-forms ((clause adjoin-clause))
   `((,(khazern::accumulation-reference  (khazern::var-spec (khazern::accum-var clause)) :adjoin)
     ,(khazern::it-form (khazern::form clause)))))
