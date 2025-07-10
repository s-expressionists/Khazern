(cl:in-package #:khazern-extension)

(defclass adjoin-clause (khazern::accumulation-clause)
  ((%args :accessor args
          :initform nil)))

(defun parse-adjoin (client)
  (let ((instance (make-instance 'adjoin-clause
                 :start khazern:*start*
                 :form (khazern:parse-token)
                 :accum-var (khazern:parse-into :accumulation-category :list)
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

(defmethod khazern:body-forms ((clause adjoin-clause))
  (let ((head (khazern::accumulation-reference (khazern::var-spec (khazern::accum-var clause))
                                               :head))
        (tail (khazern::accumulation-reference (khazern::var-spec (khazern::accum-var clause))
                                               :tail)))
    `((rplacd ,head
              (adjoin ,(khazern::it-form (khazern::form clause)) (cdr ,head) ,@(args clause)))
      (when (eq ,head ,tail)
        (setq ,tail (cdr ,head))))))
