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

(defclass vector-scope (khazern:sequence-scope) ())

#+(or abcl clasp sbcl)
(defclass extensible-sequence-scope (khazern:sequence-scope) ())

(defclass ansi-sequence-scope (khazern:sequence-scope) ())

(defmethod khazern:make-scope
    ((client extension-client) name type (category (eql :sequence)) references)
  (declare (ignore references))
  (khazern:check-subtype type 'sequence)
  (cond ((subtypep type 'list)
         (call-next-method))
        ((and (subtypep type 'vector)
              (not (subtypep type 'simple-vector)))
         (multiple-value-bind (element-type length)
             (parse-vector-type type)
           (let ((instance (make-instance 'vector-scope)))
             (setf (khazern:var instance)
                   (nth-value 1
                              (khazern:add-simple-binding instance
                                                          :var name
                                                          :type type
                                                          :form `(make-array ,length
                                                                             :fill-pointer 0
                                                                             :adjustable t
                                                                             :element-type ',element-type)
                                                          :category category)))
             instance)))
        #+(or abcl clasp sbcl)
        ((ignore-errors (sequence:adjust-sequence (make-sequence type 0) 1))
         (let ((instance (make-instance 'extensible-sequence-scope)))
           (setf (khazern:var instance)
                 (nth-value 1
                            (khazern:add-simple-binding instance
                                                        :var name
                                                        :type type
                                                        :form `(make-sequence ',type 0)
                                                        :category category)))
           instance))
        (t
         (let ((instance (make-instance 'ansi-sequence-scope)))
           (setf (khazern:var instance)
                 (nth-value 1
                            (khazern:add-simple-binding instance
                                                        :var name
                                                        :type type
                                                        :form `(make-sequence ',type 0)
                                                        :category category)))
           instance))))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :collect)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'khazern:accumulation-clause :reference :collect)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:collect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :collecting))
     &key)
  (khazern:parse-accumulation client
                              (make-instance 'khazern:accumulation-clause :reference :collect)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:collect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :append)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'khazern:accumulation-clause :reference :append)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:collect nil :append nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :appending))
     &key)
  (khazern:parse-accumulation client
                              (make-instance 'khazern:accumulation-clause :reference :append)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:collect nil :append nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nconc)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'khazern:accumulation-clause :reference :nconc)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:collect nil :nconc nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nconcing))
     &key)
  (khazern:parse-accumulation client
                              (make-instance 'khazern:accumulation-clause :reference :nconc)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:collect nil :nconc nil)))

(defmethod khazern:scope-functions
    ((client extension-client) (instance vector-scope) (reference (eql :collect))
     name)
  (let ((var (khazern:var-spec (khazern:var instance))))
    (values `((,name (value)
                (vector-push-extend value ,var)))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance ansi-sequence-scope)
     (reference (eql :collect)) name)
  (let ((var (khazern:var-spec (khazern:var instance)))
        (type (khazern:type-spec (khazern:var instance))))
    (values `((,name (value)
                (setq ,var (concatenate ',type ,var (list value)))))
            `((inline ,name)))))

#+(or abcl clasp sbcl)    
(defmethod khazern:scope-functions
    ((client extension-client) (instance extensible-sequence-scope) (reference (eql :collect))
     name)
  (let ((var (khazern:var-spec (khazern:var instance))))
    (values `((,name (value)
                (setq ,var (sequence:adjust-sequence ,var (1+ (length ,var))
                                                     :initial-element value))))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance vector-scope) (reference (eql :append))
     name)
  (let ((collect-name (khazern:scope-reference instance :collect)))
    (values `((,name (value)
                (map nil #',collect-name value)))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance ansi-sequence-scope)
     (reference (eql :append)) name)
  (let ((var (khazern:var-spec (khazern:var instance)))
        (type (khazern:type-spec (khazern:var instance))))
    (values `((,name (value)
                (setq ,var (concatenate ',type ,var value))))
            `((inline ,name)))))

#+(or abcl clasp sbcl)
(defmethod khazern:scope-functions
    ((client extension-client) (instance extensible-sequence-scope) (reference (eql :append))
     name)
  (let ((var (khazern:var-spec (khazern:var instance))))
    (values `((,name (value)
                (let ((start1 (length ,var)))
                  (setq ,var (replace (sequence:adjust-sequence ,var (+ start1 (length value)))
                                      value :start1 start1)))))
              `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance vector-scope) (reference (eql :nconc))
     name)
  (let ((collect-name (khazern:scope-reference instance :collect)))
    (values `((,name (value)
                (map nil #',collect-name value)))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance ansi-sequence-scope)
     (reference (eql :nconc)) name)
  (let ((var (khazern:var-spec (khazern:var instance)))
        (type (khazern:type-spec (khazern:var instance))))
    (values `((,name (value)
                (setq ,var (concatenate ',type ,var value))))
            `((inline ,name)))))

#+(or abcl clasp sbcl)
(defmethod khazern:scope-functions
    ((client extension-client) (instance extensible-sequence-scope) (reference (eql :nconc))
     name)
  (let ((var (khazern:var-spec (khazern:var instance))))
    (values `((,name (value)
                (setq ,var (sequence:adjust-sequence ,var (+ (length ,var) (length value))
                                                     :initial-contents value))))
              `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:list-scope)
     (reference (eql :append)) name)
  (let ((list-name (gensym (symbol-name reference)))
        (collect-name (khazern:scope-reference instance :collect)))
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

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:list-scope)
     (reference (eql :nconc)) name)
  (let ((list-name (gensym (symbol-name reference)))
        (collect-name (khazern:scope-reference instance :collect)))
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

(defclass key-test-accumulation-clause (khazern:accumulation-clause) ())

(defmethod khazern:preposition-names
    ((client extension-client) (clause key-test-accumulation-clause))
  (values '(:key :test)
          '()
          '()))

(defclass key-predicate-accumulation-clause (khazern:accumulation-clause) ())

(defmethod khazern:preposition-names
    ((client extension-client) (clause key-predicate-accumulation-clause))
  (values '(:key :predicate)
          '(:predicate)
          '()))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :adjoin)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause :reference :adjoin)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:adjoin nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :adjoining)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause :reference :adjoin)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:adjoin nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :union)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause :reference :union)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:adjoin nil :union nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :unioning)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause :reference :union)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:adjoin nil :union nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nunion)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause :reference :nunion)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:adjoin nil :nunion nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :nunioning)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause :reference :nunion)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:adjoin nil :nunion nil)))
(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :intersection))
     &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause
                                             :reference :intersect)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:intersect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :intersecting)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause
                                             :reference :intersect)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:intersect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :nintersection))
     &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause
                                             :reference :intersect)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:intersect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :nintersecting)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause
                                             :reference :intersect)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:intersect nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :disjoin)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause :reference :disjoin)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:disjoin nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region)
     (keyword (eql :disjoining)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause :reference :disjoin)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:disjoin nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :difference))
     &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause
                                             :reference :difference)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:disjoin nil :difference nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :differencing))
     &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause
                                             :reference :difference)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:disjoin nil :difference nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :ndifference))
     &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause
                                             :reference :difference)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:disjoin nil :difference nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :ndifferencing))
     &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-test-accumulation-clause
                                             :reference :difference)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:disjoin nil :difference nil)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:selectable-region) (keyword (eql :merge)) &key)
  (khazern:parse-accumulation client
                              (make-instance 'key-predicate-accumulation-clause
                                             :reference :merge)
                              :default-type-spec 'list
                              :parse-type-spec t
                              :category :sequence
                              :scope-references '(:merge nil)))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:list-scope) (reference (eql :adjoin)) name)
  (let ((head (khazern:scope-reference instance :head))
        (tail (khazern:scope-reference instance :tail)))
    (values `((,name (value &rest args)
                (unless (apply #'member value (cdr ,head) args)
                  (rplacd ,tail
                          (setq ,tail (cons value nil))))))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance vector-scope) (reference (eql :adjoin)) name)
  (let ((var (khazern:var-spec (khazern:var instance))))
    (values `((,name (value &rest args)
                (unless (apply #'position value ,var args)
                  (vector-push-extend value ,var))))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance ansi-sequence-scope) (reference (eql :adjoin)) name)
  (let ((var (khazern:var-spec (khazern:var instance)))
        (type (khazern:type-spec (khazern:var instance))))
    (values `((,name (value &rest args)
                (unless (apply #'position value ,var args)
                  (setq ,var (concatenate ',type ,var (list value))))))
            `((inline ,name)))))

#+(or abcl clasp sbcl)
(defmethod khazern:scope-functions
    ((client extension-client) (instance extensible-sequence-scope) (reference (eql :adjoin))
     name)
  (let ((var (khazern:var-spec (khazern:var instance))))
    (values `((,name (value &rest args)
                (unless (apply #'position value ,var args)
                  (setq ,var (sequence:adjust-sequence ,var (1+ (length ,var))
                                                       :initial-element value)))))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:sequence-scope) (reference (eql :union)) name)
  (let ((adjoin-name (khazern:scope-reference instance :adjoin)))
    (values `((,name (value &rest args)
                (map nil (lambda (item)
                           (apply #',adjoin-name item args))
                     value)))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:sequence-scope) (reference (eql :nunion)) name)
  (let ((adjoin-name (khazern:scope-reference instance :adjoin)))
    (values `((,name (value &rest args)
                (map nil (lambda (item)
                           (apply #',adjoin-name item args))
                     value)))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:list-scope)
     (reference (eql :nunion)) name)
  (let ((head (khazern:scope-reference instance :head))
        (tail (khazern:scope-reference instance :tail))
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

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:list-scope) (reference (eql :intersect)) name)
  (let ((head (khazern:scope-reference instance :head))
        (tail (khazern:scope-reference instance :tail)))
    `((,name (value &key (test #'eql) (key #'identity))
        (prog ((head ,head))
         next
           (cond ((null (cdr head))
                  (return nil))
                 ((some (lambda (item)
                          (funcall test (funcall key (cadr head)) (funcall key item)))
                        value)
                  (setq head (cdr head)))
                 (t
                  (when (eq ,tail (cdr head))
                    (setq ,tail head))
                  (rplacd head (cddr head))))
           (go next))))))


(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:sequence-scope) (reference (eql :intersect))
     name)
  (let ((var (khazern:var-spec (khazern:var instance))))
    (values `((,name (value &rest args)
                (setq ,var (delete-if (lambda (item)
                                        (null (apply #'position item value args)))
                                      var))))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:list-scope) (reference (eql :disjoin)) name)
  (let ((head (khazern:scope-reference instance :head))
        (tail (khazern:scope-reference instance :tail)))
    `((,name (value &key (test #'eql) (key #'identity))
        (prog ((head ,head)
               (value-key (funcall key value)))
         next
           (cond ((null (cdr head))
                  (return nil))
                 ((funcall test (funcall key (cadr head)) value-key)
                  (when (eq ,tail (cdr head))
                    (setq ,tail head))
                  (rplacd head (cddr head)))
                 (t
                  (setq head (cdr head))))
           (go next))))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:sequence-scope) (reference (eql :disjoin))
     name)
  (let ((var (khazern:var-spec (khazern:var instance))))
    (values `((,name (value &rest args)
                (setq ,var (apply #'delete value ,var args))))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:sequence-scope) (reference (eql :difference))
     name)
  (let ((disjoin-name (khazern:scope-reference instance :disjoin)))
    (values `((,name (value &rest args)
                (map nil (lambda (item)
                           (apply #',disjoin-name item args))
                     value)))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:sequence-scope) (reference (eql :merge)) name)
  (let ((var (khazern:var-spec (khazern:var instance)))
        (type (khazern:type-spec (khazern:var instance))))
    (values `((,name (value &key predicate key)
                (setq ,var (merge ',type ,var value predicate :key key))))
            `((inline ,name)))))

(defmethod khazern:scope-functions
    ((client extension-client) (instance khazern:list-scope) (reference (eql :merge)) name)
  (let ((head (khazern:scope-reference instance :head))
        (tail (khazern:scope-reference instance :tail)))
    (values `((,name (value &key predicate key)
                (rplacd ,head (merge 'list (cdr ,head) value predicate :key key))
                (setq ,tail (last ,head))))
            `((inline ,name)))))
