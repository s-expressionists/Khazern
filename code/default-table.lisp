(cl:in-package #:khazern)

(defparameter *default-parsers*
  '(always-clause
    append-clause
    collect-clause
    conditional-clause
    count-clause
    do-clause
    final-clause
    for-as-across
    for-as-arithmetic-parser
    for-as-clause
    for-as-equals-then-parser
    for-as-in-list-parser
    for-as-on-list-parser
    for-as-path-parser
    initial-clause
    maximize-clause
    minimize-clause
    name-clause
    nconc-clause
    never-clause
    repeat-clause
    return-clause
    sum-clause
    thereis-clause
    until-clause
    while-clause
    with-clause))

(defparameter *default-paths*
  '((:names (:hash-key :hash-keys)
     :function make-for-as-hash-key
     :prepositions ((:in :of)
                    (:using)))
    (:names (:hash-value :hash-values)
     :function make-for-as-hash-value
     :prepositions ((:in :of)
                    (:using)))
    (:names (:symbol :symbols)
     :function make-for-as-package-symbol
     :prepositions ((:in :of)))
    (:names (:present-symbol :present-symbols)
     :function make-for-as-package-present-symbol
     :prepositions ((:in :of)))
    (:names (:external-symbol :external-symbols)
     :function make-for-as-package-external-symbol
     :prepositions ((:in :of)))))

(defmethod copy-parser-table ((table null))
  (make-instance 'parser-table
                 :parsers (copy-list *default-parsers*)
                 :paths (let ((paths (make-hash-table :test #'equal)))
                          (mapc (lambda (path-desc)
                                  (let ((instance (make-instance 'path
                                                                 :function (getf path-desc :function)
                                                                 :data (getf path-desc :data))))
                                    (mapc (lambda (p)
                                            (mapc (lambda (n)
                                                    (setf (gethash (symbol-name n) (path-prepositions instance))
                                                          (car p)))
                                                  p))
                                          (getf path-desc :prepositions))
                                    (mapc (lambda (name)
                                            (setf (gethash (symbol-name name) paths)
                                                  instance))
                                          (getf path-desc :names))))
                                *default-paths*)
                          paths)))
