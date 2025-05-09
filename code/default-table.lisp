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
  '((make-for-as-hash-key :hash-key :hash-keys)
    (make-for-as-hash-value :hash-value :hash-values)
    (make-for-as-package-symbol :symbol :symbols)
    (make-for-as-package-present-symbol :present-symbol :present-symbols)
    (make-for-as-package-external-symbol :external-symbol :external-symbols)))

(defmethod copy-parser-table ((table null))
  (let ((table (make-instance 'parser-table
                              :parsers (copy-list *default-parsers*))))
    (mapc (lambda (args)
            (apply #'add-path table args))
          *default-paths*)
    table))
