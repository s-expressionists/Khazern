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
    for-as-hash-key
    for-as-hash-value
    for-as-in-list-parser
    for-as-on-list-parser
    for-as-package-parser
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

(defmethod copy-parser-table ((table (eql nil)))
  (make-instance 'parser-table
                 :parsers (copy-seq *default-parsers*)))
