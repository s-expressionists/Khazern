(cl:in-package #:khazern)

(deftype constant ()
  '(satisfies constantp))

(deftype simple-type-spec ()
  '(or null (member fixnum float t)))

(deftype d-var-spec ()
  '(satisfies d-var-spec-p))

(deftype d-type-spec ()
  '(satisfies d-type-spec-p))

(deftype simple-var ()
  '(and symbol (not constant)))

(deftype nullable-simple-var ()
  '(or null simple-var))
