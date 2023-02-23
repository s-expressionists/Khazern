(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given a symbol S (no matter what package), return a singleton
;;; parser Q that recognizes symbols with the same name as one of
;;; symbols.  If Q succeeds, it returns the first token.

(defun keyword (&rest symbols)
  (singleton #'identity
             (lambda (token)
               (member token symbols :test #'symbol-equal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for anything, i.e. a parser that succeeds whenever the list
;;; of tokens is not empty.  It returns the first token as a result of
;;; the parse, and the list of tokens with the first one removed as
;;; the list of remaining tokens.

(define-parser anything ()
  (singleton #'identity (constantly t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A parser that recognizes one of the LOOP keywords EACH and THE.
;;; It is used to parse FOR-AS-HASH and FOR-AS-PACKAGE subclauses.

(define-parser each-the-parser ()
  (keyword 'each 'the))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A parser that recognizes one of the LOOP keywords IN and OF.
;;; It is used to parse FOR-AS-HASH and FOR-AS-PACKAGE subclauses.

(define-parser in-of-parser ()
  (keyword 'in 'of))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A parser that recognizes one of the LOOP keyword BEING.
;;; It is used to parse FOR-AS-HASH and FOR-AS-PACKAGE subclauses.

(define-parser being-parser ()
  (keyword 'being))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for COMPOUND-FORM+, i.e. a non-empty sequence of compound
;;; forms.

(define-parser compound-form+ ()
  (repeat+ (lambda (&rest forms)
             (cons 'progn forms))
           (singleton #'identity #'consp)))

(define-parser body-parser ()
  (consecutive (lambda (clauses none)
                 (declare (ignore none))
                 clauses)
               (repeat* #'list
                        (alternative-by-category :body-clause))
               (none)))

;;; Create a list of clauses from the body of the LOOP form.
(defun parse-loop-body (body parser-table)
  (multiple-value-bind (success-p clauses tokens)
      (let ((*parser-table* parser-table))
        (body-parser body))
    (unless success-p
      ;; FIXME: this is not the right error to signal.
      (error 'expected-keyword-but-found
             :found tokens))
    clauses))
