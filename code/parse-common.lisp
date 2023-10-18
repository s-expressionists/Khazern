(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for anything, i.e. a parser that succeeds whenever the list
;;; of tokens is not empty.  It returns the first token as a result of
;;; the parse, and the list of tokens with the first one removed as
;;; the list of remaining tokens.

(define-parser anything ()
  (typep t))

(define-parser optional-into-phrase ()
  (optional nil
            (consecutive #'identity
                         (keyword :into)
                         'terminal
                         'simple-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A parser that recognizes one of the LOOP keyword BEING.
;;; It is used to parse FOR-AS-HASH and FOR-AS-PACKAGE subclauses.

(define-parser simple-var ()
  (typep '(and symbol (not (satisfies constantp)))))

(define-parser d-var-spec ()
  (typep '(satisfies d-var-spec-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for COMPOUND-FORM+, i.e. a non-empty sequence of compound
;;; forms.

(define-parser compound-form+ ()
  (repeat+ (lambda (&rest forms)
             forms)
           (typep 'cons)))
                  
(define-parser body-parser ()
  (lambda (tokens
           &aux results)
    (block nil
      (tagbody
       repeat
       (when tokens
         (multiple-value-bind (successp keywordp result remaining-tokens)
             (funcall (alternative-by-category :body-clause) tokens)
           (declare (ignore keywordp))
           (unless successp
             (return (values nil nil result tokens)))
           (push result results)
           (setf tokens remaining-tokens)
           (go repeat))))
      (values t nil (nreverse results) nil))))

;;; Create a list of clauses from the body of the LOOP form.
(defun parse-loop-body (body parser-table)
  (multiple-value-bind (success-p keywordp result tokens)
      (let ((*parser-table* parser-table))
        (body-parser body))
    (declare (ignore keywordp))
    (cond (success-p
           result)
          (result
           (setf (car (location result))
                 (- (length tokens) (car (location result))))
           (error result))
          (t
           (error "Unknown loop parse error")))))
