(cl:in-package #:khazern)

;;; Parser table

(defclass parser-table ()
  ((parsers :accessor parser-table-parsers
            :initarg :parsers
            :initform nil
            :type cl:list)
   (paths :reader parser-table-paths
          :initarg :paths
          :initform (make-hash-table :test #'equal))))

(defmethod parser-enabled-p ((table parser-table) name)
  (and (member name (parser-table-parsers table))
       t))

(defmethod (setf parser-enabled-p) (value (table parser-table) name)
  (if value
      (pushnew name (parser-table-parsers table))
      (setf (parser-table-parsers table) (delete name (parser-table-parsers table))))
  value)

(defmethod copy-parser-table ((table parser-table))
  (make-instance 'parser-table
                 :parsers (copy-list (parser-table-parsers table))
                 :paths (let ((paths (make-hash-table :test #'equal)))
                          (maphash (lambda (k v)
                                     (setf (gethash k paths) v))
                                   (parser-table-paths table))
                          paths)))

(defparameter *parser-table* nil)

(defmethod (setf iterator-path) (new-func (table parser-table) name)
  (setf (gethash (symbol-name name) (parser-table-paths table)) new-func))

(defmethod iterator-path ((table parser-table) name)
  (gethash (symbol-name name) (parser-table-paths table)))

(defmethod remove-iterator-path ((table parser-table) name)
  (remhash (symbol-name name) (parser-table-paths table)))

(defun loop-path-p (name)
  (and (iterator-path *parser-table* name) t))

;;; A parser is a function that takes a list of tokens to parse, and
;;; that returns three values:
;;;
;;;   * A generalized Boolean indicating whether the parse succeeded.
;;;
;;;   * The result of the parse.  If the parse does not succeed, then
;;;     this value is unspecified.
;;;
;;;   * A list of the tokens that remain after the parse.  If the
;;;     parse does not succeed, then this list contains the original
;;;     list of tokens passed as an argument.

;;; Functions that take one or more parsers as arguments can take
;;; either a function or the name of a function.

(defparameter *indent-level* 0)

(defparameter *parse-trace-p* nil)

(defparameter *empty-result* (cons nil nil))

(defparameter *placeholder-result* (cons nil nil))

(defvar *categories* (make-hash-table :test #'eq))

(defun parse-trace-output (format-control &rest arguments)
  (when *parse-trace-p*
    (format *trace-output*
            (make-string (* 2 *indent-level*) :initial-element #\Space))
    (apply #'format *trace-output* format-control arguments)))

(defun trace-parser (name parser tokens)
  (let ((*indent-level* (1+ *indent-level*)))
    (parse-trace-output "trying ~s on ~s~%" name tokens)
    (multiple-value-bind (successp terminalp result rest)
        (funcall parser tokens)
      (parse-trace-output "~:[no ~;~]success, ~:[no ~;~] terminal, ~a~%"
                          successp terminalp result)
      (values successp terminalp result rest))))

(defmacro define-parser (name categories &body body)
  `(progn
     ;; At compile time, we define a parser that generates an error
     ;; message if invoked.  The reason for doing that is to avoid
     ;; warnings at compile time saying the function does not exist. 
     (eval-when (:compile-toplevel)
       (setf (fdefinition ',name)
             (lambda (tokens)
               (declare (ignore tokens))
               (error "Undefined parser: ~s" ',name))))
     ;; At load time, we set the FDEFINITION of the name to the
     ;; result of executing BODY.
     (eval-when (:load-toplevel :execute)
       (setf (gethash ',name *categories*) ',categories
             (fdefinition ',name)
             (lambda (tokens)
               (trace-parser ',name (progn ,@body) tokens)))
                (finish-output))))

(defun none ()
  (lambda (tokens)
    (if tokens
        (values nil nil nil tokens)
        (values t nil nil nil))))

;;; Take a function designator (called the TRANSFORMER) and a
;;; predicate P and return a parser Q that invokes the predicate on
;;; the first token.  If P returns true then Q succeeds and returns
;;; the result of invoking TRANSFORMER on the token together with the
;;; remaining tokens (all tokens except the first one).  If P returns
;;; false, then Q fails.  If there are no tokens, then Q also fails.
(defun singleton (transformer predicate)
  (lambda (tokens)
    (if (and (not (null tokens))
             (funcall predicate (car tokens)))
        (values t nil (funcall transformer (car tokens)) (cdr tokens))
        (values nil nil nil tokens))))

(defun typep (type)
  (lambda (tokens)
    (cond ((null tokens)
           (values nil nil
                   (make-condition 'expected-token-but-end
                                   :location (cl:list (length tokens))
                                   :expected-type type)
                   nil))
          ((cl:typep (car tokens) type)
           (values t nil (car tokens) (cdr tokens)))
          (t
           (values nil nil
                   (make-condition 'expected-token-but-found
                                   :location (cl:list (length tokens))
                                   :expected-type type
                                   :found (car tokens))
                   tokens)))))

;;; Take a list of parsers P1, P2, ..., Pn and return a parser Q that
;;; invokes Pi in order until one of them succeeds.  If some Pi
;;; succeeds. then Q also succeeds with the same result as Pi.  If
;;; every Pi fails, then Q also fails.
(defun alternative (&rest parsers)
  (lambda (tokens)
    ;; We promised not to use the LOOP macro, so we do this with
    ;; TAGBODY instead. 
    (block nil
      (let ((remaining-parsers parsers)
            error-result)
        (tagbody
         again
           (when remaining-parsers
             (multiple-value-bind (successp terminalp result rest)
                 (funcall (pop remaining-parsers) tokens)
               (when (or successp terminalp)
                 (return (values successp terminalp result rest)))
               (setf error-result (combine-parse-errors error-result result))
               (go again))))
        (values nil nil error-result tokens)))))

;;; Take a function designator (called the COMBINER) and a list of
;;; parsers P1, P2, ..., Pn and return a parser Q that invokes every
;;; Pi in order.  If any Pi fails, then Q fails as well.  If every Pi
;;; succeeds, then Q also succeeds and returns the result of calling
;;; APPLY on COMBINER and the list of results of the invocation of
;;; each Pi.
(defun consecutive (combiner &rest parsers)
  (lambda (tokens)
    ;; We promised not to use the LOOP macro, so we do this with
    ;; TAGBODY instead. 
    (block nil
      (let ((remaining-tokens tokens)
            (remaining-parsers parsers)
            (results '())
            (found-terminal-p nil))
        (tagbody
         again
           (if (null remaining-parsers)
               (return (values t
                               found-terminal-p
                               (apply combiner (nreverse results))
                               remaining-tokens))
               (multiple-value-bind (successp terminalp result rest)
                   (funcall (car remaining-parsers) remaining-tokens)
                 (pop remaining-parsers)
                 (setf found-terminal-p (or found-terminal-p terminalp))  
                 (when successp
                   (unless (eq result *empty-result*)
                     (push result results))
                   (setf remaining-tokens rest)
                   (go again))
                 (return (values nil found-terminal-p result tokens)))))))))

(defun list (combiner &rest parsers)
  (lambda (tokens)
    (cond ((null tokens)
           (values nil nil
                   (make-condition 'expected-token-but-end
                                   :location (cl:list (length tokens))
                                   :expected-type 'list)
                   nil))
          ((listp (car tokens))
           (multiple-value-bind (successp terminalp result remaining-tokens)
               (funcall (apply #'consecutive combiner parsers) (car tokens))
             (cond ((and successp remaining-tokens)
                    (values nil terminalp
                            (make-condition 'unexpected-tokens-found
                                            :location (list (length tokens)
                                                            (- (length (car tokens))
                                                               (length remaining-tokens)))
                                            :found remaining-tokens)
                            tokens))
                   ((not successp)
                    (when result
                      (setf (location result)
                          (list* (length tokens)
                                 (- (length (car tokens))
                                    (car (location result)))
                                 (cdr (location result)))))
                    (values nil terminalp result tokens))
                   (t
                    (values t terminalp
                            result
                            (cdr tokens))))))
          (t
           (values nil nil
                   (make-condition 'expected-token-but-found
                                   :location (cl:list (length tokens))
                                   :expected-type 'list
                                   :found (car tokens))
                   tokens)))))

;;; Take a function designator (called the COMBINER) and a parser P
;;; and return a parser Q that invokes P repeatedly until it fails,
;;; each time with the tokens remaining from the previous invocation.
;;; The result of the invocation of Q is the result of calling APPLY
;;; on COMBINER and the list of the results of each invocation of P.
;;; Q always succeeds.  If the first invocation of P fails, then Q
;;; succeeds returning the result of calling APPLY on COMBINER and the
;;; empty list of results, and the original list of tokens as usual.
(defun repeat* (combiner parser)
  (lambda (tokens)
    (let ((remaining-tokens tokens)
          (results '()))
      (block nil
        (tagbody
         again
           (multiple-value-bind (successp terminalp result rest)
               (funcall parser remaining-tokens)
             (declare (ignore terminalp))
             (unless successp
               (return (values t nil
                               (apply combiner (reverse results))
                               remaining-tokens)))
             (unless (eq *empty-result* result)
               (push result results))
             (setf remaining-tokens rest)
             (go again)))))))

;;; Take a function designator (called the COMBINER) and a parser P
;;; and return a parser Q that invokes P repeatedly until it fails,
;;; each time with the tokens remaining from the previous invocation.
;;; The result of the invocation of Q is the result of calling APPLY
;;; on COMBINER and the list of the results of each invocation of P.
;;; Q succeeds if and only if at least one invocation of P succeeds.
(defun repeat+ (combiner parser)
  (lambda (tokens)
    (let ((results '()))
      (multiple-value-bind (successp terminalp result rest)
          (funcall parser tokens)
        (if (not successp)
            (values nil terminalp result tokens)
            (let ((remaining-tokens rest))
              (unless (eq *empty-result* result)
                (push result results))
              (block nil
                (tagbody
                 again 
                   (multiple-value-bind (successp terminalp result rest)
                       (funcall parser remaining-tokens)
                     (declare (ignore terminalp))
                     (unless successp
                       (return (values t nil
                                       (apply combiner (reverse results))
                                       remaining-tokens)))
                     (unless (eq *empty-result* result)
                       (push result results)
                       (setf remaining-tokens rest)
                       (go again)))))))))))

;;; Take a default value and a parser P and return a parser Q that
;;; always succeeds.  Q invokes P once.  If P succeeds, then Q
;;; succeeds with the same result as P and with the same remaining
;;; tokens.  If P fails, then Q succeeds, returning the default value
;;; and the original list of tokens.
(defun optional (default parser)
  (lambda (tokens)
    (multiple-value-bind (successp terminalp result rest)
        (funcall parser tokens)
      (cond (successp
             (values t nil result rest))
            (terminalp
             (values nil t result tokens))
            (t
             (values t nil default tokens))))))

(defun alternative-by-category (category)
  (lambda (tokens
           &aux error-result)
    (block parse-category
      (mapc (lambda (name)
              (when (member category (gethash name *categories*))
                (multiple-value-bind (successp terminalp result rest)
                    (funcall name tokens)
                  (when (or successp terminalp)
                    (return-from parse-category (values successp terminalp result rest)))
                  (setf error-result (combine-parse-errors error-result result)))))
            (parser-table-parsers *parser-table*))
      (values nil nil error-result tokens))))

(defun delimited-list-by-category (category &rest delimiters)
  (consecutive #'cons
               (alternative-by-category category)
               (repeat* #'cl:list
                        (consecutive #'identity
                                     (apply #'keyword delimiters)
                                     (alternative-by-category category)))))

(defun terminal (tokens)
  (values t t *empty-result* tokens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given a symbol S (no matter what package), return a singleton
;;; parser Q that recognizes symbols with the same name as one of
;;; symbols.  If Q succeeds, it returns the first token.

(defun keyword (&rest symbols)
  (lambda (tokens)
    (cond ((null tokens)
           (values nil nil
                   (make-condition 'expected-token-but-end
                                   :location (cl:list (length tokens))
                                   :expected-keywords symbols)
                   nil))
          ((member (car tokens) symbols :test #'symbol-equal)
           (values t nil *empty-result* (cdr tokens)))
          (t
           (values nil nil
                   (make-condition 'expected-token-but-found
                                   :location (cl:list (length tokens))
                                   :expected-keywords symbols
                                   :found (car tokens))
                   tokens)))))

(defun keyword? (&rest symbols)
  (lambda (tokens)
    (values t nil *empty-result*
            (if (and tokens
                     (member (car tokens) symbols :test #'symbol-equal))
                (cdr tokens)
                tokens))))

;;;  LocalWords:  parsers


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser simple-type-spec-parser ()
  (lambda (tokens)
    (if (and (not (null tokens))
             (member (car tokens) '(fixnum float t nil)))
        (values t nil
                (car tokens)
                (cdr tokens))
        (values nil nil nil tokens))))

(define-parser destructured-type-spec-parser ()
  (consecutive #'identity
               (keyword :of-type)
               'anything))

(define-parser type-spec-parser ()
  (alternative 'simple-type-spec-parser 'destructured-type-spec-parser))

(define-parser optional-type-spec/t ()
  (optional t 'type-spec-parser))

(define-parser optional-type-spec/placeholder ()
  (optional *placeholder-result* 'type-spec-parser))

(define-parser optional-type-spec/number ()
  (optional 'number 'type-spec-parser))

(define-parser optional-type-spec/real ()
  (optional 'real 'type-spec-parser))

(define-parser optional-type-spec/fixnum ()
  (optional 'fixnum 'type-spec-parser))
