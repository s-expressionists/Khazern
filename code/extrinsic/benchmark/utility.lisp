(cl:in-package #:khazern-extrinsic/benchmark)

(defvar *database-path* (make-pathname :directory '(:relative "benchmark")))

(defun write-results (name results)
  (ensure-directories-exist *database-path*)
  (let ((path (merge-pathnames (concatenate 'string name ".sexp")
                               *database-path*)))
    (with-open-file (stream path
                     :if-exists :supersede :if-does-not-exist :create
                     :direction :output)
        (with-standard-io-syntax
          (write results :stream stream))))
  (values))

(defvar *benchmarks* (make-hash-table))

(defclass benchmark ()
  ((intrinsic-function :reader intrinsic-function
                       :initarg :intrinsic-function)
   (extrinsic-function :reader extrinsic-function
                       :initarg :extrinsic-function)))

(defun make-extrinsic (expr)
  (cond ((consp expr)
         (cons (make-extrinsic (car expr))
               (make-extrinsic (cdr expr))))
        ((eq expr 'loop)
         'khazern-extrinsic:loop)
        (t
         expr)))
 
(defmacro define-benchmark (name &body body)
  `(setf (gethash ',name *benchmarks*)
         (make-instance 'benchmark
                        :intrinsic-function (lambda ()
                                              ,@body)
                        :extrinsic-function (lambda ()
                                              ,@(make-extrinsic body)))))

(defvar *minimum-bench-time* 10)

(defvar *overhead-time* 0)

(defun bench (thunk)
  (loop with start = (get-internal-run-time)
        with end = (+ start (* *minimum-bench-time* internal-time-units-per-second))
        for count from 1
        finally (return (- (/ (coerce (- (get-internal-run-time) start) 'double-float)
                              internal-time-units-per-second count)
                            *overhead-time*))
        while (< (get-internal-run-time) end)
        do (funcall thunk)))

(defun run ()
  (loop with *overhead-time* = 0
        for name being the hash-key in *benchmarks* using (hash-value benchmark)
        initially (format t "Measuring overhead...~%")
                  (setf *overhead-time* (bench (lambda () ())))
        finally (write-results (format nil "~a-~a" (uiop:implementation-type)
                                       (let* ((version (software-version))
                                              (pos (position #\- version)))
                                         (if pos
                                             (subseq version 0 pos)
                                             version)))
                                         
                               results)
        do (format t "Running ~a...~%" name)
        collect (list name
                      (bench (intrinsic-function benchmark))
                      (bench (extrinsic-function benchmark)))
        into results))

(defun report ()
  (loop with results = (sort (loop for path in (directory (merge-pathnames "*.sexp" *database-path*))
                             collect (cons (pathname-name path)
                                           (with-open-file (stream path)
                                             (with-standard-io-syntax
                                               (read stream)))))
                             #'string< :key #'car)
        with table = (ascii-table:make-table (list* "Test" (mapcar #'car results)))
        for test in (loop with tests = nil
                          for impl in results
                          finally (return (sort tests #'string<))
                          do (loop for (name) in (cdr impl)
                                   do (pushnew name tests)))
        finally (ascii-table:display table)
        do (ascii-table:add-row table
                                (list* (symbol-name test)
                                       (mapcar (lambda (impl)
                                                 (let ((pair (assoc test (cdr impl))))
                                                   (if pair
                                                       (format nil "~v,2f%"
                                                               (1- (length (car impl)))
                                                               (* 100 (/ (third pair) (second pair))))
                                                       "")))
                                               results)))))
