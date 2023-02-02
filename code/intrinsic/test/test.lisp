(in-package #:khazern-intrinsic/test)

(defun check-repo (&key directory repository &allow-other-keys)
  (format t "~:[Did not find~;Found~] ~A clone in ~A, assuming everything is okay.~%"
          (probe-file directory) repository directory))

(defun sync-repo (&key (git "git") clean directory repository branch commit
                  &allow-other-keys
                  &aux (exists (probe-file directory)))
  (cond ((and exists (not clean))
         (format t "Fetching ~A~%" repository)
         (uiop:run-program (list git "fetch" "--quiet")
                           :output :interactive
                           :error-output :output
                           :directory directory))
        (t
         (when (and clean exists)
           (format t "Removing existing directory ~A~%" directory)
           (uiop:delete-directory-tree exists :validate t))
         (format t "Cloning ~A~%" repository)
         (uiop:run-program (list git "clone" repository (namestring directory))
                           :output :interactive
                           :error-output :output)))
  (when (or commit branch)
    (format t "Checking out ~A from ~A~%" (or commit branch) repository)
    (uiop:run-program (list git "checkout" "--quiet" (or commit branch))
                      :output :interactive
                      :error-output :output
                      :directory directory))
  (when (and branch (not commit))
    (format t "Fast forwarding to origin/~A from ~A~%" branch repository)
    (uiop:run-program (list git "merge" "--ff-only" (format nil "origin/~A" branch))
                      :output :interactive
                      :error-output :output
                      :directory directory)))

(defvar +ansi-test-repository+ "https://gitlab.common-lisp.net/ansi-test/ansi-test.git")

(defun test (&rest args &key skip-sync &allow-other-keys)
  (let ((*default-pathname-defaults* (merge-pathnames (make-pathname :directory '(:relative "ansi" "ansi-test"))
                                                      (asdf:component-pathname (asdf:find-system :khazern-intrinsic/test)))))
    (if skip-sync
        (check-repo :directory *default-pathname-defaults* :repository +ansi-test-repository+)
        (apply #'sync-repo :directory *default-pathname-defaults* :repository +ansi-test-repository+ args))
    (load #P"doit1.lsp")
    (load #P"iteration/load.lsp")
    (uiop:symbol-call :regression-test :do-tests)
    (when (symbol-value (find-symbol "*FAILED-TESTS*" :regression-test))
      (error "Unexpected failures in ANSI tests"))))
