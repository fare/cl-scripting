;;;; Useful information when failing.

(uiop:define-package :cl-scripting/failure
  (:use :common-lisp :fare-utils)
  (:export
   #:success #:failure #:successp #:failurep #:success-values #:failure-failures
   #:with-failure-context #:success-if #:failure-if #:fail!
   #:without-stopping #:call-without-stopping))

(in-package :cl-scripting/failure)

(defclass context ()
  ((names :reader names :initarg :names :type list)
   (muffle-failures :reader muffle-failures :initarg :muffle-failures :type boolean)
   (ancestor :reader ancestor :initarg :ancestor :type (or context null))
   (failures :accessor failures :initform nil :type list)))

(defvar *context* nil)

(define-condition execution-error (error)
  ())

(defmethod print-object ((failure execution-error) stream)
  (if *print-escape*
      (print-unreadable-object (failure stream :type t :identity nil)
        (let ((*print-escape* nil))
          (print-object failure stream)))
      (princ "FAILED" stream)))

(define-condition execution-failure (execution-error)
  ((context :initform nil :initarg :context :reader failure-context)
   (reason :initarg :reason :reader failure-reason)))

(defmethod print-object ((failure execution-failure) stream)
  (if *print-escape* (call-next-method)
      (format stream "~@[In ~{~A~^, ~}: ~]~:[FAILED~;~:*~A~]"
              (reverse (failure-context failure))
              (failure-reason failure))))

(define-condition execution-failures (execution-error)
  ((failures :initform nil :initarg :failures :reader failure-list)))

(defmethod print-object ((failures execution-failures) stream)
  (if *print-escape* (call-next-method)
      (format stream "Failure~@[~P~:*~{~&~A~&~}~]"
              (reverse (failure-list failures)))))

(defun make-error (&optional reason &rest arguments)
  (etypecase reason
    (error reason)
    (string (make-condition 'simple-error
                            :format-control reason :format-arguments arguments))
    (null (make-failures))
    (symbol (apply 'make-condition reason arguments))))

(defun fail! (&rest reason-arguments)
  (error (apply 'make-error reason-arguments)))

(defun success (&rest values)
  (apply 'values t '#:success values))

(defun result-values (&rest values)
  "If values represent success, pass them on.
If values represent failure, fail.
Otherwise, return values representing success with given values."
  (cond
    ((successp values) (apply 'values values))
    ((failurep values) (error (make-failures (failure-failures values))))
    (t (apply 'success values))))

(defun failure (&optional failures)
  "Return magic values that signal failure"
  (values nil '#:failure failures))

(defun successp (value-list)
  (and (length>=n-p value-list 2)
       (equal (subseq value-list 0 2)
              (multiple-value-list (success)))))

(defun success-values (value-list)
  (subseq value-list 2))

(defun failurep (value-list)
  (and (length>=n-p value-list 2)
       (equal (subseq value-list 0 2)
              (subseq (multiple-value-list (failure)) 0 2))))

(defun failure-failures (value-list)
  (third value-list))

(defun success-if (test &rest failure-args)
  "If TEST, return success, otherwise, return failure with FAILURE-ARGS"
  (if test
      (success)
      (apply 'fail! (or failure-args '("failed")))))

(defun failure-if (test &rest failure-args)
  "If not TEST, return success, otherwise, return failure with FAILURE-ARGS"
  (apply 'success-if (not test) failure-args))

(defun make-failure (&optional reason &rest arguments)
  (if (typep reason 'execution-error)
      reason
      (make-condition 'execution-failure
                      :context (names *context*)
                      :reason (apply 'make-error reason arguments))))

(defun make-failures (&optional list)
  (make-condition 'execution-failures :failures list))

(defun register-failures (failures)
  (etypecase failures
    (execution-failure
     (push failures (failures (or (ancestor *context*) *context*))))
    (execution-failures
     (register-failures (failure-list failures)))
    (list
     (map () 'register-failures failures))
    (error
     (register-failures (make-failure failures)))))

(defmacro with-failure-context ((&rest keys &key name muffle-failures) &body body)
  (declare (ignore name muffle-failures))
  `(call-with-failure-context (lambda () ,@body) ,@keys))

(defun call-with-failure-context (thunk &key name (muffle-failures nil mfp))
  (block nil
    (let* ((parent *context*)
           (toplevel (not parent))
           (muffle-failures (if mfp muffle-failures toplevel))
           (context
            (make-instance 'context
                           :names (let ((p (when parent (names parent)))) (if name (cons name p) p))
                           :ancestor (when parent (or (ancestor parent) parent))
                           :muffle-failures muffle-failures))
           (*context* context))
      (labels ((process-failures (c)
                 (register-failures c)
                 (let ((failures (when toplevel (reverse (failures context)))))
                   (if muffle-failures
                       (return (failure failures))
                       (error (make-failures failures)))))
               (compute ()
                 (handler-bind ((error #'process-failures))
                   (funcall thunk))))
        (let ((results (multiple-value-list (compute))))
          (when (failurep results)
            (process-failures (failure-failures results)))
          (apply 'result-values results))))))

(defmacro without-stopping (() &body body)
  `(call-without-stopping (list ,@(mapcar (lambda (form) `(lambda () ,form)) body))))

(defun call-without-stopping (thunks)
  (with-failure-context ()
    (let ((failurep nil)
          (results nil))
      (dolist (thunk thunks)
        (setf results (multiple-value-list
                       (with-failure-context (:muffle-failures t)
                         (funcall thunk))))
        (when (failurep results) (setf failurep t)))
      (when failurep (fail!))
      (apply 'values results))))
