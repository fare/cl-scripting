(uiop:define-package :cl-scripting/test/failure
  (:use :cl :uiop :fare-utils :hu.dwim.stefil :cl-scripting/failure :cl-scripting/test/suite)
  (:import-from :cl-scripting/failure
   #:execution-failure
   #:failure-context
   #:failure-reason
   #:make-failure
   #:make-error)
  (:export
   #:test-suite))

(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3)))

(in-package :cl-scripting/test/failure)

(in-suite cl-scripting/test)

(defsuite* (test-suite :in cl-scripting/test))

(defun is-success (result values)
  (is (first result))
  (is (successp result))
  (is (equal (success-values result) values)))

(deftest toplevel-wfc-success ()
  (is-success
   (multiple-value-list (with-failure-context () (values 1 2 3)))
   '(1 2 3)))

(deftest toplevel-wfc-success-nil ()
  (is-success
   (multiple-value-list (with-failure-context (:name "irrelevant") nil))
   '(nil)))

(deftest toplevel-wfc-success-success ()
  (is-success
   (multiple-value-list (with-failure-context () (success 1 2 3)))
   '(1 2 3)))

(defun is-same-failure (failure1 failure2)
  (check-type failure1 execution-failure)
  (check-type failure2 execution-failure)
  (is (equal (failure-context failure1) (failure-context failure2)))
  (let ((reason1 (failure-reason failure1))
        (reason2 (failure-reason failure2)))
    (is (eq (type-of reason1) (type-of reason2)))
    (when (typep reason1 'simple-condition)
     (is (equal (simple-condition-format-control reason1)
                (simple-condition-format-control reason2)))
     (is (equal (simple-condition-format-arguments reason1)
                (simple-condition-format-arguments reason2))))))

(defun make-execution-failure (description)
  (destructuring-bind (context &optional reason &rest arguments) description
    (check-type context list)
    (check-type reason (or string (and symbol (not null))))
    (make-condition 'execution-failure
                    :context (reverse context)
                    :reason (apply 'make-error reason arguments))))

(defun is-failure (result expected-failures)
  (is (not (first result)))
  (is (failurep result))
  (let ((failures (failure-failures result)))
    (is (= (length failures) (length expected-failures)))
    (map () 'is-same-failure failures (mapcar 'make-execution-failure expected-failures))))

(deftest toplevel-wfc-fail! ()
  (is-failure
   (multiple-value-list (with-failure-context (:name "a") (fail! "foo")))
   '((("a") "foo"))))

(deftest toplevel-wfc-error ()
  (is-failure
   (multiple-value-list (with-failure-context () (error "bar ~A" 1)))
   '((() "bar ~A" 1))))

(deftest toplevel-wfc-failure ()
  (is-failure
   (multiple-value-list
    (with-failure-context (:name "a")
      (with-failure-context (:name "b")
        (failure (list (make-failure "foo") (make-failure "bar"))))))
   '((("a" "b") "foo")
     (("a" "b") "bar"))))

(deftest toplevel-wfc-first-failure-only ()
  (is-failure
   (multiple-value-list
    (with-failure-context (:name "a")
      (with-failure-context (:name "b") (fail! "foo"))
      (with-failure-context (:name "c") (fail! "bar"))))
   '((("a" "b") "foo"))))

(deftest toplevel-wfc-without-stopping ()
  (is-failure
   (multiple-value-list
    (with-failure-context (:name "a")
      (without-stopping ()
        (with-failure-context (:name "b")
          (fail! "foo"))
        (with-failure-context (:name "c")
          (error "bar"))
        (with-failure-context (:name "d")
          (failure (list (make-failure "baz") (make-failure "quux")))))))
   '((("a" "b") "foo")
     (("a" "c") "bar")
     (("a" "d") "baz")
     (("a" "d") "quux"))))

(deftest toplevel-wfc-without-stopping-no-context ()
  (is-failure
   (multiple-value-list
    (without-stopping ()
      (success 1)
      (fail! "foo")
      (success 2)
      (error "bar")
      (success 3)
      (failure (list (make-failure "baz") (make-failure "quux")))
      (success 4)))
   '((() "foo")
     (() "bar")
     (() "baz")
     (() "quux"))))
