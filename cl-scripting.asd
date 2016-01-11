#-asdf3.1 (error "ASDF 3.1 or bust!")

(defsystem "cl-scripting"
  :version "0.2"
  :description "Utilities to help in writing scripts in CL"
  :license "MIT" ;; code originally excerpted from ASDF
  :author "Francois-Rene Rideau"
  :class :package-inferred-system
  :depends-on ("cl-scripting/cl-scripting")
  :in-order-to ((test-op (test-op "cl-scripting/test"))))

(defsystem "cl-scripting/test"
  :description "Unit tests for cl-scripting"
  :depends-on ("cl-scripting/test/failure")
  :perform (test-op (o c) (symbol-call :cl-scripting/test/suite :cl-scripting/test)))
