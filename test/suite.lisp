(uiop:define-package :cl-scripting/test/suite
  (:use :cl :uiop :hu.dwim.stefil)
  (:export #:cl-scripting/test))

(in-package :cl-scripting/test/suite)

(defsuite (cl-scripting/test :in root-suite :documentation "cl-scripting tests"))
