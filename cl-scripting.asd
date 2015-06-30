#-asdf3.1 (error "ASDF 3.1 or bust!")

(defsystem "cl-scripting"
  :version "0" ;; not even released
  :description "Utilities to help in writing scripts in CL"
  :license "MIT" ;; code originally excerpted from ASDF
  :author "Francois-Rene Rideau"
  :class :package-inferred-system
  :depends-on ("cl-scripting/cl-scripting"))
