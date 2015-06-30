(uiop:define-package :cl-scripting/commands
  (:use :cl :uiop :cl-launch/dispatch :cl-scripting/failure)
  (:export #:run-command #:register-command #:register-commands))

(in-package :cl-scripting/commands)

(defun run-command (fun &rest args)
  (let ((results (multiple-value-list (with-failure-context () (apply fun args)))))
    ;; Don't print anything on success for regular commands, otherwise print all values returned.
    (if (failurep results)
        (let ((failures (failure-failures results)))
          (format t "~&Failure~P:~{~& ~A~}~&" (length failures) failures))
        (format t "~{~&~S~&~}" (if (successp results) (success-values results) results)))
    (apply 'values results)))

(defun register-command (command)
  (check-type command symbol)
  (when (fboundp command)
    (cl-launch/dispatch:register-entry
     (string-downcase command)
     #'(lambda (argv) (apply 'run-command command argv)))))

(defun register-commands (commands)
  (etypecase commands
    (list (map () 'register-command commands))
    ((or string symbol package) (do-external-symbols (command commands) (register-command command)))))
