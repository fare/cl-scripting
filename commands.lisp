(uiop:define-package :cl-scripting/commands
  (:use :cl :uiop :cl-launch/dispatch :cl-scripting/failure)
  (:export #:run-command #:register-command #:register-commands #:*failure-output* #:*success-output*))

(in-package :cl-scripting/commands)

(defvar *failure-output* t)
(defvar *success-output* t)

(defun run-command (fun &rest args)
  (let ((results (multiple-value-list (with-failure-context () (apply fun args)))))
    ;; Don't print anything on success for regular commands, otherwise print all values returned.
    (if (failurep results)
        (let ((failures (failure-failures results)))
          (format *failure-output* "~&Failure~P:~{~& ~A~}~&" (length failures) failures))
        (format *success-output* "~{~&~S~&~}" (if (successp results) (success-values results) results)))
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
