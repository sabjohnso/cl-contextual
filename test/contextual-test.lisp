(in-package :cl-user)

(defpackage :contextual-test
  (:use :cl :5am :contextual)
  (:export #:run-all-tests!))

(in-package :contextual-test)

(defun run-all-tests! ()
  (format t "CONTEXTUAL-TEST"))
