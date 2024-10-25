(in-package :cl-user)

(defpackage :contextual-system
  (:use :cl :asdf :uiop))

(in-package :contextual-system)

(defsystem :contextual
  :description "Tools for functors, applicative functors and monads."
  :version "0.1.0"
  :author "Samuel B. Johnson <sabjohnso.dev@gmail.com>"
  :license "MIT"
  :depends-on (:trivia)
  :serial t
  :components
  ((:module "src"
    :components
    ((:file syntax-helpers)
     (:file contextual))))
  :in-order-to ((test-op
                 (load-op :contextual)
                 (test-op :contextual/test))))

(defsystem :contextual/test
  :description "Tests for the `CONTEXTUAL' system"
  :depends-on (:fiveam)
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "syntax-helpers-test")
     (:file "contextual-test"))))
  :perform (test-op (o s)
             (symbol-call :binding-syntax-helpers-test :run-all-tests!)
             (symbol-call :contextual-test :run-all-tests!)))
