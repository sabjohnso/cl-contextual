(in-package :cl-user)

(defpackage :contextual-system
  (:use :cl :asdf :uiop))

(in-package :contextual-system)

(defsystem :contextual
  :description "Tools for functors, applicative functors and monads."
  :version "0.3.0"
  :author "Samuel B. Johnson <sabjohnso.dev@gmail.com>"
  :license "MIT"
  :depends-on (:trivia)
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "utility")
     (:file "syntax-helpers")
     (:file "internal")
     (:file "derivation")
     (:file "contextual")
     (:file "list")
     (:file "optional")
     (:file "thunk"))))
  :in-order-to ((test-op
                 (load-op :contextual)
                 (test-op :contextual/test))))

(defsystem :contextual/test
  :description "Tests for the `CONTEXTUAL' system"
  :depends-on (:fiveam :contextual)
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "syntax-helpers-test")
     (:file "internal-test")
     (:file "derivation-test")
     (:file "contextual-test")
     (:file "list-test")
     (:file "optional-test")
     (:file "thunk-test"))))
  :perform (test-op (o s)
             (symbol-call :binding-syntax-helpers-test :run-all-tests!)
             (symbol-call :contextual-internal-test :run-all-tests!)
             (symbol-call :contextual-derivation-test :run-all-tests!)
             (symbol-call :contextual-test :run-all-tests!)
             (symbol-call :contextual-list-test :run-all-tests!)
             (symbol-call :contextual-optional-test :run-all-tests!)
             (symbol-call :contextual-thunk-test :run-all-tests!)))
