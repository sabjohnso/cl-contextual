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
    :serial t
    :components
    ((:file "package")
     (:file "utility")
     (:file "syntax-helpers")
     (:file "internal")
     (:file "derivation")
     (:file "monoid")
     (:file "functor")
     (:file "applicative")
     (:file "monad")
     (:file "comonad")
     (:file "trivial")
     (:file "contextual")
     (:file "monad-fail")
     (:file "monad-plus")
     (:file "monad-environment")
     (:file "monad-state")
     (:file "list")
     (:file "optional")
     (:file "thunk")
     (:file "bare")
     (:file "bare-function")
     (:file "bare-state")
     (:file "unitary-list"))))
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
     (:file "thunk-test")
     (:file "bare-test")
     (:file "bare-function-test")
     (:file "bare-state-test")
     (:file "unitary-list-test")
     (:file "monoid-test"))))
  :perform (test-op (o s)
             (symbol-call :binding-syntax-helpers-test :run-all-tests!)
             (symbol-call :contextual-internal-test :run-all-tests!)
             (symbol-call :contextual-derivation-test :run-all-tests!)
             (symbol-call :contextual-test :run-all-tests!)
             (symbol-call :contextual-list-test :run-all-tests!)
             (symbol-call :contextual-optional-test :run-all-tests!)
             (symbol-call :contextual-thunk-test :run-all-tests!)
             (symbol-call :contextual-thunk-test :run-all-tests!)
             (symbol-call :contextual-bare-test :run-all-tests!)
             (symbol-call :contextual-bare-function-test :run-all-tests!)
             (symbol-call :contextual-bare-state-test :run-all-tests!)
             (symbol-call :contextual-unitary-list-test :run-all-tests!)
             (symbol-call :contextual-monoid-test :run-all-tests!)))
