(in-package :cl-user)

(defpackage :contextual-bare
  (:use :cl :contextual)
  (:export #:make-bare-context))


(in-package :contextual-bare)

(defun make-bare-context ()
  "Return a trivial context for bare values"
  (make-instance 'trivial-operators
    :wrap #'identity
    :unwrap #'identity))
