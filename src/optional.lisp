(in-package :cl-user)

(defpackage :contextual-optional
  (:use :cl :trivia :contextual-utility :contextual)
  (:export #:optional #:optional-p
           #:just #:just-p
           #:none #:none-p
           #:make-optional-context))

(in-package :contextual-optional)

(defunion optional
  (just x)
  (none))

(declaim (ftype (function (function optional) optional) optional-flatmap))
(defun optional-flatmap (f mx)
  (match mx
    ((just x) (funcall f x))
    ((none)   mx)))

(defun make-optional-context ()
  (make-instance 'monad-operators
    :pure #'just
    :flatmap #'optional-flatmap))
