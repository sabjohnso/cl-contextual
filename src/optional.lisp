(in-package :cl-user)

(defpackage :contextual-optional
  (:use :cl :trivia :contextual-utility :contextual)
  (:export #:optional #:optional-p
           #:just #:just-p
           #:none #:none-p
           #:make-optional-context))

(in-package :contextual-optional)

(defunion optional
  (just value)
  (none))


(deftype optional-constructor ()
  '(function (t) optional))


(defun optional-flatmap (f mx)
  (declare (type optional-constructor f)
           (type optional mx))
  (the optional
    (ematch mx
      ((just value) (funcall f value))
      ((none) mx))))

(defun make-optional-context ()
  (make-instance 'monad-operators
    :pure #'just
    :flatmap #'optional-flatmap))
