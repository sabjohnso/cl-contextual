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
    (etypecase mx
      (just (funcall f (just-value mx)))
      (none (none)))))

(defun optional-fail (string)
  (declare (ignore string))
  (none))

(defun make-optional-context ()
  (make-instance 'monad-fail-operators
    :pure #'just
    :flatmap #'optional-flatmap
    :fail #'optional-fail))
