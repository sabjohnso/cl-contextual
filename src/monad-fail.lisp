(in-package :contextual)

(defgeneric fail-func (context))
(defun ask-fail ()
  (ctx-asks #'fail-func))

(defun fail (str)
  (let-app/ctx ((fail (ask-fail)))
    (funcall fail str)))

(defclass monad-fail-operators (monad-operators)
  ((fail :initarg :fail :reader fail-func)))

(defmethod initialize-instance ((obj monad-fail-operators) &rest args)
  (call-next-method)
  (let ((fail (get-argument-or-slot-value args :fail obj 'fail)))
    (assert fail)
    (setf (slot-value obj 'fail) fail)))
