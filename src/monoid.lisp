(in-package :contextual)

(defgeneric mempty-func (context))
(defun ask-mempty ()
  (ctx-asks #'mempty-func))
(defun mempty ()
  (let-app/ctx ((mempty (ask-mempty)))
    (funcall mempty)))


(defgeneric mappend-fuc (context))
(defun ask-mappend ()
  (ctx-asks #'mappend-func))
(defun mappend (cmx cmy)
  (let-app/ctx ((mappend (ask-mappend))
                (mx (ctx-injest cmx))
                (my (ctx-injest cmy)))
    (funcall mappend mx my)))

(defclass monoid-operators ()
  ((mempty :initarg :mempty :type function :reader mempty-func)
   (mappend :initarg :mappend :type function :reader mappend-func)))

(defmethod initialize-instance ((obj monoid-operators) &rest args)
  (initialize-mempty obj args)
  (initialize-mappend obj args))

(defun initialize-mempty (obj args)
    (let ((mempty (get-argument-or-slot-value args :mempty obj 'mempty)))
      (assert mempty)
      (setf (slot-value obj 'mempty) mempty)))

(defun initialize-mappend (obj args)
  (let ((mappend (get-argument-or-slot-value args :mappend obj 'mappend)))
    (assert mappend)
    (setf (slot-value obj 'mappend) mappend)))
