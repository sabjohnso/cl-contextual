(in-package :contextual)

(defgeneric comp-func (obj))
(defun ask-comp () (ctx-asks #'comp-func))
(defun comp2 (ccat0 ccat1)
  (let-app/ctx ((comp (ask-comp))
                 (cat0 (ctx-injest ccat0))
                 (cat1 (ctx-injest ccat1)))
    (funcall comp cat0 cat1)))

(defun comp (&rest cats)
  (fold #'comp2 (id) cats))

(defgeneric id-value (obj))
(defun ask-id () (ctx-asks #'id-value))
(defun id ()
  (let-app/ctx ((id (ask-id)))
    id))

(defclass category-operators ()
  ((comp :initarg :comp :type function :reader comp-func)
   (id :initarg :id :reader id-value)))

(defmethod initialize-instance ((obj category-operators) &rest args)
  (validate-minimal-category-definition obj args)
  (initialize-id obj args)
  (initialize-comp obj args))

(defun validate-minimal-category-definition (obj args)
  (with-init-lookup (args obj)
      (id comp)
    (unless (and comp id)
      (error "insufficient input to define category operators"))))

(defun initialize-id (obj args)
  (with-init-lookup (args obj) (id)
    (setf (slot-value obj 'id) id)))

(defun initialize-comp (obj args)
  (with-init-lookup (args obj) (comp)
    (setf (slot-value obj 'comp) comp)))

(defun <<< (&rest fs)
  "Right to left composition of category members"
  (fold #'comp (id) fs))

(defun >>> (&rest fs)
  "Left to right composition of category members"
    (apply #'<<< (reverse fs)))
