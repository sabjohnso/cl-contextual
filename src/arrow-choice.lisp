(in-package :contextual)

(defgeneric choose-either-func (ctx))
(defun ask-choose-either ()
  (ctx-asks #'choose-either-func))
(defun choose-either (ca0 ca1)
  "Split the input between the two argument arrows, retagging
and merging their outputs. Those marked `LEFT' are passed to the
first argument arrow and those marked `RIGHT' are passed to the
second argument arrow."
  (let-app/ctx ((choose-either (ask-choose-either))
                (a0 (ctx-injest ca0))
                (a1 (ctx-injest ca1)))
    (funcall choose-either a0 a1)))

(defgeneric choose-left-func (ctx))
(defun ask-choose-left ()
  (ctx-asks #'choose-left-func))
(defun choose-left (ca)
  "Feed inputs marked `LEFT' through the argument arrow, passing the rest
through unchanged to the output."
  (let-app/ctx ((choose-left (ask-choose-left))
                (a (ctx-injest ca)))
    (funcall choose-left a)))

(defgeneric choose-right-func (ctx))
(defun ask-choose-right ()
  (ctx-asks #'choose-right-func))
(defun choose-right (ca)
  "Feed inputs marked `RIGHT' through the argument arrow, passing the rest
through unchanged to the output."
  (let-app/ctx ((choose-right (ask-choose-right))
                (a (ctx-injest ca)))
    (funcall choose-right a)))

(defgeneric fanin-func (ctx))
(defun ask-fanin ()
  (ctx-asks #'fanin-func))
(defun fanin (ca0 ca1)
  "Split the input between the two argument arrows and merge their outputs.
Those marked `LEFT' are passed to the first argument arrow and those marked
`RIGHT' are passed to the second argument arrow."
  (let-app/ctx ((fanin (ask-fanin))
                (a0 (ctx-injest ca0))
                (a1 (ctx-injest ca1)))
    (funcall fanin a0 a1)))

(defclass arrow-choice-operators (arrow-operators)
  ((choose-either   :initarg :choose-either   :type function :reader choose-either-func)
   (choose-left  :initarg :choose-left  :type function :reader choose-left-func)
   (choose-right :initarg :choose-right :type function :reader choose-right-func)
   (fanin    :initarg :fanin    :type function :reader fanin-func))
  (:documentation
   "Choice, for arrows that support it. A minimal `ARROW-CHOICE-OPPERATORS'
definition requires (OR `LEFT' `CHOOSE-EITHER') in addition to the minimal
requirements for `ARROW-OPERATORS'."))

(defmethod initialize-instance ((obj arrow-choice-operators) &rest args)
  (call-next-method)
  (validate-minimal-arrow-choice-definition obj args)
  (initialize-choose-either   obj args)
  (initialize-choose-left  obj args)
  (initialize-choose-right obj args)
  (initialize-fanin    obj args))

(defun validate-minimal-arrow-choice-definition (obj args)
  (with-init-lookup (args obj)
      (choose-either choose-left)
    (unless (or choose-either choose-left)
      (error "insufficient input for the definition of `ARROW-CHOICE-OPERATORS'"))))

(defun initialize-choose-either   (obj args)
  (with-init-lookup (args obj)
      (choose-either (choose-left a0) (id) (comp a0 a1))
    (setf (slot-value obj 'choose-either)
          (or choose-either
              (lambda (a0 a1)
                (fold #'comp (id)
                      (list
                       (arr #'either-swap)
                       (choose-left a1)
                       (arr #'either-swap)
                       (choose-left a0))))))))

(defun initialize-choose-left  (obj args)
  (with-init-lookup (args obj)
      (choose-left (choose-either a0 a1) id)
    (setf (slot-value obj 'choose-left)
          (or choose-left
              (lambda (a)
                (choose-either a id))))))

(defun initialize-choose-right (obj args)
  (with-init-lookup (args obj)
      (choose-right (choose-either a0 a1) id)
    (setf (slot-value obj 'choose-right)
          (or choose-right
              (lambda (a)
                (choose-either id a))))))

(defun initialize-fanin (obj args)
  (with-init-lookup (args obj)
      (fanin (choose-either a0 a1) (comp a0 a1) (arr f))
    (setf (slot-value obj 'fanin)
          (or fanin
              (lambda (a0 a1)
                (comp (arr #'either-untag) (choose-either a0 a1)))))))
