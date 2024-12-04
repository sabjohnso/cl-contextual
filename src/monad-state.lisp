(in-package :contextual)

(defgeneric state-func (monad-state))
(defun ask-state () (ctx-asks #'state-func))
(defun state (f)
  "Embed a simple state action into the monad"
  (let-app/ctx ((state (ask-state)))
    (funcall state f)))


(defgeneric mget-func (monad-state))
(defun ask-mget () (ctx-asks #'mget-func))
(defun mget ()
  "Return the state values from the internals of the monad"
  (let-app/ctx ((mget (ask-mget)))
   (funcall mget)))

(defgeneric mput-func (monad-state))
(defun ask-mput () (ctx-asks #'mput-func))
(defun mput (s)
  "Replace the state inside the monad"
  (let-app/ctx ((mput (ask-mput)))
    (funcall mput s)))

(defgeneric select-func (monad-state))
(defun ask-select ()
  (ctx-asks #'select-func))
(defun select (f)
  "Get a specific component of the state, using the provide projection function"
  (let-app/ctx ((select (ask-select)))
    (funcall select f)))

(defgeneric modify-func (monad-state))
(defun ask-modify () (ctx-asks #'modify-func))
(defun modify (f)
  "Map an old state to a new state in the monad, discarding the old state"
  (let-app/ctx ((modify (ask-modify)))
    (funcall modify f)))

(defclass monad-state-operators (monad-operators)
  ((state :initarg :state :type function :reader state-func)
   (mget :initarg :mget :type function :reader mget-func)
   (mput :initarg :mput :type function :reader mput-func)
   (select :initarg :select :type function :reader select-func)
   (modify :initarg :modify :type function :reader modify-func))
  (:documentation
   "A class describing a collection of functions acting
as monad-state operators, where monad state operators
extend monad operators with the following
  `STATE'  - Embed a simple state action into the monad
  `MGET'   - Return the state values from the internals of the monad
  `MPUT'   - Replace the state inside the monad
  `SELECT' - Get a specific component of the state, using the provide projection function
  `MODIFY' - Map an old state to a new state in the monad, discarding the old state

The minimal definition is the minimal definition of a monad, plus `STATE' or
`MGET' and `MPUT'"))

(defmethod initialize-instance ((obj monad-state-operators) &rest args)
  (call-next-method)
  (validate-minimal-monad-state-definition obj args)
  (initialize-state   obj args)
  (initialize-mget    obj args)
  (initialize-mput    obj args)
  (initialize-select  obj args)
  (initialize-modify  obj args))

(defun validate-minimal-monad-state-definition (obj args)
  (with-init-lookup (args obj)
    (state
     mget
     mput)
    (unless (or state (and  mget mput))
      (error "insufficient input to define monad state operators"))))

(defun initialize-state (obj args)
  (with-init-lookup (args obj)
    (state
     (mget)
     (mput s)
     (flatmap f mx))
    (setf (slot-value obj 'state)
          (or state
              (flet ((bind (mx f) (flatmap f mx)))
                (lambda (f)
                  (bind (mget)   (lambda (s) (destructuring-bind (x s) (funcall f s)
                  (bind (mput s) (lambda (ignored) (declare (ignore ignored))
                    (mreturn x))))))))))))


(defun initialize-mget (obj args)
  (with-init-lookup (args obj)
    ((state f)
     mget)
    (setf (slot-value obj 'mget)
          (or mget (state (lambda (s) (list s s)))))))

(defun initialize-mput (obj args)
  (with-init-lookup (args obj)
    (mput
     (state s))
    (setf (slot-value obj 'mput)
          (or mput (lambda (s)
                     (state (lambda (ignored)
                              (declare (ignore ignored))
                              (list nil s))))))))

(defun initialize-select (obj args)
  (with-init-lookup (args obj)
    (select
     mget
     (fmap f mx))
    (setf (slot-value obj 'select)
          (or select (lambda (f) (fmap f mget))))))

(defun initialize-modify (obj args)
  (with-init-lookup (args obj)
    (modify
     mget
     (mput s)
     (flatmap f mx))
    (setf (slot-value obj 'modify)
          (or modify
              (lambda (f)
                (flatmap
                 (lambda (s)
                   (mput (funcall f s)))
                 mget))))))
