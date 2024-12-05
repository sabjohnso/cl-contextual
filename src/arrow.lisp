(in-package :contextual)

(defgeneric arr-func (ctx))
(defun ask-arr () (ctx-asks #'arr-func))
(defun arr (func)
  "Lift a function into the arrow context"
  (let-app/ctx ((arr (ask-arr)))
    (funcall arr func)))

(defgeneric split-func (ctx))
(defun ask-split () (ctx-asks #'split-func))
(defun split (carrow0 carrow1)
  "Split the input between the two argument arrow and combine
their output"
  (let-app/ctx ((split (ask-split))
                (arrow0 (ctx-injest carrow0))
                (arrow1 (ctx-injest carrow1)))
    (funcall split arrow0 arrow1)))

(defgeneric fst-func (ctx))
(defun ask-fst () (ctx-asks #'fst-func))
(defun fst (carrow)
  "Send the first component of the input trough the argument
arrwo and copy the rest unchanged to the output"
  (let-app/ctx ((fst (ask-fst))
                (arrow (ctx-injest carrow)))
    (funcall fst arrow)))

(defgeneric snd-func (ctx))
(defun ask-snd () (ctx-asks #'snd-func))
(defun snd (carrow)
  "A mirror of `FST': send the first component of the input
 trough the argumentg arrwo and copy the rest unchanged to
 the output"
  (let-app/ctx ((snd (ask-snd))
                (arrow (ctx-injest carrow)))
    (funcall snd arrow)))

(defgeneric fanout-func (ctx))
(defun ask-fanout () (ctx-asks #'fanout-func))
(defun fanout (carrow0 carrow1)
  "Send the input to both argument arrows and combine their
output."
  (let-app/ctx ((fanout (ask-fanout))
                (arrow0 (ctx-injest carrow0))
                (arrow1 (ctx-injest carrow1)))
    (funcall fanout arrow0 arrow1)))

(defclass arrow-operators (category-operators)
  ((arr :initarg :arr :type function :reader arr-func)
   (fst :initarg :fst :type function :reader fst-func)
   (snd :initarg :snd :type function :reader snd-func)
   (split :initarg :split :type function :reader split-func)
   (fanout :initarg :fanout :type function :reader fanout-func))
  (:documentation
   "A class describing a context for arrow operators with category
operators as a base. A minimal definition requires:
(and `COMP' `ARR' (or `FST' `SPLIT')).  The defaults for the other
 operators may be overridden for efficiency."))

(defmethod initialize-instance ((obj arrow-operators) &rest args)
  (validate-minimal-arrow-definition obj args)
  (initialize-arr      obj args)
  (initialize-category obj args)

  (call-next-method)

  (initialize-split    obj args)
  (initialize-fst      obj args)
  (initialize-snd      obj args)
  (initialize-fanout   obj args))

(defun validate-minimal-arrow-definition (obj args)
  (let ((arr (get-argument-or-slot-value args :arr obj 'arr))
        (fst (get-argument-or-slot-value args :fst obj 'fst))
        (split (get-argument-or-slot-value args :split obj 'split)))
    (when (not (and arr (or fst split)))
      (error "insufficient input to define an arrow context"))))

(defun initialize-arr (obj args)
  (with-init-lookup (args obj)
    (arr)
    (setf (slot-value obj 'arr) arr)))


(defun initialize-split (obj args)
  (with-init-lookup (args obj)
    (split (fst arg) (arr arg) (comp arg0 arg1))
    (setf (slot-value obj 'split)
          (or split
              (let ((swap (arr (lambda (arg) (list (cadr arg) (car arg))))))
                (lambda (arg0 arg1)
                  (comp swap (comp (fst arg1) (comp swap  (fst arg0))))))))))

(defun initialize-fst (obj args)
  (with-init-lookup (args obj)
      (fst id (split arg0 arg1))
    (setf (slot-value obj 'fst)
          (or fst (lambda (a) (split a id))))))

(defun initialize-snd (obj args)
  (with-init-lookup (args obj)
    (snd id (split arg0 arg1))
    (setf (slot-value obj 'snd)
          (or snd (lambda (a) (split id a))))))

(defun initialize-fanout (obj args)
  (with-init-lookup (args obj)
    (fanout (arr f) (split a0 a1) (comp a0 a1))
    (setf (slot-value obj 'fanout)
          (or fanout
              (lambda (a0 a1)
                (comp (split a0 a1)
                      (arr (lambda (x) (list x x)))))))))

(defun initialize-category (obj args)
  (with-init-lookup (args obj)
      (id (arr func))
    (unless id
      (setf (slot-value obj 'id) (arr #'identity)))))
