(in-package :contextual)

(defgeneric mzero-func (context))
(defgeneric mplus-func (context))

(defun ask-mzero ()
  (ctx-asks #'mzero-func))

(defun ask-mplus ()
  (ctx-asks #'mplus-func))

(defun mzero ()
  (let-app/ctx ((mzero (ask-mzero)))
    (funcall mzero)))

(defun mplus (cmx cmy)
  (let-app/ctx ((mplus (ask-mplus))
                (mx (ctx-injest cmx))
                (my (ctx-injest cmy)))
    (funcall mplus mx my)))

(defclass monad-plus-operators (monad-fail-operators monoid-operators)
  ((mzero :initarg :mzero :reader mzero-func)
   (mplus :initarg :mplus :reader mplus-func)))

(defmethod initialize-instance ((obj monad-plus-operators) &rest args)
  ;;
  ;; ... set the slots for the monad plus operators
  ;;

  ;; mzero
  (let ((mzero (get-argument-or-slot-value args :mzero obj 'mzero)))
    (setf (slot-value obj 'mzero) mzero))

  ;; mplus
  (let ((mplus (get-argument-or-slot-value args :mplus obj 'mplus)))
    (setf (slot-value obj 'mplus) mplus))

  ;;
  ;; ... set the slots for the monad-fail uperators
  ;;

  ;; fail
  (let ((fail (get-argument-or-slot-value args :fail obj 'fail)))
    (if fail (setf (slot-value obj 'fail) fail)
        (let ((mzero (slot-value obj 'mzero)))
          (setf (slot-value obj 'fail)
                (lambda (str)
                  (declare (ignore str))
                  (funcall mzero))))))

  ;;
  ;; ... set the slots for the monoid operators
  ;;

  ;; mempty
  (let ((mempty (or (get-argument-or-slot-value args :mempty obj 'mempty)
                    (get-argument-or-slot-value args :mzero obj 'mzero))))
    (setf (slot-value obj 'mempty) mempty))

  ;; mappend
  (let ((mappend (or (get-argument-or-slot-value args :mappend obj 'mappend)
                     (get-argument-or-slot-value args :mplus obj 'mplus))))
    (setf (slot-value obj 'mappend) mappend))

  (call-next-method))
