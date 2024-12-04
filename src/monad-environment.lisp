(in-package :contextual)

(defgeneric ask-func (context))
(defgeneric lookup-func (context))
(defgeneric local-func (context))

(defun ask-ask ()
  (ctx-asks #'ask-func))

(defun ask-lookup ()
  (ctx-asks #'lookup-func))

(defun ask-local ()
  (ctx-asks #'local-func))

(defun ask ()
  (let-app/ctx ((ask (ask-ask)))
    (funcall ask)))

(defun lookup (f)
  (let-app/ctx ((lookup (ask-lookup)))
    (funcall lookup f)))

(defun local (f cmx)
  (let-app/ctx ((local (ask-local))
                (mx (ctx-injest cmx)))
    (funcall local f mx)))

(defclass monad-environment-operators (monad-operators)
  ((ask    :initarg :ask   :reader ask-func)
   (lookup :initarg :asks  :reader lookup-func)
   (local  :initarg :local :reader local-func)))

(defmethod initialize-instance ((obj monad-environment-operators) &rest args)
  (call-next-method)

  (let ((ask (get-argument-or-slot-value args :ask obj 'ask)))
    (if ask (setf (slot-value obj 'ask) ask)
        (let ((lookup (get-argument-or-slot-value args :lookup obj 'lookup)))
          (if lookup
              (setf (slot-value obj 'ask)
                    (lambda/lookup-to-ask :lookup lookup))
              (error "`ASK' was not provided and cannot be derived for `MONAD-ENVIRONMENT-OPERATORS'")))))

  (let ((lookup (get-argument-or-slot-value args :lookup obj 'lookup)))
    (if lookup
        (setf (slot-value obj 'lookup) lookup)
        (let ((ask (slot-value obj 'ask))
              (fmap (slot-value obj 'fmap)))
          (assert ask)
          (assert fmap)
          (setf (slot-value obj 'lookup)
                (lambda/ask-to-lookup :ask ask :fmap fmap)))))

  (let ((local (getf args :local)))
    (if local (setf (slot-value obj 'local) local)
        (error "`LOCAL' was not provided for `MONAD-ENVIRONMENT-OPERATORS'"))))
