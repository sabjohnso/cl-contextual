(in-package :contextual)

(defgeneric fmap-func    (context))

(defun ask-fmap ()
  (ctx-asks #'fmap-func))

(defun fmap (f cmx)
  "Given a function and an embelished value, return a
contexual expression with the embellishment of the result
of applyin the function to the value."
  (let-app/ctx ((ctx (ctx-ask))
                (fmap (ask-fmap))
                (mx (ctx-injest cmx)))
    (funcall fmap (lambda (x)
                    (let ((result (funcall f x)))
                      (if (contextual-p result)
                          (ctx-run ctx result)
                          result)))
             mx)))

(defclass functor-operators ()
  ((fmap :initarg :fmap :type function :reader fmap-func)))

(defmethod initialize-instance ((obj functor-operators) &rest args)
  (let ((fmap (get-argument-or-slot-value args :fmap obj 'fmap)))
    (if fmap
        (setf (slot-value obj 'fmap) fmap)
        (error "`FMAP' was not provide and cannot be derived for `FUNCTOR-OPERATORS'"))))
