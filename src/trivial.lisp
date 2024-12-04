(in-package :contextual)


(defgeneric wrap-func    (context))
(defgeneric unwrap-func  (context))
(defun ask-wrap ()
  (ctx-asks #'wrap-func))

(defun ask-unwrap ()
  (ctx-asks #'unwrap-func))

(defun wrap (cx)
  "Return a contextual expression with the input injected into the context's embellishment."
  (let-app/ctx ((wrap (ask-wrap))
                (x (ctx-injest cx)))
    (funcall wrap x)))

(defun unwrap (cmx)
  "Return a contextual expression with the context's embellishment stripped from the input."
  (let-app/ctx ((unwrap (ask-unwrap))
                (mx (ctx-injest cmx)))
    (funcall unwrap mx)))

(defclass trivial-operators (monad-operators comonad-operators)
  ((wrap :initarg :wrap :type function :reader wrap-func)
   (unwrap :initarg :unwrap :type function :reader unwrap-func)))


(defmethod initialize-instance ((obj trivial-operators) &rest args)
  (let ((wrap (get-argument-or-slot-value args :wrap obj 'wrap)))
    (if wrap
        (setf (slot-value obj 'wrap) wrap)
        (let ((mreturn (get-argument-or-slot-value args :mreturn obj 'mreturn))
              (pure (get-argument-or-slot-value args :pure obj 'pure))
              (duplicate (get-argument-or-slot-value args :duplicate obj 'duplicate)))
          (if (or mreturn pure duplicate)
              (setf (slot-value obj 'wrap) (or mreturn pure duplicate))
              (error "`WRAP' was not provided and cannot be derived for `TRIVIAL-OPERATORS'")))))

  (let ((unwrap (get-argument-or-slot-value args :unwrap obj 'unwrap)))
    (if unwrap
        (setf (slot-value obj 'unwrap) unwrap)
        (let ((flatten (get-argument-or-slot-value args :flatten obj 'flatten))
              (extract (get-argument-or-slot-value args :extract obj 'extract)))
          (if (or flatten extract)
              (setf (slot-value obj 'unwrap) (or flatten extract))
              (error "`UNWRAP' was not provided and cannot be derived for `TRIVIAL-OPERATORS'")))))

  (setf (slot-value obj 'mreturn) (slot-value obj 'wrap))
  (setf (slot-value obj 'pure) (slot-value obj 'wrap))
  (setf (slot-value obj 'duplicate) (slot-value obj 'wrap))

  (setf (slot-value obj 'flatten) (slot-value obj 'unwrap))
  (setf (slot-value obj 'extract) (slot-value obj 'unwrap))

  (setf (slot-value obj 'fmap)
        (lambda/wrap-and-unwrap-to-fmap
         :wrap (slot-value obj 'wrap)
         :unwrap (slot-value obj 'unwrap)))

  (let ((wrap (slot-value obj 'wrap)))
    (setf (slot-value obj 'extend)
          (lambda (f wx)
            (funcall wrap (funcall f wx)))))

  (assert (slot-boundp obj 'wrap))
  (assert (slot-boundp obj 'unwrap))
  (assert (slot-boundp obj 'flatten))
  (assert (slot-boundp obj 'mreturn))
  (assert (slot-boundp obj 'pure))
  (assert (slot-boundp obj 'fmap))

  (call-next-method))
