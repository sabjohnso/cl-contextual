(in-package :contextual)

(defgeneric mreturn-func (context))
(defgeneric flatmap-func (context))
(defgeneric flatten-func (context))

(defun ask-mreturn ()
  (ctx-asks #'mreturn-func))

(defun ask-flatmap ()
  (ctx-asks #'flatmap-func))

(defun ask-flatten ()
  (ctx-asks #'flatten-func))

(defun mreturn (cx)
  "Return a contextual expression with the input value injected into
the embellishment."
  (let-app/ctx ((mreturn (ask-mreturn))
                (x (ctx-injest cx)))
    (funcall mreturn x)))

(defun flatmap (f cmx)
  "Return a contextual expression with the input constructor mapped over the
input value, which is already embellished."
  (let-app/ctx ((context (ctx-ask))
                (flatmap (ask-flatmap))
                (mx (ctx-injest cmx)))
    (flet ((f (x)
             (let ((result (funcall f x)))
               (if (contextual-p result)
                          (ctx-run context result)
                          result))))
      (funcall flatmap #'f mx))))

(defun flatten (cmcmx)
  "Return a contexual expression with one layer of the context's embellishment
stripped from the input, which has multiple layers of embellishment."
  (let-app/ctx ((context (ctx-ask))
                (flatten (ask-flatten))
                (fmap (ask-fmap))
                (mcmx (ctx-injest cmcmx)))
    (flet ((expel (cmx) (ctx-run context (ctx-injest cmx))))
      (let ((mmx (funcall fmap #'expel mcmx)))
        (funcall flatten mmx)))))

(defclass monad-operators (applicative-operators)
  ((mreturn :initarg :mreturn :type optional-function :reader mreturn-func)
   (flatmap :initarg :flatmap :type optional-function :reader flatmap-func)
   (flatten :initarg :flatten :type optional-function :reader flatten-func)))

(defmethod initialize-instance ((obj monad-operators) &rest args)
  (let ((flatmap (get-argument-or-slot-value args :flatmap obj 'flatmap)))
    (if flatmap
        (setf (slot-value obj 'flatmap) flatmap)
        (let ((flatten (get-argument-or-slot-value args :flatten obj 'flatten))
              (fmap (get-argument-or-slot-value args :fmap obj 'fmap)))
          (if (and flatten fmap)
              (setf (slot-value obj 'flatmap)
                    (lambda/flatten-to-flatmap :flatten flatten :fmap fmap))
              (error "`FLATMAP' was not provided and cannot be derived for `MONAD-OPERATORS'")))))

  (let ((flatten (get-argument-or-slot-value args :flatten obj 'flatten)))
    (if flatten
        (setf (slot-value obj 'flatten) flatten)
        (let ((flatmap (get-argument-or-slot-value args :flatmap obj 'flatmap)))
          (assert flatmap) ;; flatmap should be guaranteed at this point
          (setf (slot-value obj 'flatten) (lambda/flatmap-to-flatten :flatmap flatmap)))))

  (let ((mreturn (get-argument-or-slot-value args :mreturn obj 'mreturn)))
    (if mreturn
        (setf (slot-value obj 'mreturn) mreturn)
        (let ((pure (get-argument-or-slot-value args :pure obj 'pure)))
          (if pure
              (setf (slot-value obj 'mreturn) pure)
              (error "`MRETURN' was not provided and cannot be derived for `MONAD-OPERATORS'")))))

  (setf (slot-value obj 'fmap)
        (lambda/flatmap-to-fmap
         :flatmap (slot-value obj 'flatmap)
         :mreturn (slot-value obj 'mreturn)))

  (setf (slot-value obj 'fapply)
        (lambda/flatmap-to-fapply
         :flatmap (slot-value obj 'flatmap)
         :pure (slot-value obj 'mreturn)))

  (setf (slot-value obj 'pure) (slot-value obj 'mreturn))

  (assert (slot-boundp obj 'flatmap))
  (assert (slot-boundp obj 'flatten))
  (assert (slot-boundp obj 'mreturn))
  (assert (slot-boundp obj 'pure))
  (assert (slot-boundp obj 'fapply))

  (call-next-method))
