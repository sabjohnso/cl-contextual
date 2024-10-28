(in-package :cl-user)

(defpackage :contextual
  (:use :cl :binding-syntax-helpers :contextual-internal :contextual-derivation)
  (:export
   #:fmap
   #:pure #:fapply #:product
   #:mreturn #:flatmap #:flatten
   #:wrap #:unwrap

   #:fmap-func
   #:pure-func #:fapply-func #:product-func
   #:flatmap-func #:flatten-func
   #:wrap-func #:unwrap-func
   #:let*-fun #:let-fun #:let-app #:let*-mon #:let-mon
   #:lift #:lift2 #:lift3 #:lift4 #:lift5 #:lift6 #:lift7

   #:functor-operators
   #:applicative-operators
   #:monad-operators
   #:trivial-operators

   #:ctx-run))

(in-package :contextual)

(defgeneric fmap-func    (context))
(defgeneric pure-func    (context))
(defgeneric fapply-func  (context))
(defgeneric product-func (context))
(defgeneric mreturn-func (context))
(defgeneric flatmap-func (context))
(defgeneric flatten-func (context))
(defgeneric wrap-func    (context))
(defgeneric unwrap-func  (context))

(defun ask-fmap ()
  (ctx-asks #'fmap-func))

(defun ask-pure ()
  (ctx-asks #'pure-func))

(defun ask-fapply ()
  (ctx-asks #'fapply-func))

(defun ask-product ()
  (ctx-asks #'product-func))

(defun ask-mreturn ()
  (ctx-asks #'mreturn-func))

(defun ask-flatmap ()
  (ctx-asks #'flatmap-func))

(defun ask-flatten ()
  (ctx-asks #'flatten-func))

(defun ask-wrap ()
  (ctx-asks #'wrap-func))

(defun ask-unwrap ()
  (ctx-asks #'unwrap-func))

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

(defun pure (cx)
  "Return a contextual expression with the input value injected into
the embellishment."
  (let-app/ctx ((pure (ask-pure))
                (x (ctx-injest cx)))
    (funcall pure x)))


(defun fapply (cmf cmx)
  "Given an embellished function and an embellished value,
return a contextual expression with the embellishment of the result
of applying the function to the value"
  (let-app/ctx ((fapply (ask-fapply))
                (mf (ctx-injest cmf))
                (mx (ctx-injest cmx)))
    (funcall fapply mf mx)))

(defun product (cmx cmy)
  "Given two embellished values, return a contextual expression with
an embellished list holding the two values."
  (let-app/ctx ((product (ask-product))
                (mx (ctx-injest cmx))
                (my (ctx-injest cmy)))
    (funcall product mx my)))

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


(defmacro let*-fun (((var expr) &rest more-bindings) body &body more-body)
  (make-sequential-functor-binding
   'let*-fun
   :fmap 'fmap
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))

(defmacro let-fun (((var expr) &rest more-bindings) body &body more-body)
  (make-parallel-functor-binding
   'let-fun
   :let-sequential 'let*-fun
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))

(defmacro let-app (((var expr) &rest more-bindings) body &body more-body)
  (make-parallel-applicative-binding
   'let-app
   :fmap 'fmap
   :fapply 'fapply
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))

(defmacro progn-mon (body &body more-body)
  (make-monad-progn
   'progn-mon
   :flatmap 'flatmap
   :body body
   :more-body more-body))

(defmacro let*-mon (((var expr) &rest more-bindings) body &body more-body)
  (make-sequential-monad-binding
   'let*-mon
   :flatmap 'flatmap
   :monad-progn 'progn-mon
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))

(defmacro let-mon (((var expr) &rest more-bindings) body &body more-body)
  (make-parallel-monad-binding
   'let-mon
   :flatmap 'flatmap
   :sequential-let-name 'let*-mon
   :monad-progn 'progn-mon
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))

(defun lift (f ma)
  "Return the result of mapping function `F' over the input value
in a functor context `MA'"
  (fmap f ma))

(defun lift2 (f ma mb)
  "Return the result of mapping the binary function over plain vaues (`F')
over the input values in an applicative context."
  (let-app ((a ma)
            (b mb))
    (funcall f a b)))

(defun lift3 (f ma mb mc)
    "Return the result of mapping the 3-ary function of plain vaues (`F')
over the input values in an applicative context."
  (let-app ((a ma)
            (b mb)
            (c mc))
    (funcall f a b c)))

(defun lift4 (f ma mb mc md)
  "Return the result of mapping the 4-ary function of plain vaues (`F')
over the input values in an applicative context."
  (let-app ((a ma)
            (b mb)
            (c mc)
            (d md))
    (funcall f a b c d)))

(defun lift5 (f ma mb mc md me)
  "Return the result of mapping the 5-ary function of plain vaues (`F')
over the input values in an applicative context."
  (let-app ((a ma)
            (b mb)
            (c mc)
            (d md)
            (e me))
    (funcall f a b c d e)))

(defun lift6 (f ma mb mc md me mf)
  "Return the result of mapping the 6-ary function of plain vaues (`F')
over the input values in an applicative context."
  (let-app ((a ma)
            (b mb)
            (c mc)
            (d md)
            (e me)
            (f mf))
    (funcall f a b c d e f)))

(defun lift7 (f ma mb mc md me mf mg)
  "Return the result of mapping the 7-ary function of plain vaues (`F')
over the input values in an applicative context."
  (let-app ((a ma)
            (b mb)
            (c mc)
            (d md)
            (e me)
            (f mf)
            (g mg))
    (funcall f a b c d e f f)))

(defun get-argument-or-slot-value (args keyword obj slot-name)
  "Return a value from the arguments or a bound slot with preference to the
the value from the arguments.  If the slot is not bound and the keyword does
not occur in the arguments, return `NIL'."
  (let ((arg (getf args keyword)))
    (or arg (and (slot-boundp obj slot-name) (slot-value obj slot-name)))))

(deftype optional-function ()
  '(or null function))

(defclass functor-operators ()
  ((fmap :initarg :fmap :type optional-function :reader fmap-func)))

(defmethod initialize-instance ((obj functor-operators) &rest args)
  (let ((fmap (get-argument-or-slot-value args :fmap obj 'fmap)))
    (if fmap
        (setf (slot-value obj 'fmap) fmap)
        (error "`FMAP' was not provide and cannot be derived for `FUNCTOR-OPERATORS'"))))

(defclass applicative-operators (functor-operators)
  ((pure :initarg :pure :type optional-function :reader pure-func)
   (fapply :initarg :fapply :type optional-function :reader fapply-func)
   (product :initarg :product :type optional-function :reader product-func)))

(defmethod initialize-instance ((obj applicative-operators) &rest args &key &allow-other-keys)
  (let ((fapply (get-argument-or-slot-value args :fapply obj 'fapply)))
    (if fapply (setf (slot-value obj 'fapply) fapply)
        (let ((product (get-argument-or-slot-value args :product obj 'product))
              (fmap (get-argument-or-slot-value args :fmap obj 'fmap)))
          (if (and product fmap)
              (setf (slot-value obj 'fapply)
                    (lambda/product-to-fapply :product product :fmap fmap))
              (error "`FAPPLY' was not provided and cannot be derived for `APPLICATIVE-OPERATORS'")))))

  (let ((product (get-argument-or-slot-value args :product obj 'product)))
    (if product (setf (slot-value obj 'product) product)
        (let ((fapply (get-argument-or-slot-value args :fapply obj 'fapply))
              (pure (get-argument-or-slot-value args :pure obj 'pure)))
          (if (and fapply pure)
              (setf (slot-value obj 'product)
                    (lambda/fapply-to-product :fapply fapply :pure pure))
              (error "`PURE' was not provide and cannot be derived for `APPLICATIVE-OPERATORS'")))))

  (let ((pure (get-argument-or-slot-value args :pure obj 'pure)))
    (if pure
        (setf (slot-value obj 'pure) pure)
        (error "`PURE' was not provide and cannot be derived for `APPLICATIVE-OPERATORS'")))

  ;; Derive the function for `FMAP'.
  (setf (slot-value obj 'fmap)
        (lambda/fapply-to-fmap
         :fapply (slot-value obj 'fapply)
         :pure (slot-value obj 'pure)))

  (call-next-method))

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
          (if flatmap
              (setf (slot-value obj 'flatten) (lambda/flatmap-to-flatten :flatmap flatmap))
              (error "`FLATTEN' was not provided and cannot be derived for `MONAD-OPERATORS'")))))

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

(defclass trivial-operators (monad-operators)
  ((wrap :initarg :wrap :type function :reader wrap-func)
   (unwrap :initarg :unwrap :type function :reader unwrap-func)))


(defmethod initialize-instance ((obj trivial-operators) &rest args)
  (let ((wrap (get-argument-or-slot-value args :wrap obj 'wrap)))
    (if wrap
        (setf (slot-value obj 'wrap) wrap)
        (let ((mreturn (get-argument-or-slot-value args :mreturn obj 'mreturn))
              (pure (get-argument-or-slot-value args :pure obj 'pure)))
          (if (or mreturn pure)
              (setf (slot-value obj 'wrap) (or mreturn pure))
              (error "`WRAP' was not provided and cannot be derived for `TRIVIAL-OPERATORS'")))))

  (let ((unwrap (get-argument-or-slot-value args :unwrap obj 'unwrap)))
    (if unwrap
        (setf (slot-value obj 'unwrap) unwrap)
        (let ((flatten (get-argument-or-slot-value args :flatten obj 'flatten)))
          (if flatten
              (setf (slot-value obj 'unwrap) flatten)
              (error "`UNWRAP' was not provided and cannot be derived for `TRIVIAL-OPERATORS'")))))

  (setf (slot-value obj 'mreturn) (slot-value obj 'wrap))
  (setf (slot-value obj 'pure) (slot-value obj 'wrap))
  (setf (slot-value obj 'flatten) (slot-value obj 'unwrap))

  (setf (slot-value obj 'fmap)
        (lambda/wrap-and-unwrap-to-fmap
         :wrap (slot-value obj 'wrap)
         :unwrap (slot-value obj 'unwrap)))

  (assert (slot-boundp obj 'wrap))
  (assert (slot-boundp obj 'unwrap))
  (assert (slot-boundp obj 'flatten))
  (assert (slot-boundp obj 'mreturn))
  (assert (slot-boundp obj 'pure))
  (assert (slot-boundp obj 'fmap))

  (call-next-method))
