(in-package :cl-user)

(defpackage :contextual
  (:use :cl :binding-syntax-helpers :contextual-internal :contextual-derivation)
  (:export
   #:fmap
   #:pure #:fapply #:product
   #:mreturn #:flatmap #:flatten
   #:wrap #:unwrap
   #:extract #:duplicate #:extend
   #:expel
   #:ask #:lookup #:local

   #:fmap-func
   #:pure-func #:fapply-func #:product-func
   #:flatmap-func #:flatten-func
   #:wrap-func #:unwrap-func
   #:extract-func #:duplicate-func #:extend-func
   #:ask-func #:asks-func #:local-func

   #:let*-fun #:let-fun #:let-app #:let*-mon #:let-mon
   #:lift #:lift2 #:lift3 #:lift4 #:lift5 #:lift6 #:lift7

   #:functor-operators
   #:applicative-operators
   #:monad-operators
   #:comonad-operators
   #:trivial-operators
   #:monad-environment-operators

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

(defgeneric extract-func (context))
(defgeneric duplicate-func (context))
(defgeneric extend-func (context))

(defgeneric ask-func (context))
(defgeneric lookup-func (context))
(defgeneric local-func (context))

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

(defun ask-extract ()
  (ctx-asks #'extract-func))

(defun ask-duplicate ()
  (ctx-asks #'duplicate-func))

(defun ask-extend ()
  (ctx-asks #'extend-func))

(defun ask-ask ()
  (ctx-asks #'ask-func))

(defun ask-lookup ()
  (ctx-asks #'lookup-func))

(defun ask-local ()
  (ctx-asks #'local-func))

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

(defun extract (cwx)
  "Return a contextual expression extracting the value from the embellishment."
  (let-app/ctx ((extract (ask-extract))
                (wx (ctx-injest cwx)))
    (funcall extract wx)))

(defun expel (ctx cwx)
  (ctx-run ctx (extract cwx)))

(defun duplicate (cwx)
  "Return a contextual expression with the context's embellishment duplicated on the input"
  (let-app/ctx ((duplicate (ask-duplicate))
                (wx (ctx-injest cwx)))
    (funcall duplicate wx)))

(defun extend (f cwx)
  "Return a contextual expression with the context's embellishment extended back over
the value extracted from the embellishment."
  (let-app/ctx ((ctx (ctx-ask))
                (extend (ask-extend))
                (wx (ctx-injest cwx)))
    (funcall extend (lambda (wx)
                      (let ((result (funcall f wx)))
                        (if (contextual-p result)
                            (ctx-run ctx result)
                            result)))
             wx)))

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

(defun lift6 (f ma mb mc md me mg)
  "Return the result of mapping the 6-ary function of plain vaues (`F')
over the input values in an applicative context."
  (let-app ((a ma)
            (b mb)
            (c mc)
            (d md)
            (e me)
            (g mg))
    (funcall f a b c d e g)))

(defun lift7 (f ma mb mc md me mg mh)
  "Return the result of mapping the 7-ary function of plain vaues (`F')
over the input values in an applicative context."
  (let-app ((a ma)
            (b mb)
            (c mc)
            (d md)
            (e me)
            (g mg)
            (h mh))
    (funcall f a b c d e g h)))

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
    (let ((pure (get-argument-or-slot-value args :pure obj 'pure)))
    (if pure
        (setf (slot-value obj 'pure) pure)
        (error "`PURE' was not provide and cannot be derived for `APPLICATIVE-OPERATORS'")))

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
          (assert fapply) ;; fapply should be guaranteed at this point
          (assert pure)   ;; pure should be guaranteed at this point
          (setf (slot-value obj 'product) (lambda/fapply-to-product :fapply fapply :pure pure)))))

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

(defclass comonad-operators (functor-operators)
  ((extract :initarg :extract :type function :reader extract-func)
   (duplicate :initarg :duplicate :type function :reader duplicate-func)
   (extend :initarg :extend :type function :reader extend-func)))

(defmethod initialize-instance ((obj comonad-operators) &rest args)
  (let ((extract (get-argument-or-slot-value args :extract obj 'extract)))
    (if extract
        (setf (slot-value obj 'extract) extract)
        (error "`EXTRACT' was not provided and could not be derived fro `TRIVIAL-OPERATORS'")))

  (let ((extend (get-argument-or-slot-value args :extend obj 'extend)))
    (if extend
        (setf (slot-value obj 'extend) extend)
        (let ((fmap (get-argument-or-slot-value args :fmap obj 'fmap))
              (duplicate (get-argument-or-slot-value args :duplicate obj 'duplicate)))
          (if (and fmap duplicate)
              (setf (slot-value obj 'extend)
                    (lambda/duplicate-to-extend :duplicate duplicate :fmap fmap))))))

  (let ((duplicate (get-argument-or-slot-value args :duplicate obj 'duplicate)))
    (if duplicate (setf (slot-value obj 'duplicate) duplicate)
        (let ((extend (get-argument-or-slot-value args :extend obj 'extend)))
          (setf (slot-value obj 'duplicate)
                (lambda/extend-to-duplicate :extend extend)))))

  (let ((extend (get-argument-or-slot-value args :extend obj 'extend))
        (extract (get-argument-or-slot-value args :extract obj 'extract)))
    (setf (slot-value obj 'fmap)
          (lambda (f wx)
            (funcall extend (lambda (wx) (funcall f (funcall extract wx))) wx))))

  (call-next-method)

  (assert (slot-value obj 'fmap))
  (assert (slot-value obj 'extract))
  (assert (slot-value obj 'duplicate))
  (assert (slot-value obj 'extend)))

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
                    (lambda () (funcall lookup #'identity)))
              (error "`ASK' was not provided and cannot be derived for `MONAD-ENVIRONMENT-OPERATORS'")))))
  (let ((lookup (get-argument-or-slot-value args :lookup obj 'lookup)))
    (if lookup
        (setf (slot-value obj 'lookup) lookup)
        (let ((ask (slot-value obj 'ask))
              (fmap (slot-value obj 'fmap)))
          (assert ask)
          (assert fmap)
          (setf (slot-value obj 'lookup)
                (lambda (f) (funcall fmap f (funcall ask)))))))

  (let ((local (getf args :local)))
    (if local (setf (slot-value obj 'local) local)
        (error "`LOCAL' was not provided for `MONAD-ENVIRONMENT-OPERATORS'"))))
