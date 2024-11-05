(in-package :cl-user)

(defpackage :contextual-derivation
  (:use :cl :contextual-utility :binding-syntax-helpers)
  (:export
   #:defun/flatmap-to-fmap         #:lambda/flatmap-to-fmap
   #:defun/wrap-and-unwrap-to-fmap #:lambda/wrap-and-unwrap-to-fmap
   #:defun/flatmap-to-flatten      #:lambda/flatmap-to-flatten
   #:defun/flatten-to-flatmap      #:lambda/flatten-to-flatmap
   #:defun/flatmap-to-fapply       #:lambda/flatmap-to-fapply
   #:defun/fapply-to-product       #:lambda/fapply-to-product
   #:defun/product-to-fapply       #:lambda/product-to-fapply
   #:defun/fapply-to-fmap          #:lambda/fapply-to-fmap
   #:defun/duplicate-to-extend     #:lambda/duplicate-to-extend
   #:defun/extend-to-duplicate     #:lambda/extend-to-duplicate
   #:defun/ask-to-lookup           #:lambda/ask-to-lookup
   #:defun/lookup-to-ask           #:lambda/lookup-to-ask

   #:derive-functor-interface
   #:derive-applicative-interface
   #:derive-monad-interface
   #:derive-environment-monad-interface
   #:derive-trivial-interface
   #:derive-comonad-interface))

(in-package :contextual-derivation)

(defmacro defun/flatmap-to-fmap (name &key flatmap mreturn)
  "Define a named function that performs functor mapping,
as derived from `FLATMAP' and `MRETURN'"
  (with-syms (f mx x)
    `(defun ,name (,f ,mx)
       (,flatmap (lambda (,x) (,mreturn (funcall ,f ,x))) ,mx))))

(defun lambda/flatmap-to-fmap (&key flatmap mreturn)
  "Return an unnamed function that performs functor mapping,
as derived from `FLATMAP' and `MRETURN'"
  (flet ((flatmap (f mx) (funcall flatmap f mx))
         (mreturn (x) (funcall mreturn x)))
    (lambda (f mx)
      (flatmap (lambda (x) (mreturn (funcall f x))) mx))))

(defmacro defun/wrap-and-unwrap-to-fmap (name &key wrap unwrap)
  "Return and unnamed function that performs functor mapping, as
derived from `WRAP' and `UNWRAP' for a trivial context"
  (with-syms (f mx)
    `(defun ,name (,f ,mx)
       (,wrap (funcall ,f (,unwrap ,mx))))))

(defun lambda/wrap-and-unwrap-to-fmap (&key wrap unwrap)
  "Return and unnamed function that performs functor mapping, as
derived from `WRAP' and `UNWRAP' for a trivial context"
  (flet ((wrap (x) (funcall wrap x))
         (unwrap (mx) (funcall unwrap mx)))
    (lambda (f mx) (wrap (funcall f (unwrap mx))))))


(defmacro defun/flatmap-to-flatten (name &key flatmap)
  "Define a named function that performs monadic flattening,
as derived from `FLATMAP'."
  (with-syms  (mmx mx)
    `(defun ,name (,mmx)
       (,flatmap (lambda (,mx) ,mx) ,mmx))))

(defun lambda/flatmap-to-flatten (&key flatmap)
  "Return an unnamed function that performs monadic flattening,
as derived from `FLATMAP'."
  (flet ((flatmap (f mx) (funcall flatmap f mx)))
    (lambda (mmx)
      (flatmap (lambda (mx) mx) mmx))))

(defmacro defun/flatten-to-flatmap (name &key flatten fmap)
  "Define a named function performing monadic mapping,
as derived from `FLATTEN' and `FMAP'"
  (with-syms (f mx)
    `(defun ,name (,f ,mx)
       (,flatten (,fmap ,f ,mx)))))

(defun lambda/flatten-to-flatmap (&key flatten fmap)
  "Return an unnamed function performing monadic mapping,
as derived from `FLATTEN' and `FMAP'"
  (assert flatten)
  (assert fmap)
  (flet ((flatten (mmx) (funcall flatten mmx))
         (fmap (f mx) (funcall fmap f mx)))
    (lambda (f mx) (flatten (fmap f mx)))))

(defmacro defun/flatmap-to-fapply (name &key flatmap pure)
  "Define a named function that performs  applicative mapping,
as derived from the monad functions `FLATMAP' and `PURE'."
  (assert flatmap)
  (assert pure)
  (with-syms (mf mx f x)
    `(defun ,name (,mf ,mx)
       (,flatmap (lambda (,f) (,flatmap (lambda (,x) (,pure (funcall ,f ,x))) ,mx)) ,mf))))

(defun lambda/flatmap-to-fapply (&key flatmap pure)
  "Return an unnamed function that performs  applicative mapping,
as derived from the monad functions `FLATMAP' and `PURE'."
  (assert flatmap)
  (assert pure)
  (flet ((flatmap (f mx) (funcall flatmap f mx))
         (pure (x) (funcall pure x)))
    (lambda (mf mx)
      (flatmap (lambda (f) (flatmap (lambda (x) (pure (funcall f x))) mx)) mf))))

(defmacro defun/fapply-to-product (name &key fapply pure)
  "Define a named function that performs the applicative
product operation, as derived from the named function `FAPPLY'
and `PURE'."
  (assert fapply)
  (assert pure)
  (with-syms (mx my x y)
    `(defun ,name (,mx ,my)
       (,fapply (,fapply (,pure (lambda (,x) (lambda (,y) (list ,x ,y)))) ,mx) ,my))))

(defun lambda/fapply-to-product (&key fapply pure)
  "Return an unnamed function that performs the applicative
product operation, as derived from `FAPPLY' and `PURE'."
  (assert fapply)
  (assert pure)
  (flet ((fapply (mf mx) (funcall fapply mf mx))
         (pure (x) (funcall pure x)))
    (lambda (mx my)
      (fapply (fapply (pure (lambda (x) (lambda (y) (list x y)))) mx) my))))

(defmacro defun/product-to-fapply (name &key product fmap)
  "Define a named function that performs applicative mapping,
as derived from the named input functions `PRODUCT' and `FMAP'"
  (assert product)
  (assert fmap)
  (with-syms (mf mx fx)
    `(defun ,name (,mf ,mx)
       (,fmap (lambda (,fx)
                (funcall (car ,fx) (cadr ,fx)))
              (,product ,mf ,mx)))))

(defun lambda/product-to-fapply (&key product fmap)
  "Return an unamed function that performs applicative mapping,
as derived from the input functions `PRODUCT' and `FMAP'"
  (assert product)
  (assert fmap)
  (flet ((product (mx my) (funcall product mx my))
         (fmap (f mx) (funcall fmap f mx)))
    (lambda (mf mx)
      (fmap (lambda (fx)
              (funcall (car fx) (cadr fx)))
            (product mf mx)))))

(defmacro defun/fapply-to-fmap (name &key fapply pure)
  "Define a named function the performs functor mapping, as derived
from the named functions `FAPPLY' and `PURE'."
  (assert fapply)
  (assert pure)
  (with-syms (f mx)
    `(defun ,name (,f ,mx)
       (,fapply (,pure ,f) ,mx))))

(defun lambda/fapply-to-fmap (&key fapply pure)
  "Return an unamed function that performS functor mapping, as derived
 from `FAPPLY' and `PURE'."
  (assert fapply)
  (assert pure)
  (lambda (f mx)
    (funcall fapply (funcall pure f) mx)))


(defmacro defun/duplicate-to-extend (name &key duplicate fmap documentation)
  "Defines a function `NAME' that prefoms a comonadic mapping operation.

The resulting function will take two arguments: `F', the function to map,
and `WX', the comonadic input, which will be duplicated before mapping.
The defined function uses the provided comonadic duplicate and fmap functions
name by the inputs `DUPLICATE` and `FMAP`.

Parameters:
  NAME           - The name of the function to define.
  :DUPLICATE     - The name of the comonadic duplciate function for the context.
  :FMAP          - The name of the functorial mapping function for the context.
  :DOCUMENTATION - Optional Documentation for the defined function.
"
  (assert duplicate)
  (assert fmap)

  (let ((documentation (if documentation (list documentation) documentation)))
    (with-syms (f wx)
      `(defun ,name (,f ,wx)
         ,@documentation
         (,fmap ,f (,duplicate ,wx))))))

(defun lambda/duplicate-to-extend (&key duplicate fmap)
  "Creates a lambda function that performs a comonadic 'extend' operation.

This function constructs and returns an anonymous function (lambda) that
takes a function `F' and a comonadic context `WX', and then performs an
extend-like operation by:
  1. Duplicating the context `WX' using the provided `DUPLICATE' function.
  2. Mapping `F' over the duplicated context using the provided `FMAP' function.

Parameters:
  :DUPLICATE - A function that duplicates the comonadic context `WX'.
  :FMAP      - A function that maps `F' over elements in the duplicated
               context.

Usage:
  This returned lambda function can be applied to a function `F' and a
  comonadic context `WX' to perform comonadic mapping.

Example:
  (let ((extend-fn (lambda/duplicate-to-extend :duplicate duplicate-fn :fmap fmap-fn)))
    (funcall extend-fn some-function some-context))"
  (flet ((duplicate (wx) (funcall duplicate wx))
         (fmap (f wx) (funcall fmap f wx)))
    (lambda (f wx)
      (fmap f (duplicate wx)))))


(defmacro defun/extend-to-duplicate (name &key extend documentation)
  "Defines a function `NAME` that duplicates a comonadic context using an 'extend' function.

This macro generates a function that takes a single argument `WX` and applies
the `EXTEND` function to it with an identity transformation, effectively duplicating
the comonadic context. This is useful in comonadic programming where duplicating
or 'extending' the context is required to perform further operations.

Parameters:
  NAME           - The name of the function to define. Must be a symbol.
  :EXTEND        - A function that takes a transformation function and the comonadic
                   context `WX`, and applies the transformation across the context.
                   Must be a symbol.
  :DOCUMENTATION - An optional string describing the purpose or behavior of the
                   generated function, added as a docstring.

Example Usage:
  (defun/extend-to-duplicate my-duplicate :extend extend-fn :documentation \"Duplicates the context using extend-fn\")

This will create a function `my-duplicate` that takes a comonadic context `WX`
and returns a duplicated context by applying `extend-fn` with an identity transformation."

  (assert (symbolp name))
  (assert (symbolp extend))
  (assert (or (null documentation)
              (stringp documentation)))

  (let ((documentation (if documentation (list documentation) nil)))
    (with-syms (wx)
      `(defun ,name (,wx)
         ,@documentation
         (,extend (lambda (,wx) ,wx) ,wx)))))



(defun lambda/extend-to-duplicate (&key extend)
  "Creates a lambda function that perfoms a comonadic 'duplicate' operation using an 'extend' function.

This function constructs and returns an anonymous function (lambda) that,
when given a comonadic context `WX', applies an extend-like operation to
produce a duplicated context. It effectively uses the `EXTEND' function to
create a new context containing the original context at each level.

Parameters:
  :`EXTEND' - A function that performs comonadic mapping to the comonadic context `WX'.

Usage:
  The returned lambda function can be used to duplicate a comonadic
  structure by.

Example:
  (let ((duplicate-fn (lambda/extend-to-duplicate :extend extend-fn)))
    (funcall duplicate-fn some-context))

This will return a duplicated context by applying the `EXTEND' function
to some-context with an identity function."
  (assert (functionp extend))
  (flet ((extend (f wx)
           (funcall extend f wx)))
    (lambda (wx) (extend (lambda (wx) wx) wx))))

(defmacro defun/ask-to-lookup (name &key ask fmap documentation)
  (let ((documentation (if documentation (list documentation) nil)))
    (with-syms (f)
      `(defun ,name (,f)
         ,@documentation
         (,fmap ,f (,ask))))))

(defun lambda/ask-to-lookup (&key ask fmap)
  (flet ((ask () (funcall ask))
         (fmap (f mx) (funcall fmap f mx)))
    (lambda (f)
      (fmap f (ask)))))


(defmacro defun/lookup-to-ask (name &key lookup documentation)
  (let ((documentation (if documentation (list documentation) nil)))
    `(defun ,name ()
       ,@documentation
       (,lookup #'identity))))

(defun lambda/lookup-to-ask (&key lookup)
  (flet ((lookup (f) (funcall lookup f)))
    (lambda ()
      (lookup #'identity))))

(defun make-functor-binding-interface ( tag &key fmap)
  (let ((let*-fun (format-symbol "LET*-FUN/~A" tag))
        (let-fun  (format-symbol "LET-FUN/~A" tag))
        (lift     (format-symbol "~A-LIFT" tag)))
    (values
     (with-syms (bindings body fun arg)
       `((defmacro ,let*-fun  ((&rest ,bindings) &body ,body)
           (make-sequential-functor-binding-ez
            ',let*-fun
            :fmap ',fmap
            :bindings ,bindings
            :body ,body))
         (defmacro ,let-fun ((&rest ,bindings) &rest ,body)
           (make-parallel-functor-binding-ez
            ',let-fun
            :let-sequential ',let*-fun
            :bindings ,bindings
            :body ,body))

         (defun ,lift (,fun ,arg)
           (,fmap ,fun ,arg))))
     `(:let*-fun ,let*-fun :let-fun ,let-fun :lift ,lift))))

(defmacro derive-functor-interface (tag &key fmap)
  (if fmap
      (multiple-value-bind (functor-interface functor-interface-symbols)
          (make-functor-binding-interface tag
                                          :fmap fmap)
        `(progn
           ,@functor-interface
           ',functor-interface-symbols))
      (error "The input was insufficient for deriving a functor interface:~% ~S"
             `((:minimal-definition fmap)
               (:symbols-received (:fmap ,fmap))))))


(defun make-applicative-binding-interface (tag &key fapply fmap)
  "Return code for applicative binding based on the input names for
the fapply and fmap functions."
  (assert (and fapply fmap))
  (let ((let-app (format-symbol "LET-APP/~a" tag)))
    (values
     (with-syms (bindings body)
       `((defmacro ,let-app ((&rest ,bindings) &body ,body)
           (make-parallel-applicative-binding-ez
            ',let-app
            :fmap ',fmap
            :fapply ',fapply
            :bindings ,bindings
            :body ,body))))
     `(:let-app ,let-app))))

(defun make-applicative-lift-interface (tag &key let-app)
  (with-syms (arg0 arg1 arg2 arg3 arg4 arg5 arg6
              x0 x1 x2 x3 x4 x5 x6
              fun)
    (let ((lift2 (format-symbol "~A-LIFT2" tag))
          (lift3 (format-symbol "~A-LIFT3" tag))
          (lift4 (format-symbol "~A-LIFT4" tag))
          (lift5 (format-symbol "~A-LIFT5" tag))
          (lift6 (format-symbol "~A-LIFT6" tag))
          (lift7 (format-symbol "~A-LIFT7" tag)))
      (values
       `((defun ,lift2 (,fun ,arg0 ,arg1)
           (,let-app ((,x0 ,arg0)
                      (,x1 ,arg1))
                     (funcall ,fun ,x0 ,x1)))

         (defun ,lift3 (,fun ,arg0 ,arg1, arg2)
           (,let-app ((,x0 ,arg0)
                      (,x1 ,arg1)
                      (,x2 ,arg2))
                     (funcall ,fun ,x0 ,x1 ,x2)))

         (defun ,lift4 (,fun ,arg0 ,arg1, arg2, arg3)
           (,let-app ((,x0 ,arg0)
                      (,x1 ,arg1)
                      (,x2 ,arg2)
                      (,x3 ,arg3))
                     (funcall ,fun ,x0 ,x1 ,x2 ,x3)))

         (defun ,lift5 (,fun ,arg0 ,arg1 ,arg2 ,arg3 ,arg4)
           (,let-app ((,x0 ,arg0)
                      (,x1 ,arg1)
                      (,x2 ,arg2)
                      (,x3 ,arg3)
                      (,x4 ,arg4))
                     (funcall ,fun ,x0 ,x1 ,x2 ,x3 ,x4)))

         (defun ,lift6 (,fun ,arg0 ,arg1, arg2 ,arg3 ,arg4 ,arg5)
           (,let-app ((,x0 ,arg0)
                      (,x1 ,arg1)
                      (,x2 ,arg2)
                      (,x3 ,arg3)
                      (,x4 ,arg4)
                      (,x5 ,arg5))
                     (funcall ,fun ,x0 ,x1 ,x2 ,x3 ,x4 ,x5)))

         (defun ,lift7 (,fun ,arg0 ,arg1, arg2 ,arg3 ,arg4 ,arg5 ,arg6)
           (,let-app ((,x0 ,arg0)
                      (,x1 ,arg1)
                      (,x2 ,arg2)
                      (,x3 ,arg3)
                      (,x4 ,arg4)
                      (,x5 ,arg5)
                      (,x6 ,arg6))
                     (funcall ,fun ,x0 ,x1 ,x2 ,x3 ,x4 ,x5 ,x6))))
       `(:lift2 ,lift2
         :lift3 ,lift3
         :lift4 ,lift4
         :lift5 ,lift5
         :lift6 ,lift6
         :lift7 ,lift7)))))

(defun make-applicative-interface (tag &key fmap pure fapply product)
  (if (and pure (or fapply (and fmap product)))
      (let* ((fmap-name (or fmap (format-symbol "~A-FMAP" tag)))
             (pure-name pure)
             (fapply-name (or fapply (format-symbol "~A-FAPPLY" tag)))
             (product-name (or product (format-symbol "~A-PRODUCT" tag)))
             (product-def
               (when-missing product
                             `(defun/fapply-to-product ,product-name
                                :fapply ,fapply-name
                                :pure ,pure-name)))
             (fapply-def
               (when-missing fapply
                             `(defun/product-to-fapply ,fapply-name
                                :product ,product-name
                                :fmap ,fmap-name)))
             (fmap-def
               (when-missing fmap
                             `(defun/fapply-to-fmap ,fmap-name
                                :fapply ,fapply-name
                                :pure  ,pure-name))))
        (multiple-value-bind (applicative-binding-macros applicative-binding-symbols)
            (make-applicative-binding-interface tag :fapply fapply-name :fmap fmap-name)
          (multiple-value-bind (functor-binding-macros functor-binding-symbols)
              (make-functor-binding-interface tag :fmap fmap-name)
            (multiple-value-bind (applicative-lift-functions applicative-lift-symbols)
                (make-applicative-lift-interface tag :let-app (getf applicative-binding-symbols :let-app))

              (values
               `(,@product-def
                 ,@fapply-def
                 ,@fmap-def
                 ,@functor-binding-macros
                 ,@applicative-binding-macros
                 ,@applicative-lift-functions)
               `(,@functor-binding-symbols
                 ,@applicative-binding-symbols
                 ,@applicative-lift-symbols
                 :fmap ,fmap-name
                 :pure ,pure-name
                 :fapply ,fapply-name
                 :product ,product-name))))))

      (error "Input was insufficient for deriving an applicative interface:~% ~S"
             `((:minimal-definition (and pure (or fapply (and fmap product))))
               (:symbols-received
                ((:fmap ,fmap)
                 (:pure ,pure)
                 (:fapply ,fapply)
                 (:product ,product)))))))



(defmacro derive-applicative-interface (tag &key fmap pure fapply product)
  (multiple-value-bind (applicative-interface applicative-interface-symbols)
      (make-applicative-interface tag
        :fmap fmap
        :pure pure
        :fapply fapply
        :product product)
    `(progn
       ,@applicative-interface
       ',applicative-interface-symbols)))

(defun make-monad-binding-interface (tag &key flatmap)
  (let ((progn-mon (format-symbol "PROGN-MON/~A" tag))
        (let*-mon (format-symbol "LET*-MON/~A" tag))
        (let-mon (format-symbol "LET-MON/~A" tag)))
    (values
     (with-syms (bindings body)
       `((defmacro ,progn-mon (&body ,body)
           (make-monad-progn-ez ',progn-mon
             :flatmap ',flatmap
             :body ,body))
         (defmacro ,let*-mon ((&rest ,bindings) &body ,body)
           (make-sequential-monad-binding-ez ',let*-mon
             :flatmap ',flatmap
             :monad-progn ',progn-mon
             :bindings ,bindings
             :body ,body))
         (defmacro ,let-mon ((&rest ,bindings) &body ,body)
           (make-parallel-monad-binding-ez ',let-mon
             :flatmap ',flatmap
             :sequential-let-name ',let*-mon
             :monad-progn ',progn-mon
             :bindings ,bindings
             :body ,body))))
     `(:progn-mon ,progn-mon
       :let*-mon  ,let*-mon
        :let-mon ,let-mon))))

(defun make-monad-interface (tag &key fmap pure fapply product mreturn flatmap flatten)
  (if (and (or mreturn pure) (or flatmap (and flatten (or fmap fapply))))
      (let* ((fmap-name (if fmap fmap (format-symbol "~A-FMAP" tag)))
             (pure-name (if pure pure (format-symbol "~A-PURE" tag)))
             (fapply-name (if fapply fapply (format-symbol "~A-FAPPLY" tag)))
             (mreturn-name (if mreturn mreturn (format-symbol "~A-MRETURN" tag)))
             (flatmap-name (if flatmap flatmap (format-symbol "~A-FLATMAP" tag)))
             (flatten-name (if flatten flatten (format-symbol "~A-FLATTEN" tag)))
             (mreturn-def
               (when-missing mreturn
                 (with-syms (x)
                   `(defun ,mreturn-name (,x)
                      (,pure-name ,x)))))
             (pure-def
               (when-missing pure
                 (with-syms (x)
                   `(defun ,pure-name (,x)
                      (,mreturn-name ,x)))))
             (flatmap-def
               (when-missing flatmap
                 `(defun/flatten-to-flatmap ,flatmap-name
                    :flatten ,flatten-name
                    :fmap ,fmap-name)))

             (flatten-def
               (when-missing flatten
                 `(defun/flatmap-to-flatten ,flatten-name
                    :flatmap ,flatmap-name)))

             (fapply-def
               (when-missing (and fapply product)
                 `(defun/flatmap-to-fapply ,fapply-name
                    :flatmap ,flatmap-name
                    :pure ,pure-name))))
        (multiple-value-bind (applicative-interface applicative-interface-symbols)
            (make-applicative-interface tag
              :fmap fmap
              :pure pure-name
              :fapply (if (or fapply product) fapply fapply-name)
              :product product)

          (multiple-value-bind (monad-binding-macros monad-binding-symbols)
              (make-monad-binding-interface tag :flatmap flatmap-name)
            (values
             `(,@mreturn-def
               ,@pure-def
               ,@flatmap-def
               ,@flatten-def
               ,@fapply-def
               ,@applicative-interface
               ,@monad-binding-macros)
             `(:mreturn ,mreturn-name
               :flatmap ,flatmap-name
               :flatten ,flatten-name
               ,@applicative-interface-symbols
               ,@monad-binding-symbols)))))

      (error "The input was insufficient for deriving a monad interface:~% ~S"
             `((:minimal-definition (and (or mreturn pure) (or flatmap (and flatten (or fmap fapply)))))
               (:symbols-received
                ((:fmap ,fmap)
                 (:fapply ,fapply)
                 (:product ,product)
                 (:mreturn ,mreturn)
                 (:flatmap ,flatmap)
                 (:flatten ,flatten)))))))

(defmacro derive-monad-interface (tag &key fmap pure fapply product mreturn flatmap flatten)
  (multiple-value-bind (monad-interface monad-interface-symbols)
      (make-monad-interface tag
        :fmap fmap
        :pure pure
        :fapply fapply
        :product product
        :mreturn mreturn
        :flatmap flatmap
        :flatten flatten)
    `(progn
     ,@monad-interface
     ',monad-interface-symbols)))

(defun make-environment-monad-interface (tag &key ask lookup local fmap pure fapply product mreturn flatmap flatten)
  "Return syntax for the interface for an environment monad"
  (if (and local (or ask lookup))
      (multiple-value-bind (monad-interface monad-interface-symbols)
          (make-monad-interface tag
            :fmap fmap
            :pure pure
            :fapply fapply
            :product product
            :mreturn mreturn
            :flatmap flatmap
            :flatten flatten)
        (let* ((ask-name (if ask ask (format-symbol "~A-ASK" tag)))
               (lookup-name (if lookup lookup (format-symbol " ~A-LOOKUP" tag)))
               (ask-def
                 (when-missing ask
                   `(defun/lookup-to-ask ,ask-name
                      :lookup ,lookup-name)))
               (lookup-def
                 (when-missing lookup
                   `(defun/ask-to-lookup ,lookup-name
                      :ask ,ask-name
                      :fmap ,(getf monad-interface-symbols :fmap)))))
          (values
           `(,@monad-interface
             ,@ask-def
             ,@lookup-def
             )
           `(,@monad-interface-symbols
             :ask ,ask-name
             :lookup ,lookup-name
             :local ,local))))
      (error "The input was insufficient for deriving an environment monad interface:~% ~S"
             `((:minimal-definition (and local (or ask lookup)))
               (:symbols-received
                (:ask ,ask)
                (:lookup ,lookup)
                (:local ,local))))))

(defmacro derive-environment-monad-interface
    (tag &key ask lookup local fmap pure fapply product mreturn flatmap flatten)
  "Derive an interface for an environment monad."
  (multiple-value-bind (environment-monad-interface environment-monad-symbols)
      (make-environment-monad-interface tag
        :ask ask
        :lookup lookup
        :local local
        :fmap fmap
        :pure pure
        :fapply fapply
        :product product
        :mreturn mreturn
        :flatmap flatmap
        :flatten flatten)
    `(progn
       ,@environment-monad-interface
       ',environment-monad-symbols)))
