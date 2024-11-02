(in-package :cl-user)

(defpackage :contextual-derivation
  (:use :cl)
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
   #:defun/extend-to-duplicate     #:lambda/extend-to-duplicate))
(in-package :contextual-derivation)

(defmacro with-syms ((&rest names) &body body)
  (labels ((symbindings (names accum)
             (if (null names)
                 (reverse accum)
                 (destructuring-bind (name . names) names
                   (symbindings names (cons `(,name ',(gensym (symbol-name name))) accum))))))
    `(let (,@(symbindings names nil))
       ,@body)))

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
  (flet ((flatten (mmx) (funcall flatten mmx))
         (fmap (f mx) (funcall fmap f mx)))
    (lambda (f mx) (flatten (fmap f mx)))))

(defmacro defun/flatmap-to-fapply (name &key flatmap pure)
  "Define a named function that performs  applicative mapping,
as derived from the monad functions `FLATMAP' and `PURE'."
  (with-syms (mf mx f x)
    `(defun ,name (,mf ,mx)
       (,flatmap (lambda (,f) (,flatmap (lambda (,x) (,pure (funcall ,f ,x))) ,mx)) ,mf))))

(defun lambda/flatmap-to-fapply (&key flatmap pure)
  "Return an unnamed function that performs  applicative mapping,
as derived from the monad functions `FLATMAP' and `PURE'."
  (flet ((flatmap (f mx) (funcall flatmap f mx))
         (pure (x) (funcall pure x)))
    (lambda (mf mx)
      (flatmap (lambda (f) (flatmap (lambda (x) (pure (funcall f x))) mx)) mf))))

(defmacro defun/fapply-to-product (name &key fapply pure)
  "Define a named function that performs the applicative
product operation, as derived from the named function `FAPPLY'
and `PURE'."
  (with-syms (mx my x y)
    `(defun ,name (,mx ,my)
       (,fapply (,fapply (,pure (lambda (,x) (lambda (,y) (list ,x ,y)))) ,mx) ,my))))

(defun lambda/fapply-to-product (&key fapply pure)
  "Return an unnamed function that performs the applicative
product operation, as derived from `FAPPLY' and `PURE'."
  (flet ((fapply (mf mx) (funcall fapply mf mx))
         (pure (x) (funcall pure x)))
    (lambda (mx my)
      (fapply (fapply (pure (lambda (x) (lambda (y) (list x y)))) mx) my))))

(defmacro defun/product-to-fapply (name &key product fmap)
  "Define a named function that performs applicative mapping,
as derived from the named input functions `PRODUCT' and `FMAP'"
  (with-syms (mf mx fx)
    `(defun ,name (,mf ,mx)
       (,fmap (lambda (,fx)
                (funcall (car ,fx) (cadr ,fx)))
              (,product ,mf ,mx)))))

(defun lambda/product-to-fapply (&key product fmap)
  "Return an unamed function that performs applicative mapping,
as derived from the input functions `PRODUCT' and `FMAP'"
  (flet ((product (mx my) (funcall product mx my))
         (fmap (f mx) (funcall fmap f mx)))
    (lambda (mf mx)
      (fmap (lambda (fx)
              (funcall (car fx) (cadr fx)))
            (product mf mx)))))

(defmacro defun/fapply-to-fmap (name &key fapply pure)
  "Define a named function the performs functor mapping, as derived
from the named functions `FAPPLY' and `PURE'."
  (with-syms (f mx)
    `(defun ,name (,f ,mx)
       (,fapply (,pure ,f) ,mx))))

(defun lambda/fapply-to-fmap (&key fapply pure)
  "Return an unamed function that performS functor mapping, as derived
 from `FAPPLY' and `PURE'."
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
  :EXTEND - A function that performs comonadic mapping to the comonadic context `WX'.

Usage:
  The returned lambda function can be used to duplicate a comonadic
  structure by.

Example:
  (let ((duplicate-fn (lambda/extend-to-duplicate :extend extend-fn)))
    (funcall duplicate-fn some-context))

This will return a duplicated context by applying the `EXTEND' function
to `some-context' with an identity function."
  (assert (functionp extend))
  (flet ((extend (f wx)
           (funcall extend f wx)))
    (lambda (wx) (extend (lambda (wx) wx) wx))))
