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
   #:defun/fapply-to-fmap          #:lambda/fapply-to-fmap))

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
  (with-syms (mf mx fx f x)
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
