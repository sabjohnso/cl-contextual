(in-package :contextual)

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


(deftype optional-function ()
  '(or null function))
