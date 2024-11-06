(in-package :cl-user)


(defpackage :contextual-bare-state
  (:nicknames :bs)
  (:use :cl :contextual :contextual-derivation)
  (:export
   #:bs-run #:bs-exec #:bs-eval
   #:bs-fmap
   #:bs-pure #:bs-fapply #:bs-product
   #:bs-mreturn #:bs-flatmap #:bs-bind #:bs-flatten
   #:bs-mget #:bs-mput #:bs-select #:bs-modify
   #:let*-fun/bs #:let-fun/bs
   #:let-app/bs
   #:progn-mon/bs #:let*-mon/bs #:let-mon/bs))

(in-package :contextual-bare-state)

(defun bs-run (s mx)
  "Runs a stateful computation `MX' on an initial state `S' and
returns the result of calling `MX' with `S'."

  (declare (type function mx))
  (funcall mx s))


(defun bs-exec (s mx)
  "Return the value from the running the the input stateful
calculation, `MX' with the initial state `S'"

  (declare (type function mx))
  (car (bs-run s mx)))

(defun bs-eval (s mx)
  "Return the final state from the running the the input stateful
calculation, `MX' with the initial state `S', ignoring the final value."

  (declare (type function mx))
  (cadr (bs-run s mx)))


(defun bs-mreturn (x)
  "Wraps a value `X' in a state monad: returns a function that, when
given a state `S', returns `X' alongside `S' in a 2-elment list."

  (lambda (s) (list x s)))

(defun bs-fmap (f mx)
  "Applies a function `F' to the result of a stateful computation `MX'
and wrraps the transformed result back in the state monad."

  (declare (type function f mx))
  (lambda (s)
    (destructuring-bind (x s) (bs-run s mx)
      (bs-run s (bs-mreturn (funcall f x))))))

(defun bs-pure (x)
  "Alias for `BS-MRETURN', used to inject a value `X' into the state
context."

  (bs-mreturn x))

(defun bs-fapply (mf mx)
  "Applies a stateful function `MF' to a stateful value `MX',
threading the state through both computations."

  (declare (type function mf mx))
  (lambda (s)
    (destructuring-bind (f s) (bs-run s mf)
      (destructuring-bind (x s) (bs-run s mx)
        (bs-run s (bs-mreturn (funcall f x)))))))

(defun bs-product (mx my)
  "Combines two state computations `MX' and `MY', returning their
results as a list in a single state context."

  (declare (type function mx my))
  (lambda (s)
    (destructuring-bind (x s) (bs-run s mx)
      (destructuring-bind (y s) (bs-run s my)
        (bs-run s (bs-mreturn (list x y)))))))

(defun bs-flatmap (f mx)
  "Chains two state computations, using `F' to produce the next
computation based on `MX''s result."

  (declare (type function f mx))
  (lambda (s)
    (destructuring-bind (x s) (bs-run s mx)
      (bs-run s (funcall f x)))))

(defun bs-bind (mx f)
  "An alias for `BS-FLATMAP' with the arguments swapped, following the
common bind convention in monads."

  (declare (type function mx f))
  (bs-flatmap f mx))

(defun bs-flatten (mmx)
  "Remove one of multiple layers of the state context from the input `MMX'"
  (declare (type function mmx))
  (lambda (s)
    (destructuring-bind (mx s) (bs-run s mmx)
      (bs-run s mx))))

(defun bs-mget ()
  "Retrieves the current state as the result of the computation."

  (lambda (s)
    (bs-run s (bs-mreturn s))))

(defun bs-mput (s)
  "Sets the state to `S', discarding the old state. Returns `NIL'."

  (lambda (s-ignored)
    (declare (ignore s-ignored))
    (bs-run s (bs-mreturn nil))))

(defun bs-modify (f)
  "Applies a function `F' to modify the current state, returning `NIL'
with the updated state."

  (declare (type function f))
  (lambda (s)
    (bs-run (funcall f s) (bs-mreturn nil))))

(defun bs-select (f)
  "Projects a value out of the current state by applying `F' to the
result of `BS-MGET'."

  (lambda (s)
    (list (funcall f s) s)))


(derive-monad-interface bs
  :fmap bs-fmap
  :pure bs-pure
  :fapply bs-fapply
  :product bs-product
  :mreturn bs-mreturn
  :flatmap bs-flatmap
  :flatten bs-flatten)
