(in-package :contextual-bare-function)

(eval-when (:load-toplevel :compile-toplevel)
  (defun bf-run (e mx)
    "Run a bare function context with the environment `E'."
    (declare (type function mx))
    (funcall mx e))

  (defun bf-fmap (f mx)
    (lambda (e)
      (funcall f (bf-run e mx))))

  (defun bf-pure (x)
    (lambda (e)
      (declare (ignore e))
      x))

  (defun bf-mreturn (x)
    (lambda (e)
      (declare (ignore e))
      x))

  (defun bf-fapply (mf mx)
    (lambda (e) (funcall (bf-run e mf) (bf-run e mx))))

  (defun bf-product (mx my)
    (lambda (e)
      (list (bf-run e mx) (bf-run e my))))

  (defun bf-flatmap (f mx)
    (lambda (e)
      (bf-run e (funcall f (bf-run e mx)))))

  (defun bf-flatten (mmx)
    (lambda (e)
      (bf-run e (bf-run e mmx))))

  (defun bf-ask ()
    #'identity)

  (defun bf-lookup (f)
    "Return the environment"
    (declare (type function f))
    (lambda (e)
      (funcall f e)))

  (defun bf-local (f mx)
    "Run a bare function with a locally modified environemnt"
    (lambda (e)
      (bf-run (funcall f e) mx)))

  (defun make-bare-function-context ()
    "Return a monadic context for bare functions."
    (make-instance 'monad-environment-operators
      :fmap #'bf-fmap
      :fapply #'bf-fapply
      :product #'bf-product
      :mreturn #'bf-mreturn
      :flatmap #'bf-flatmap
      :flatten #'bf-flatten
      :ask #'bf-ask
      :lookup #'bf-lookup
      :local #'bf-local)))

(defmacro let*-fun/bf ((&rest bindings) &body body)
  (make-sequential-functor-binding-ez
   'let*-fun/bf
   :fmap 'bf-fmap
   :bindings bindings
   :body body))

(defmacro let-fun/bf ((&rest bindings) &body body)
  (make-parallel-functor-binding-ez
   'let-fun/bf
   :let-sequential 'let*-fun/bf
   :bindings bindings
   :body body))

(defmacro let-app/bf ((&rest bindings) &body body)
  (make-parallel-applicative-binding-ez
   'let-app/bf
   :fmap 'bf-fmap
   :fapply 'bf-fapply
   :bindings bindings
   :body body))

(defmacro progn-mon/bf (&body body)
  (make-monad-progn-ez
   'progn-mon/bf
   :flatmap 'bf-flatmap
   :body body))

(defmacro let*-mon/bf ((&rest bindings) &body body)
  (make-sequential-monad-binding-ez
   'let*-mon/bf
   :flatmap 'bf-flatmap
   :monad-progn 'progn-mon/bf
   :bindings bindings
   :body body))

(defmacro let-mon/bf ((&rest bindings) &body body)
  (make-parallel-monad-binding-ez
   'let-mon/bf
   :flatmap 'bf-flatmap
   :sequential-let-name 'let*-mon/bf
   :monad-progn 'progn-mon/bf
   :bindings bindings
   :body body))


(define-constant +bare-function+
  (make-bare-function-context))
