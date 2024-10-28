(in-package :cl-user)
(defpackage :contextual-internal
  (:use :cl :binding-syntax-helpers)
  (:export #:contextual-p
           #:ctx-run
           #:ctx-return
           #:ctx-injest
           #:ctx-fmap
           #:ctx-fapply
           #:ctx-product
           #:ctx-flatmap
           #:ctx-flatten
           #:ctx-ask
           #:ctx-asks
           #:let*-fun/ctx
           #:let-app/ctx
           #:let*-mon/ctx))

(in-package :contextual-internal)

(defstruct contextual func)

(defun ctx-run (context cx)
  (declare (type contextual cx))
  (funcall (contextual-func cx) context))

(defun ctx-return (x)
  (make-contextual
   :func (lambda (ignored-context)
           (declare (ignore context))
           x)))

(defun ctx-injest (x)
  (if (contextual-p x) x
      (ctx-return x)))

(defun ctx-fmap (f cx)
  (make-contextual
   :func (lambda (context)
           (funcall f (ctx-run context cx)))))

(defun ctx-fapply (cf cx)
  (make-contextual
   :func (lambda (context)
           (funcall (ctx-run context cf)
                    (ctx-run context cx)))))

(defun ctx-product (cx cy)
  (make-contextual
   :func (lambda (context)
           (list (ctx-run context cx)
                 (ctx-run context cy)))))

(defun ctx-flatmap (f cx)
  (make-contextual
   :func (lambda (context)
           (ctx-run context (funcall f (ctx-run context cx))))))

(defun ctx-flatten (ccx)
  (make-contextual
   :func (lambda (context)
           (ctx-run context (ctx-run context ccx)))))

(defun ctx-ask ()
  (make-contextual
   :func (lambda (context) context)))

(defun ctx-asks (func)
  (ctx-fmap func (ctx-ask)))



(defmacro let*-fun/ctx (((var expr) &rest more-bindings) body &body more-body)
  (make-sequential-functor-binding
   'let*-fun/ctx
   :fmap 'ctx-fmap
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))

(defmacro let-app/ctx (((var expr) &rest more-bindings) body &body more-body)
  (make-parallel-applicative-binding
   'let-app/ctx
   :fmap 'ctx-fmap
   :fapply 'ctx-fapply
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))

(defmacro progn-mon/ctx (body &body more-body)
  (make-monad-progn
   'progn-mon/ctx
   :flatmap 'ctx-flatmap
   :body body
   :more-body more-body))

(defmacro let*-mon/ctx (((var expr) &rest more-bindings) body &body more-body)
  (make-sequential-monad-binding
   'let*-mon/ctx
   :monad-progn 'progn-mon/ctx
   :flatmap 'ctx-flatmap
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))
