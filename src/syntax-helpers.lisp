(in-package :cl-user)

(defpackage :binding-syntax-helpers
  (:use :cl)
  (:export
   #:make-functor-binding
   #:make-applicative-binding
   #:make-monad-progn
   #:make-monad-binding))

(in-package :binding-syntax-helpers)

(defun binding-pair-p (x)
  (and (listp x)
       (= (length x) 2)
       (symbolp (car x))))

(defun binding-pair-list-p (x)
  (and (listp x)
       (every #'binding-pair-p x)))

(deftype binding-pair ()
  '(satisfies binding-pair-p))

(deftype binding-pair-list ()
  '(satisfies binding-pair-list-p))


(defun make-functor-binding (let-name &key fmap binding more-bindings body more-body)
  (declare
   (type symbol let-name fmap)
   (type binding-pair binding)
   (type binding-pair-list more-bindings)
   (type list more-body))

  (if (null more-bindings)
      (destructuring-bind (var expr) binding
        `(,fmap (lambda (,var) ,body ,@more-body) ,expr))
      (let ((body (macroexpand
                   `(,let-name (,@more-bindings)
                       ,body
                       ,@more-body))))
        (macroexpand `(,let-name (,binding)
                                 ,body)))))

(defun make-curried-function (var vars body more-body)
  (if (null vars)
      `(lambda (,var) ,body ,@more-body)
      `(lambda (,var)
         ,(make-curried-function (car vars) (cdr vars) body more-body))))

(defun make-applicative-application (fapply fun expr more-exprs)
  (declare (type symbol fapply)
           (type list fun)
           (type t expr)
           (type list more-exprs))
  (let ((app `(,fapply ,fun ,expr)))
    (if (null more-exprs)
        app
        (make-applicative-binding fapply app (car more-exprs) (cdr more-exprs)))))

(defun make-applicative-binding (let-name &key fmap fapply binding more-bindings body more-body)
  (declare (type symbol let-name fmap fapply)
           (type list binding))
  (destructuring-bind (var expr) binding
    (if (null more-bindings)
        `(,fmap (lambda (,var) ,body ,@more-body) ,expr)
        (let* ((more-vars (mapcar #'car more-bindings))
               (more-exprs (mapcar #'cadr more-bindings))
               (fun (make-curried-function var more-vars body more-body)))
          (make-applicative-application
           fapply
           `(,fmap ,fun ,expr)
           (car more-exprs)
           (cdr more-exprs))))))

(defun make-monad-progn (progn-name &key flatmap body more-body)
  (if (null more-body)
      body
      (let ((ignored (gensym "IGNORED"))
            (expanded-body (macroexpand `(,progn-name ,@more-body))))
        `(,flatmap (lambda (,ignored)
                     (declare (ignore ,ignored))
                     ,expanded-body)
                   ,body))))

(defun make-monad-binding (let-name &key flatmap monad-progn binding more-bindings body more-body)
  (if (null more-bindings)
      (destructuring-bind (var expr) binding
        (let ((body (macroexpand `(,monad-progn ,body ,@more-body))))
          `(,flatmap (lambda (,var) ,body) ,expr)))
      (let ((body (macroexpand `(,let-name (,@more-bindings) ,body ,@more-body))))
        (macroexpand `(,let-name (,binding) ,body)))))
