(in-package :cl-user)

(defpackage :binding-syntax-helpers
  (:use :cl :contextual-utility)
  (:export
   #:make-monad-progn
   #:make-sequential-functor-binding
   #:make-sequential-monad-binding
   #:make-parallel-monad-binding
   #:make-parallel-applicative-binding
   #:make-parallel-functor-binding

   #:make-sequential-functor-binding-ez
   #:make-parallel-functor-binding-ez
   #:make-parallel-applicative-binding-ez
   #:make-monad-progn-ez
   #:make-sequential-monad-binding-ez
   #:make-parallel-monad-binding-ez))

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

(defun gensym-like (sym)
  (gensym (symbol-name sym)))


(defun make-sequential-functor-binding (let-name &key fmap binding more-bindings body more-body)
  "Return syntax for sequential functor bindings"
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

(defun make-parallel-functor-binding (let-name &key let-sequential binding more-bindings body more-body)
  (declare (ignore let-name))
  (let* ((bindings (cons binding more-bindings))
         (vars (mapcar #'car bindings))
         (exprs (mapcar #'cadr bindings))
         (new-vars (mapcar #'gensym-like vars))
         (rebindings (mapcar #'list vars new-vars))
         (body (macroexpand `(,let-sequential (,@rebindings)
                                              ,body
                                              ,@more-body))))
    `(funcall (lambda (,@new-vars) ,body) ,@exprs)))

(defun make-curried-function (var more-vars body more-body)
  (if (null more-vars)
      `(lambda (,var) ,body ,@more-body)
      `(lambda (,var)
         ,(make-curried-function (car more-vars) (cdr more-vars) body more-body))))

(defun make-applicative-application (fapply fun expr more-exprs)
  (declare (type symbol fapply)
           (type list fun)
           (type t expr)
           (type list more-exprs))
  (let ((app `(,fapply ,fun ,expr)))
    (if (null more-exprs) app
        (make-applicative-application fapply app (car more-exprs) (cdr more-exprs)))))

(defun make-parallel-applicative-binding (let-name &key fmap fapply binding more-bindings body more-body)
  "Return syntax transformed for parallel applicative binding."
  (declare (ignore let-name)
           (type symbol fmap fapply)
           (type list binding))
  (assert (listp binding))
  (assert (= 2 (length binding)))
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
  "Return syntax transformed for a monadic progn"
  (declare (type symbol progn-name flatmap))
  (if (null more-body)
      body
      (let ((ignored (gensym "IGNORED"))
            (expanded-body (macroexpand `(,progn-name ,@more-body))))
        `(,flatmap (lambda (,ignored)
                     (declare (ignore ,ignored))
                     ,expanded-body)
                   ,body))))

(defun make-sequential-monad-binding (let-name &key flatmap monad-progn binding more-bindings body more-body)
  "Return syntax transformed for sequential monadic binding"
  (declare (type symbol let-name flatmap))
  (if (null more-bindings)
      (destructuring-bind (var expr) binding
        (let ((body (macroexpand `(,monad-progn ,body ,@more-body))))
          `(,flatmap (lambda (,var) ,body) ,expr)))
      (let ((body (macroexpand `(,let-name (,@more-bindings) ,body ,@more-body))))
        (macroexpand `(,let-name (,binding) ,body)))))

(defun make-parallel-monad-binding (let-name &key flatmap sequential-let-name monad-progn binding more-bindings body more-body)
  (declare (ignore let-name))
  (if (null more-bindings)
      `(,sequential-let-name (,binding) ,body ,@more-body)
      (flet ((gensym-like (sym) (gensym (symbol-name sym))))
        (let* ((bindings (cons binding more-bindings))
               (vars (mapcar #'car bindings))
               (new-vars (mapcar #'gensym-like vars))
               (exprs (mapcar #'cadr bindings))
               (rebindings (mapcar #'list vars new-vars))
               (body (make-sequential-monad-binding sequential-let-name
                         :flatmap flatmap
                         :monad-progn monad-progn
                         :binding (car rebindings)
                         :more-bindings (cdr rebindings)
                         :body body
                         :more-body more-body)))
          `(funcall (lambda (,@new-vars) ,body)
                    ,@exprs)))))

(defun make-sequential-functor-binding-ez (let-name &key fmap bindings body)
  "Return syntax for sequential functor bindings"
  (declare (type symbol let-name fmap)
           (type list bindings body))
  (assert bindings)
  (assert body)

  (destructuring-bind (binding . more-bindings) bindings
    (destructuring-bind (body . more-body) body
      (make-sequential-functor-binding
       let-name
       :fmap fmap
       :binding binding
       :more-bindings more-bindings
       :body body
       :more-body more-body))))

(defun make-parallel-functor-binding-ez (let-name &key let-sequential bindings body)
  (declare (type symbol let-name let-sequential)
           (type list bindings body))

  (assert (not (null bindings)))
  (assert (not (null body)))

  (destructuring-bind (binding . more-bindings) bindings
    (destructuring-bind (body . more-body) body
      (make-parallel-functor-binding
       let-name
       :let-sequential let-sequential
       :binding binding
       :more-bindings more-bindings
       :body body
       :more-body more-body))))

(defun make-parallel-applicative-binding-ez (let-name &key fmap fapply bindings body)
  (declare (type symbol let-name fmap fapply)
           (type list bindings body))
  (assert (not (null bindings)))
  (assert (not (null body)))

  (destructuring-bind (binding . more-bindings) bindings
    (destructuring-bind (body . more-body) body
      (make-parallel-applicative-binding
       let-name
       :fmap fmap
       :fapply fapply
       :binding binding
       :more-bindings more-bindings
       :body body
       :more-body more-body))))


(defun make-monad-progn-ez (progn-name &key flatmap body)
  (declare (type symbol progn-name flatmap)
           (type list body))
  (assert (not (null body)))
  (make-monad-progn progn-name :flatmap flatmap :body (car body) :more-body (cdr body)))


(defun make-sequential-monad-binding-ez (let-name &key flatmap monad-progn bindings body)
  (declare (type symbol let-name flatmap monad-progn)
           (type list bindings body))
  (assert (not (null body)))
  (if (null bindings)
      (macroexpand `(,monad-progn ,@body))
      (destructuring-bind (binding . more-bindings) bindings
        (destructuring-bind (body . more-body) body
          (make-sequential-monad-binding
             let-name
             :flatmap flatmap
             :monad-progn monad-progn
             :binding binding
             :more-bindings more-bindings
             :body body
             :more-body more-body)))))


(defun make-parallel-monad-binding-ez (let-name &key flatmap sequential-let-name monad-progn bindings body)
  (declare (type symbol let-name flatmap sequential-let-name monad-progn)
           (type list bindings body))
  (assert (not (null body)))
  (if (null bindings)
      (macroexpand `(,monad-progn ,@body))
      (destructuring-bind (binding . more-bindings) bindings
        (destructuring-bind (body . more-body) body
          (make-parallel-monad-binding let-name
           :flatmap flatmap
           :sequential-let-name sequential-let-name
           :monad-progn monad-progn
           :binding binding
           :more-bindings more-bindings
           :body body
           :more-body more-body)))))
