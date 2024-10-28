(in-package :cl-user)

(defpackage :binding-syntax-helpers-test
  (:use :cl :5am :binding-syntax-helpers)
  (:export #:run-all-tests!))

(in-package :binding-syntax-helpers-test)

(defun run-all-tests! ()
  (format t "~%BINDING-SYNTAX-HELPERS-TEST~%")
  (run! 'binding-syntax-helpers))


(def-suite binding-syntax-helpers)

(in-suite binding-syntax-helpers)

(defmacro let/fun (((var expr) &rest more-bindings) body &body more-body)
  (make-sequential-functor-binding
   'let/fun
   :fmap 'fmap
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))

(test make-sequential-functor-binding
  (is (equal
       '(fmap (lambda (x) (* x x)) 3)
       (macroexpand
         '(let/fun ((x 3))
           (* x x)))))

  (is (equal
       '(fmap (lambda (x) (fmap (lambda (y) (+ x y)) 4)) 3)
       (macroexpand
        '(let/fun ((x 3)
                   (y 4))
          (+ x y))))))


(defmacro let/app (((var expr) &rest more-bindings) body &body more-body)
  (make-parallel-applicative-binding
   'let/app
   :fmap 'fmap
   :fapply 'fapply
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))


(test make-parallel-applicative-binding
  (is (equal
       '(fmap (lambda (x) (* x x)) 3)
       (macroexpand
        '(let/app ((x 3))
          (* x x)))))

  (is (equal
       '(fapply (fmap (lambda (x) (lambda (y) (+ x y))) 3) 4)
       (macroexpand
        '(let/app ((x 3)
                   (y 4))
          (+ x y))))))


(defmacro progn/mon (body &body more-body)
  (make-monad-progn
   'progn/mon
   :flatmap 'flatmap
   :body body
   :more-body more-body))


(defun rewrite-form (form)
  (cond ((and (listp form) (eq (car form) 'lambda))
         `(lambda (ignored)
            ,@(rewrite-form (cddr form))))
        ((and (listp form) (equal (car form) 'ignore))
         '(ignore ignored))
        ((listp form) (mapcar #'rewrite-form form))
        (t form)))


(test make-monad-progn
  (is (equal 7 (macroexpand '(progn/mon 7))))
  (is (equal '(flatmap (lambda (ignored)
                      (declare (ignore ignored))
                      (flatmap (lambda (ignored)
                                 (declare (ignore ignored))
                                 9)
                               8))
               7)
             (rewrite-form (macroexpand '(progn/mon 7 8 9))))))


(defmacro let/mon (((var expr) &rest more-bindings) body &body more-body)
  (make-sequential-monad-binding
   'let/mon
   :flatmap 'flatmap
   :monad-progn 'progn/mon
   :binding `(,var ,expr)
   :more-bindings more-bindings
   :body body
   :more-body more-body))

(test make-sequential-monad-binding
  (is (equal
       '(flatmap (lambda (x) (1+ x)) 3)
       (macroexpand
        '(let/mon ((x 3))
          (1+ x)))))
  (is (equal
       '(flatmap (lambda (x) (flatmap (lambda (y) (+ x y)) 4)) 3)
       (macroexpand
        '(let/mon ((x 3)
                   (y 4))
          (+ x y)))))

  (is (equal
       (rewrite-form
        '(flatmap
          (lambda (x)
            (flatmap
             (lambda (y)
               (flatmap
                (lambda (ignored)
                  (declare (ignore ignored))
                  (* x y))
                (+ x y)))
             4))
          3))

       (rewrite-form
        (macroexpand
         `(let/mon ((x 3)
                    (y 4))
            (+ x y)
            (* x y)))))))
