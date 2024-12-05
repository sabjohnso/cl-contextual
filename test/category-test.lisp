(in-package :cl-user)

(defpackage :contextual-category-test
  (:use :cl :5am :contextual)
  (:shadowing-import-from :5am #:fail)
  (:export #:run-all-tests!))

(in-package :contextual-category-test)

(defun run-all-tests! ()
  (run! 'category))

(def-suite category)

(in-suite category)

(defun sqr (x)
  (* x x))

(defun twc (x)
  (+ x x))


(defun run-func (arg func)
  (funcall func arg))

(defun compose (f g)
  (declare (type function f g))
  (lambda (x) (funcall f (funcall g x))))

(defun make-function-category ()
  (make-instance 'category-operators
    :id #'identity
    :comp #'compose))

(test category-function
  (let ((context (make-function-category)))
    (is (typep context 'category-operators))
    (is (functionp (ctx-run context (id))))
    (is (eq 'x (run-func 'x (ctx-run context (id)))))
    (is (equal "X" (run-func 'x (ctx-run context (<<< #'symbol-name (id))))))
    (is (equal "X" (run-func 'x (ctx-run context (<<< (id) #'symbol-name)))))
    (is (equal "X" (run-func 'x (ctx-run context (>>> #'symbol-name (id))))))
    (is (equal "X" (run-func 'x (ctx-run context (>>> (id) #'symbol-name)))))
    (is (equal 36 (run-func 3 (ctx-run context (>>> #'twc #'sqr)))))))

(defstruct simple-function
  (func #'identity :type function))

(defun simple-function-compose (f g)
  (declare (type simple-function f g))
  (make-simple-function
   :func (compose (simple-function-func f)
                  (simple-function-func g))))

(defun simple-function-id ()
  (make-simple-function))

(defun make-simple-function-category ()
  (make-instance 'category-operators
    :id (simple-function-id)
    :comp #'simple-function-compose))

(defun run-simple-function (arg cf)
  (funcall (simple-function-func cf) arg))

(test category-simple-function-context
  (let ((context (make-simple-function-category))
        (sfsqr (make-simple-function :func #'sqr))
        (sftwc (make-simple-function :func #'twc)))
    (is (typep context 'category-operators))
    (is (simple-function-p (ctx-run context (id))))
    (is (eq 'x (run-simple-function 'x (ctx-run context (id)))))
    (is (= 9 (run-simple-function 3 (ctx-run context (ctx-injest sfsqr)))))
    (is (= 36 (run-simple-function 3
                (ctx-run context
                  (<<< sfsqr sftwc)))))
    (is (= 36 (run-simple-function 3
                (ctx-run context
                  (>>> sftwc sfsqr)))))))
