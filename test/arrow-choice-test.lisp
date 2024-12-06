(in-package :cl-user)
(defpackage :contextual-arrow-choice-test
  (:use :cl :5am :contextual)
  (:shadowing-import-from :contextual :fail)
  (:export #:run-all-tests!))

(in-package :contextual-arrow-choice-test)

(defun run-all-tests! ()
  (run! 'arrow-choice))

(defun sqr (x)
  (* x x))

(defun twc (x)
  (+ x x))

(def-suite arrow-choice)
(in-suite arrow-choice)

(defun function-compose (f g)
  (lambda (arg)
    (funcall f (funcall g arg))))

(defun function-split (f g)
  (lambda (arg)
    (list
     (funcall f (car arg))
     (funcall g (cadr arg)))))

(defun function-choose-either (f g)
  (lambda (arg)
    (etypecase arg
      (left (left (funcall f (left-value arg))))
      (right (right (funcall g (right-value arg)))))))

(defun function-run (arg func)
  (funcall func arg))

(defun make-function-arrow-choice ()
  (make-instance 'arrow-choice-operators
    :comp #'function-compose
    :arr #'identity
    :split #'function-split
    :choose-either #'function-choose-either))

(test arrow-choice-operators
  (let ((context (make-function-arrow-choice)))
    (is (typep context 'arrow-choice-operators))
    (ctx-run context (arr #'sqr))

    (let ((f (ctx-run context (choose-either (arr #'sqr) (arr #'twc)))))
      (is (equalp (left 9) (function-run (left 3) f)))
      (is (equalp (right 6) (function-run (right 3) f))))

    (let ((f (ctx-run context (choose-left #'sqr))))
      (is (equalp (left 9) (function-run (left 3) f)))
      (is (equalp (right 3) (function-run (right 3) f))))

    (let ((f (ctx-run context (choose-right #'sqr))))
      (is (equalp (left 3) (function-run (left 3) f)))
      (is (equalp (right 9) (function-run (right 3) f))))

    (let ((f (ctx-run context (fanin (arr #'sqr) (arr #'twc)))))
      (is (= 9 (function-run (left 3) f)))
      (is (= 6 (function-run (right 3) f))))))
