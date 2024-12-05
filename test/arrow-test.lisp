(in-package :cl-user)

(defpackage :contextual-arrow-test
  (:use :cl :5am :contextual)
  (:shadowing-import-from :5am :fail)
  (:export #:run-all-tests!))

(in-package :contextual-arrow-test)

(defun run-all-tests! ()
  (run! 'arrow))

(def-suite arrow)

(in-suite arrow)

(defun sqr (x)
  (* x x))

(defun twc (x)
  (+ x x))

(defun function-compose (f g)
  (declare (type function f g))
  (lambda (arg)
    (funcall f (funcall g arg))))

(defun function-split (f g)
  (lambda (arg)
    (list (funcall f (car arg))
          (funcall g (cadr arg)))))

(defun make-function-arrow-with-split ()
  (make-instance 'arrow-operators
    :comp #'function-compose
    :arr #'identity
    :split #'function-split))



(test function-arrow-with-split
  (let ((context (make-function-arrow-with-split)))
    (is (typep context 'arrow-operators))
    (is (functionp (ctx-run context (id))))
    (is (functionp (ctx-run context (comp #'sqr (id)))))
    (is (= 9 (funcall (ctx-run context (comp #'sqr (id))) 3)))
    (is (functionp (ctx-run context (comp (id) #'sqr))))
    (is (= 9 (funcall (ctx-run context (comp (id) #'sqr)) 3)))
    (is (functionp (ctx-run context (fst #'sqr))))
    (is (equal '(9 x) (funcall (ctx-run context (fst #'sqr)) '(3 x))))
    (is (equal '(x 9) (funcall (ctx-run context (snd #'sqr)) '(x 3))))
    (is (equal '(6 16) (funcall (ctx-run context (split #'twc #'sqr)) '(3 4))))
    (is (equal '(6 9) (funcall (ctx-run context (fanout #'twc #'sqr)) 3)))))


(defun function-fst (f)
  (lambda (arg)
    (list (funcall f (car arg))
          (cadr arg))))

(defun make-function-arrow-with-fst ()
  (make-instance 'arrow-operators
    :comp #'function-compose
    :arr #'identity
    :fst #'function-fst))

(test function-arrow-with-fst
  (let ((context (make-function-arrow-with-fst)))
    (is (typep context 'arrow-operators))
    (is (functionp (ctx-run context (id))))
    (is (functionp (ctx-run context (comp #'sqr (id)))))
    (is (= 9 (funcall (ctx-run context (comp #'sqr (id))) 3)))
    (is (functionp (ctx-run context (comp (id) #'sqr))))
    (is (= 9 (funcall (ctx-run context (comp (id) #'sqr)) 3)))
    (is (functionp (ctx-run context (fst #'sqr))))
    (is (equal '(9 x) (funcall (ctx-run context (fst #'sqr)) '(3 x))))
    (is (equal '(x 9) (funcall (ctx-run context (snd #'sqr)) '(x 3))))
    (is (equal '(6 16) (funcall (ctx-run context (split #'twc #'sqr)) '(3 4))))
    (is (equal '(6 9) (funcall (ctx-run context (fanout #'twc #'sqr)) 3)))))

(defstruct wrapper func)

(defun wrapper-id ()
  (make-wrapper :func #'identity))

(defun wrapper-arr (func)
  (make-wrapper :func func))

(defun wrapper-compose (a0 a1)
  (wrapper-arr (function-compose (wrapper-func a0) (wrapper-func a1))))

(defun wrapper-split (a0 a1)
  (let ((f (wrapper-func a0))
        (g (wrapper-func a1)))
    (wrapper-arr
     (lambda (arg)
       (list (funcall f (car arg))
             (funcall g (cadr arg)))))))

(defun wrapper-fst (a)
  (let ((f (wrapper-func a)))
    (wrapper-arr
     (lambda (arg)
       (list (funcall f (car arg))
             (cadr arg))))))

(defun wrapper-snd (a)
  (let ((f (wrapper-func a)))
    (wrapper-arr
     (lambda (arg)
       (list (car arg)
             (funcall f (cadr arg)))))))

(defun wrapper-fanout (a0 a1)
  (let ((f (wrapper-func a0))
        (g (wrapper-func a1)))
    (wrapper-arr
     (lambda (arg)
       (list (funcall f arg)
             (funcall g arg))))))

(defun wrapper-run (arg a)
  (funcall (wrapper-func a) arg))

(defun make-wrapper-context ()
  (make-instance 'arrow-operators
    :id     (wrapper-id)
    :comp   #'wrapper-compose
    :arr    #'wrapper-arr
    :split  #'wrapper-split
    :fst    #'wrapper-fst
    :snd    #'wrapper-snd
    :fanout #'wrapper-fanout))


(test wrapper-arrow
  (let ((context (make-wrapper-context)))
    (is (typep context 'arrow-operators))
    (is (wrapper-p (ctx-run context (id))))
    (is (wrapper-p (ctx-run context (comp (arr #'sqr) (id)))))
    (is (= 9 (wrapper-run 3 (ctx-run context (comp (arr #'sqr) (id))))))
    (is (wrapper-p (ctx-run context (comp (id) (arr #'sqr)))))
    (is (= 9 (wrapper-run 3 (ctx-run context (comp (id) (arr #'sqr))))))
    (is (wrapper-p (ctx-run context (fst (arr #'sqr)))))
    (is (equal '(9 x) (wrapper-run '(3 x) (ctx-run context (fst (arr #'sqr))))))
    (is (equal '(x 9) (wrapper-run '(x 3) (ctx-run context (snd (arr #'sqr))))))
    (is (equal '(6 16) (wrapper-run '(3 4) (ctx-run context (split (arr #'twc) (arr #'sqr))) )))
     (is (equal '(6 9) (wrapper-run 3 (ctx-run context (fanout (arr #'twc) (arr #'sqr))))))))
