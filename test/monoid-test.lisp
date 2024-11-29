(in-package :cl-user)

(defpackage :contextual-monoid-test
  (:use :cl :5am :contextual)
  (:shadowing-import-from :5am :fail)
  (:export #:run-all-tests!))

(in-package :contextual-monoid-test)

(defun run-all-tests! ()
  (run! 'monoid))

(def-suite monoid)

(in-suite monoid)

(test placeholder
  (is (= (+ 1 2) 3)))


(defun make-list-monoid-context ()
    (make-instance 'monoid-operators
      :mempty (lambda () nil)
      :mappend #'append))


(test monoid-context
  (let ((context (make-list-monoid-context)))
    (is (equal nil (ctx-run context (mempty))))
    (let ((xs '(a b))
          (ys '(c d)))
      (is (equal xs (ctx-run context (mappend xs (mempty)))))
      (is (equal ys (ctx-run context (mappend (mempty) ys))))
      (is (equal (append xs ys) (ctx-run context (mappend xs ys)))))))


(defun make-addition-monoid-context ()
  (make-instance 'monoid-operators
    :mempty (lambda () 0)
    :mappend (lambda (x y) (+ x y))))

(test addition-monoid-context
  (let ((context (make-addition-monoid-context)))
    (let ((x 3)
          (y 4))
      (is (= x (ctx-run context (mappend x (mempty)))))
      (is (= y (ctx-run context (mappend (mempty) y))))
      (is (= (+ x y) (ctx-run context (mappend x y)))))))
