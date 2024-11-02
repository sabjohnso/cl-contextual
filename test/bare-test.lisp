(in-package :cl-user)
(defpackage :contextual-bare-test
  (:use :cl :5am :contextual :contextual-bare)
  (:export #:run-all-tests!))

(in-package :contextual-bare-test)

(defun run-all-tests! ()
  (run! 'bare-context))

(def-suite bare-context)

(in-suite bare-context)

(defun sqr (x)
  (* x x))

(test functions
  (let ((context (make-bare-context)))
    (is (= 3 (ctx-run context (pure 3))))
    (is (= 3 (ctx-run context (mreturn 3))))
    (is (= 3 (ctx-run context (wrap 3))))
    (is (= 3 (ctx-run context (flatten 3))))
    (is (= 3 (ctx-run context (extract 3))))
    (is (= 3 (ctx-run context (unwrap 3))))

    (is (= 9 (ctx-run context (fmap #'sqr 3))))
    (is (= 9 (ctx-run context (fapply #'sqr 3))))
    (is (= 9 (ctx-run context (flatmap #'sqr 3))))
    (is (= 9 (ctx-run context (extend #'sqr 3))))))

(test binding-syntax
  (let ((context (make-bare-context)))
    (is (= 7
           (ctx-run context
             (let*-mon ((x 3)
                        (y (+ x 1)))
               (mreturn (+ x y))))))
    (is (= 7
           (ctx-run context
             (let-mon ((x 3)
                       (y 4))
               (+ x y)))))
    (is (= 7
           (ctx-run context
             (let-app ((x 3)
                       (y 4))
               (+ x y)))))
    (is (= 7
           (ctx-run context
             (let-fun ((x 3)
                       (y 4))
               (+ x y)))))))
