(in-package :cl-user)
(defpackage :contextual-either-test
  (:use :cl :5am :contextual)
  (:shadowing-import-from :contextual :fail)
  (:export :run-all-tests!))

(in-package :contextual-either-test)

(defun run-all-tests! ()
  (run! 'either))

(def-suite either)
(in-suite either)

(defun sqr (x)
  (* x x))

(defun twc (x)
  (* x x))

(test either-context
  (let ((context (make-either-context)))
    (is (typep context 'monad-fail-operators))
    (is (equalp (right 9) (ctx-run context (fmap #'sqr (right 3)))))
    (is (equalp (right 'x) (ctx-run context (pure 'x))))
    (is (equalp (right 9) (ctx-run context (fapply (pure #'sqr) (pure 3)))))
    (is (equalp (right 9) (ctx-run context (flatmap (lambda (x) (right (sqr x))) (pure 3)))))
    (is (equalp (left "Yikes!") (ctx-run context (fail "Yikes!"))))))

(test either-swap
  (is (equalp (left 'x) (either-swap (right 'x))))
  (is (equalp (right 'x) (either-swap (left 'x)))))

(test either-untag
  (is (eq 'x (either-untag (left 'x))))
  (is (eq 'x (either-untag (right 'x)))))
