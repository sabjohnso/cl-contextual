(in-package :cl-user)

(defpackage :contextual-test
  (:use :cl :5am :contextual)
  (:export #:run-all-tests!))

(in-package :contextual-test)

(defun run-all-tests! ()
  (run! 'contextual))

(def-suite contextual)

(in-suite contextual)



(defstruct id value)

(defparameter *id-ctx*
  (make-instance 'trivial-operators
    :wrap (lambda (x) (make-id :value x))
    :unwrap (lambda (mx) (id-value mx))
    #||#))

(defun sqr (x)
  (* x x))

(defun sqr-id (x)
  (make-id :value (sqr x)))

(test trivial-context
  (is (equalp (make-id :value 3) (ctx-run *id-ctx* (pure 3))))
  (is (equalp (make-id :value 3) (ctx-run *id-ctx* (mreturn 3))))
  (is (equalp (make-id :value 3) (ctx-run *id-ctx* (wrap 3))))
  (is (equalp 3 (ctx-run *id-ctx* (unwrap (pure 3)))))
  (is (equalp (make-id :value 9) (ctx-run *id-ctx* (fmap #'sqr (pure 3)))))
  (is (equalp (make-id :value 9) (ctx-run *id-ctx* (fapply (pure #'sqr) (pure 3)))))

  (is (equalp (make-id :value 9) (ctx-run *id-ctx* (flatmap #'sqr-id (pure 3)))))

  (is (equalp (make-id :value 3) (ctx-run *id-ctx* (flatten (pure (pure 3))))))

  )
