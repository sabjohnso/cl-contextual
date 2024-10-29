(in-package :cl-user)

(defpackage :contextual-optional-test
  (:use :cl :5am :contextual-utility :contextual :contextual-optional)
  (:export #:run-all-tests))

(in-package :contextual-optional-test)

(defun run-all-tests! ()
  (run! 'optional-tests))

(def-suite optional-tests)

(in-suite optional-tests)

(define-constant context (make-optional-context))

(test fmap
  (is (equalp (just 4) (ctx-run context (fmap #'1+ (just 3)))))
  (is (equalp (none) (ctx-run context (fmap #'1+ (none))))))

(test pure
  (is (equalp (just 'x) (ctx-run context (pure 'x)))))

(test fapply
  (is (equalp (just "X") (ctx-run context (fapply (pure #'symbol-name) (just 'x)))))
  (is (equalp (none) (ctx-run context (fapply (none) (pure 'x)))))
  (is (equalp (none) (ctx-run context (fapply (pure #'symbol-name) (none)))))
  (is (equalp (none) (ctx-run context (fapply (none) (none))))))

(test product
  (is (equalp (just '(x y)) (ctx-run context (product (pure 'x) (pure 'y)))))
  (is (equalp (just '(x y)) (ctx-run context (product (none) (pure 'y)))))
  (is (equalp (none) (ctx-run context (product (pure 'x) (none)))))
  (is (equalp (none) (ctx-run context (product (none) (none))))))

(test flatmap
  (is (equalp (just 3))))
