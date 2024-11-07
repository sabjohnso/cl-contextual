(in-package :cl-user)
(defpackage :contextual-unitary-list-test
  (:use :cl :5am :contextual :contextual-unitary-list)
  (:export :run-all-tests!))

(in-package :contextual-unitary-list-test)

(def-suite unitary-list)

(in-suite unitary-list)

(defun run-all-tests! ()
  (run! 'unitary-list))


(test type
  (is-true (unitary-list-p nil))
  (is-true (unitary-list-p '(x)))
  (is-false (unitary-list-p '(x y)))
  (is-false (unitary-list-p "A pink elephant"))
  (is-true (typep nil 'unitary-list))
  (is-true (typep '(x) 'unitary-list))
  (is-false (typep '(x y) 'unitary-list))
  (is-false (typep "A pink elephant" 'unitary-list)))

(test functions
  (let ((context (make-unitary-list-context)))
    (is (equal '(x) (ctx-run context (pure 'x))))
    (is (equal '("X") (ctx-run context (fmap #'symbol-name (pure 'x)))))
    (is (equal '("X") (ctx-run context (fapply (pure #'symbol-name) (pure 'x)))))
    (is (equal '((x y)) (ctx-run context (product (pure 'x) (pure 'y)))))
    (is (equal '("X") (ctx-run context (flatmap (lambda (x) (mreturn (symbol-name x))) (pure 'x)))))))

(test binding
  (let ((context (make-unitary-list-context)))
    (is (equal '((7)) (ctx-run context
                        (let*-fun ((x (pure 3))
                                   (y (pure (1+ x))))
                          (+ x y)))))

    (is (equal '((7)) (ctx-run context
                        (let-fun ((x (pure 3))
                                  (y (pure 4)))
                          (+ x y)))))

    (is (equal '(7) (ctx-run context
                      (let-app ((x (pure 3))
                                (y (pure 4)))
                        (+ x y)))))

    (is (equal '(7) (ctx-run context
                      (let*-mon ((x (pure 3))
                                 (y (pure (1+ x))))
                        (mreturn (+ x y))))))

    (is (equal '(7) (ctx-run context
                      (let-mon ((x (pure 3))
                                (y (pure 4)))
                        (mreturn (+ x y))))))))
