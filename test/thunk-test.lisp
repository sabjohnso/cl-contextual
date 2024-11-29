(in-package :cl-user)

(defpackage :contextual-thunk-test
  (:use :cl :5am :contextual :contextual-thunk)
  (:shadowing-import-from :contextual #:fail)
  (:export #:run-all-tests!))

(in-package :contextual-thunk-test)


(def-suite thunk)

(defun run-all-tests! ()
  (run! 'thunk))

(in-suite thunk)

(test thunk-construction
  (let ((mx (thunk (+ 1 2))))
    (is-true (functionp mx))
    (is (= 3 (funcall mx)))))

(test fmap
  (let ((context (make-thunk-context)))
    (is (equalp "x"
                (ctx-run context
                  (unwrap (fmap #'symbol-name (thunk 'x))))))))
(test pure/mreturn/wrap
  (let ((context (make-thunk-context)))
    (is (eq 'x (funcall (ctx-run context (pure 'x)))))
    (is (eq 'x (funcall (ctx-run context (mreturn 'x)))))
    (is (eq 'x (funcall (ctx-run context (wrap 'x)))))))

(test fapply
  (let ((context (make-thunk-context)))
    (is (equal "X" (funcall (ctx-run context (fapply (pure #'symbol-name) (thunk 'x))))))))

(test flatmap
  (let ((context (make-thunk-context)))
    (is (equal "X" (funcall (ctx-run context (flatmap (lambda (x) (thunk (symbol-name x))) (pure 'x))))))))

(test flatten/unwrap/extract
  (let ((context (make-thunk-context)))
    (is (eq 'x (funcall (ctx-run context (flatten (thunk (thunk 'x)))))))
    (is (eq 'x (funcall (ctx-run context (unwrap (thunk (thunk 'x)))))))
    (is (eq 'x (ctx-run context (extract (thunk 'x)))))))

(test extend
  (let ((context (make-thunk-context)))
    (is (eq 'x (funcall (ctx-run context (extend  #'extract (thunk 'x))))))))

(test duplicate
  (let ((context (make-thunk-context)))
    (is (eq 'x (funcall (funcall (ctx-run context (duplicate (pure 'x)))))))))

(test binding-syntax
  (let ((context (make-thunk-context)))
    (is (= 3
           (funcall
            (ctx-run context
              (let-mon ((x (thunk 1))
                        (y (thunk 2)))
                (mreturn (+ x y)))))))
    (is (= 4
           (funcall
            (ctx-run context
              (let*-mon ((x (thunk 1))
                         (y (thunk (+ x 2))))
                (mreturn (+ x y)))))))
    (is (= 3
           (funcall
            (ctx-run context
              (let-app ((x (thunk 1))
                        (y (thunk 2)))
                (+ x y))))))
    (is (= 3
           (funcall
            (ctx-run context
              (flatten
               (let-fun ((x (thunk 1))
                         (y (thunk 2)))
                 (+ x y)))))))
    (is (= 4
           (funcall
            (ctx-run context
              (flatten
               (let*-fun ((x (thunk 1))
                          (y (thunk (+ x 2))))
                 (+ x y)))))))))
