(in-package :cl-user)

(defpackage :contextual-bare-function-test
  (:use :cl :5am :contextual :contextual-bare-function)
  (:shadowing-import-from :contextual #:fail)
  (:export #:run-all-tests!))

(in-package :contextual-bare-function-test)

(defun run-all-tests! ()
  (run! 'bare-function))

(def-suite bare-function)

(in-suite bare-function)

(test specific-context-functions
  (is (eq 'x (bf-run 'e (bf-pure 'x))))
  (is (eq 'x (bf-run 'e (bf-mreturn 'x))))
  (is (equal "X" (bf-run 'e (bf-fmap #'symbol-name (bf-pure 'x)))))
  (is (equal "E" (bf-run 'e (bf-fmap #'symbol-name (bf-ask)))))
  (is (equal "E" (bf-run 'e (bf-fapply (bf-pure #'symbol-name) (bf-ask)))))
  (is (equal "EX" (bf-run 'e
                    (bf-flatmap
                     (lambda (x)
                       (let-app/bf ((e (bf-ask)))
                         (concatenate 'string (symbol-name e) (symbol-name x))))
                     (bf-pure 'x)))))
  (is (eq 'e (bf-run 'e
               (bf-flatten (bf-mreturn (bf-ask)))))))

(test general-context-functions
  (let ((context (make-bare-function-context)))
    (is (eq 'x (bf-run 'e (ctx-run context (pure 'x)))))
    (is (eq 'e (bf-run 'e (ctx-run context (bf-ask)))))
    (is (equal "X" (bf-run 'e (ctx-run context (fmap #'symbol-name (pure 'x))))))
    (is (equal '(x e) (bf-run 'e
                        (ctx-run context
                          (product (pure 'x) (bf-ask))))))
    (is (equal "X" (bf-run 'e
                     (ctx-run context
                       (flatmap (lambda (x)
                                  (lambda (e)
                                    (declare (ignore e))
                                    (symbol-name x)))
                                (pure 'x))))))
    (is (eq 'x (bf-run 'e
                 (ctx-run context
                   (flatten (pure (pure 'x)))))))
    (is (equal "E"
               (bf-run 'e
                 (ctx-run context
                   (bf-lookup #'symbol-name)))))
    (is (equal "E"
               (bf-run 'e
                 (ctx-run context
                   (bf-local
                    #'symbol-name
                    (bf-ask))))))))


(test binding-syntax
  (flet ((by-key (name) (lambda (e) (cdr (assoc name e)))))
    (is (= 3
           (bf-run '((x . 1) (y . 2))
             (ctx-run +bare-function+
               (flatten
                (let-fun ((x (bf-lookup (by-key 'x)))
                          (y (bf-lookup (by-key 'y))))
                  (+ x y)))))))
    (is (= 3
          (bf-run '((x . 1) (y . 2))
             (ctx-run +bare-function+
               (let-app ((x (bf-lookup (by-key 'x)))
                         (y (bf-lookup (by-key 'y))))
                 (+ x y))))))

    (is (= 3
           (bf-run '((x . 1) (y . 2))
             (ctx-run +bare-function+
               (let-mon ((x (bf-lookup (by-key 'x)))
                         (y (bf-lookup (by-key 'y))))
                 (mreturn (+ x y)))))))

    (is (= 5
           (bf-run '((x . 1) (y . 2))
             (ctx-run +bare-function+
               (let-mon ((x (lookup (by-key 'x))))
                 (local (lambda (e) (cons `(x . ,(+ x 2)) e))
                        (let-app ((x (lookup (by-key 'x)))
                                  (y (lookup (by-key 'y))))
                          (+ x y))))))))))
