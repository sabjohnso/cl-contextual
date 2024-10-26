(in-package :cl-user)

(defpackage :contextual-internal-test
  (:use :cl :5am :contextual-internal)
  (:export #:run-all-tests!))

(in-package :contextual-internal-test)

(defun run-all-tests! ()
  (format t "~%~a~%" :contextual-interal-test)
  (run! 'contextual-internal))

(def-suite contextual-internal)

(in-suite  contextual-internal)

(test contextual-p
  "It is a predicate recognizing contextual expressions"
  (is-true (contextual-p (ctx-return 'x)))
  (is-false (contextual-p "something entirely different")))

(test ctx-return
  "It places a value into a contextual expression"
  (is-true (ctx-return 'x))
  (is (eq 'x (ctx-run 'e (ctx-return 'x)))))

(test ctx-injest
  "It places a value into a contextual expression"
  (let ((cx (ctx-injest 'x)))
    (is-true (contextual-p cx))
    (is (eq 'x (ctx-run 'e cx))))
  "It is idempotent"
  (let ((cx (ctx-injest (ctx-injest (ctx-injest (ctx-injest 'x))))))
    (is-true (contextual-p cx))
    (is (eq 'x (ctx-run 'e cx)))))

(test ctx-fmap
  "It applies a function to a value in a contextual expression"
  (let ((cx (ctx-fmap #'symbol-name (ctx-return 'x))))
    (is-true (contextual-p cx))
    (is (equal "X" (ctx-run 'e cx)))))

(test ctx-fapply
  "It applies a function in a contextual expression to a value in a contextual expression"
  (let ((cx (ctx-fapply (ctx-return #'symbol-name) (ctx-return 'x))))
    (is-true (contextual-p cx))
    (is (equal "X" (ctx-run 'e cx)))))

(test ctx-product
  "It combindes to values in contextual expressions into a list containing them in a contextual expression"
  (let ((cx (ctx-product (ctx-return 'x) (ctx-return 'y))))
    (is-true (contextual-p cx))
    (is (equal '(x y) (ctx-run 'e cx)))))

(test ctx-flatmap
  "it applies a contextual expression constructor to a value in a contexual expression
without adding an additional contextual expression layer"
  (let ((cx (ctx-flatmap #'ctx-return (ctx-return 'x))))
    (is-true (contextual-p cx))
    (is (equal 'x (ctx-run 'e cx)))))

(test ctx-flatten
  "It removes one of multiple contextual expression layers"
  (let* ((cx (ctx-return 'x))
         (ccx (ctx-return cx)))
    (is-true (contextual-p cx))
    (is-true (contextual-p ccx))
    (is-true (contextual-p (ctx-run 'e ccx)))
    (is (eq 'x (ctx-run 'e (ctx-flatten ccx))))))

(test ctx-ask
  "It returns a contextual expression where the value is the context"
  (let* ((cx (ctx-ask)))
    (is-true (contextual-p cx))
    (is (eq 'e (ctx-run 'e cx)))))

(test ctx-asks
  "It applies a function to the context in a contextual expression"
  (let ((cx (ctx-asks #'symbol-name)))
    (is-true (contextual-p cx))
    (is (equal "E" (ctx-run 'e cx)))))

(test let-fun/ctx
  "It is a functor binding form."
  (let ((cx (let-fun/ctx ((x (ctx-ask)))
              (1+ x))))
    (is-true (contextual-p cx))
    (is (= 4 (ctx-run 3 cx))))

  "It creates an addditional contextual expression layer for each binding pair"
  (let ((cx (let-fun/ctx ((x (ctx-return 3))
                          (y (ctx-ask)))
              (+ x y))))
    (is-true (contextual-p cx))
    (is-true (contextual-p (ctx-run 4 cx)))
    (is (= 7 (ctx-run 4 (ctx-flatten cx))))))


(test let-app/ctx
  "It is an applicative binding form"
  (let ((cx (let-app/ctx ((x (ctx-ask)))
              (1+ x))))
    (is-true (contextual-p cx))
    (is (= 4 (ctx-run 3 cx))))

  "It creates only creates one layer regardless of the number of binding pairs"
  (let ((cx (let-app/ctx ((x (ctx-return 3))
                          (y (ctx-ask)))
              (+ x y))))
    (is-true (contextual-p cx))
    (is (= 7 (ctx-run 4 cx)))))


(test let-mon/ctx
  "It is a monadic binding form"
  (let ((cx (let-mon/ctx ((x (ctx-asks (lambda (e) (cdr (assoc 'x e)))))
                          (y (ctx-asks (lambda (e) (cdr (assoc 'y e))))))
              (ctx-return (+ x y)))))
    (is-true (contextual-p cx))
    (is (= 7 (ctx-run '((x . 3) (y . 4)) cx)))))
