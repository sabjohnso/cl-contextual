(in-package :cl-user)

(defpackage :contextual-list-test
  (:use :cl :5am :contextual :contextual-list)
  (:shadowing-import-from :contextual #:fail)
  (:export #:run-all-tests!))

(in-package :contextual-list-test)

(defun run-all-tests! ()
  (run! 'list-context))

(def-suite list-context)

(in-suite list-context)

(defun sqr (x)
  (* x x))

(test fmap
  (let ((xs '(1 2 3)))
    (is (equal (mapcar #'sqr xs)
               (ctx-run +list+
                 (fmap #'sqr xs))))))

(test pure/mreturn
  (let ((x 'x))
    (is (equal (list x)
               (ctx-run +list+
                 (pure x))))
    (is (equal (list x)
               (ctx-run +list+
                 (mreturn x))))))

(test fapply
  (let ((xs '(1 2))
        (ys '(3 4))
        (func (lambda (x) (lambda (y) (+ x y)))))
    (is (equal (loop for x in xs appending (loop for y in ys collecting (+ x y)))
               (ctx-run +list+
                 (fapply (fmap func xs) ys))))))


(test product
  (let  ((xs '(1 2))
         (ys '(3 4)))
    (is (equal (loop for x in xs
                     appending (loop for y in ys collecting (list x y)))
               (ctx-run +list+
                 (product xs ys))))))

(test flatmap
  (let ((xs '(1 2)))
    (is (equal (loop for x in xs appending (list x x))
               (ctx-run +list+ (flatmap (lambda (x) (list x x)) xs))))))

(test flatten
  (let ((xss '((1 2) (3 4))))
    (is (equal (loop for xs in xss appending xs)
               (ctx-run +list+ (flatten xss))))))

(test monad-fail
  (is (equal '() (ctx-run +list+ (fail "Yikes!")))))

(test monad-plus
  (is (equal '() (ctx-run +list+ (mzero))))
  (let ((xs '(a b c))
        (ys '(d e f)))
    (is (equal xs (ctx-run +list+ (mplus xs (mzero)))))
    (is (equal xs (ctx-run +list+ (mplus (mzero) xs))))
    (is (equal (append xs ys)
               (ctx-run +list+
                 (mplus xs ys))))))
