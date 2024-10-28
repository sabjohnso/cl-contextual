(in-package :cl-user)

(defpackage :contextual-test
  (:use :cl :5am :contextual-utility :contextual)
  (:export #:run-all-tests!))

(in-package :contextual-test)



(defun run-all-tests! ()
  (run! 'contextual))

(def-suite contextual)

(in-suite contextual)

(defun sqr (x)
  (* x x))

(defun twc (x)
  (+ x x))

(eval-when (:load-toplevel :compile-toplevel)
 (defun vector-map (f xs)
   (map 'vector f xs)))

(define-constant +vector-ctx+
  (make-instance 'functor-operators
    :fmap #'vector-map))

(test functor-context
  (is-false (typep +vector-ctx+ 'trivial-operators))
  (is-false (typep +vector-ctx+ 'monad-operators))
  (is-false (typep +vector-ctx+ 'applicative-operators))
  (is-true (typep +vector-ctx+ 'functor-operators))
  (let ((mx #(1 2 3 4)))
    (is (equalp (map 'vector #'sqr mx)
                (ctx-run +vector-ctx+
                  (fmap #'sqr #(1 2 3 4))))))
  (is (equalp #(#(4 5) #(5 6))
              (ctx-run +vector-ctx+
                (let*-fun ((x #(1 2))
                             (y #(3 4)))
                    (+ x y)))))
  (is (equalp #(#(4 5) #(5 6))
              (ctx-run +vector-ctx+
                (let-fun ((x #(1 2))
                          (y #(3 4)))
                  (+ x y)))))

  ;; Below, the binding uses `X' bound inside of the
  ;; `LET-FUN' form to compute the expression bound to `Y',
  ;; not the binding to `X' outside the `LET-FUN' form.
  (is (equalp #(#(1 1) #(4 4))
              (ctx-run +vector-ctx+
                (let ((x 0))
                  (declare (ignore x))
                  (let*-fun ((x #(1 2))
                             (y (vector x x)))
                    (* x y))))))

  ;; Below, the parallel binding uses `X' bound outside of the
  ;; `LET-FUN' form to compute the expression bound to `Y'.
  (is (equalp #(#(0 0) #(0 0))
              (ctx-run +vector-ctx+
                (let ((x 0))
                  (let-fun ((x #(1 2))
                            (y (vector x x)))
                    (* x y)))))))

(defstruct id value)

(define-constant +id-ctx+
  (make-instance 'trivial-operators
    :wrap (lambda (x) (make-id :value x))
    :unwrap (lambda (mx) (id-value mx))
    #||#))

(defun sqr-id (x)
  (make-id :value (sqr x)))

(test trivial-context
  (is-true (typep +id-ctx+ 'trivial-operators))
  (is-true (typep +id-ctx+ 'monad-operators))
  (is-true (typep +id-ctx+ 'applicative-operators))
  (is-true (typep +id-ctx+ 'functor-operators))
  (is (equalp (make-id :value 3) (ctx-run +id-ctx+ (pure 3))))
  (is (equalp (make-id :value 3) (ctx-run +id-ctx+ (mreturn 3))))
  (is (equalp (make-id :value 3) (ctx-run +id-ctx+ (wrap 3))))
  (is (equalp 3 (ctx-run +id-ctx+ (unwrap (pure 3)))))
  (is (equalp (make-id :value 9) (ctx-run +id-ctx+ (fmap #'sqr (pure 3)))))
  (is (equalp (make-id :value 9) (ctx-run +id-ctx+ (fapply (pure #'sqr) (pure 3)))))
  (is (equalp (make-id :value 9) (ctx-run +id-ctx+ (flatmap #'sqr-id (pure 3)))))
  (is (equalp (make-id :value 3) (ctx-run +id-ctx+ (flatten (pure (pure 3)))))))

(defun rappend (xs ys)
  (labels ((recur (xs accum)
             (if (null xs) accum
                 (recur (cdr xs) (cons (car xs) accum)))))
    (recur xs ys)))

(define-constant +list-flatten-ctx+
  (make-instance 'monad-operators
    :fmap (lambda (f xs) (mapcar f xs))
    :mreturn (lambda (x) (list x))
    :flatten (lambda (xss)
               (labels ((recur (xss accum)
                          (if (null xss) (reverse accum)
                              (recur (cdr xss) (rappend (car xss) accum)))))
                 (recur xss nil)))))

(test monad-context-flatten
  (is-false (typep +list-flatten-ctx+ 'trivial-operators))
  (is-true (typep +list-flatten-ctx+ 'monad-operators))
  (is-true (typep +list-flatten-ctx+ 'applicative-operators))
  (is-true (typep +list-flatten-ctx+ 'functor-operators))
  (is (equalp '(3) (ctx-run +list-flatten-ctx+ (pure 3))))
  (is (equalp '(3) (ctx-run +list-flatten-ctx+ (mreturn 3))))
  (is (equalp '(9) (ctx-run +list-flatten-ctx+ (fmap #'sqr (mreturn 3)))))
  (is (equalp '(9) (ctx-run +list-flatten-ctx+ (fmap #'sqr (list 3)))))
  (is (equalp '(9) (ctx-run +list-flatten-ctx+ (fapply (pure #'sqr) (pure 3)))))
  (is (equalp '(9) (ctx-run +list-flatten-ctx+ (fapply (list #'sqr) (pure 3)))))
  (is (equalp '(9) (ctx-run +list-flatten-ctx+ (fapply (pure #'sqr) (list 3)))))
  (is (equalp '(9) (ctx-run +list-flatten-ctx+ (fapply (list #'sqr) (list 3)))))
  (is (equalp '(3 3) (ctx-run +list-flatten-ctx+ (flatmap (lambda (x) (list x x)) (mreturn 3)))))
  (is (equalp '(3) (ctx-run +list-flatten-ctx+ (flatmap #'mreturn (mreturn 3)))))
  (is (equalp '(3) (ctx-run +list-flatten-ctx+ (flatten (mreturn (mreturn 3))))))
  (is (equalp '(3) (ctx-run +list-flatten-ctx+ (flatten (mreturn (list 3))))))
  (is (equalp '(3) (ctx-run +list-flatten-ctx+ (flatten (list (mreturn 3))))))
  (is (equalp '(3) (ctx-run +list-flatten-ctx+ (flatten (list (list 3)))))))


(define-constant +list-flatmap-ctx+
  (make-instance 'monad-operators
    :mreturn (lambda (x) (list x))
    :flatmap (lambda (f xs)
               (labels ((recur (xs accum)
                          (if (null xs) (reverse accum)
                              (recur (cdr xs) (rappend (funcall f (car xs)) accum)))))
                 (recur xs nil)))))

(test monad-context-flatmap
  (is-false (typep +list-flatmap-ctx+ 'trivial-operators))
  (is-true (typep +list-flatmap-ctx+ 'monad-operators))
  (is-true (typep +list-flatmap-ctx+ 'applicative-operators))
  (is-true (typep +list-flatmap-ctx+ 'functor-operators))

  (is (equalp '(3) (ctx-run +list-flatmap-ctx+ (pure 3))))
  (is (equalp '(3) (ctx-run +list-flatmap-ctx+ (mreturn 3))))
  (is (equalp '(9) (ctx-run +list-flatmap-ctx+ (fmap #'sqr (mreturn 3)))))
  (is (equalp '(9) (ctx-run +list-flatmap-ctx+ (fmap #'sqr (list 3)))))
  (is (equalp '(9) (ctx-run +list-flatmap-ctx+ (fapply (pure #'sqr) (pure 3)))))
  (is (equalp '(9) (ctx-run +list-flatmap-ctx+ (fapply (list #'sqr) (pure 3)))))
  (is (equalp '(9) (ctx-run +list-flatmap-ctx+ (fapply (pure #'sqr) (list 3)))))
  (is (equalp '(9) (ctx-run +list-flatmap-ctx+ (fapply (list #'sqr) (list 3)))))
  (is (equalp '(3 3) (ctx-run +list-flatmap-ctx+ (flatmap (lambda (x) (list x x)) (mreturn 3)))))
  (is (equalp '(3) (ctx-run +list-flatmap-ctx+ (flatmap #'mreturn (mreturn 3)))))
  (is (equalp '(3) (ctx-run +list-flatmap-ctx+ (flatten (mreturn (mreturn 3))))))
  (is (equalp '(3) (ctx-run +list-flatmap-ctx+ (flatten (mreturn (list 3))))))
  (is (equalp '(3) (ctx-run +list-flatmap-ctx+ (flatten (list (mreturn 3))))))
  (is (equalp '(3) (ctx-run +list-flatmap-ctx+ (flatten (list (list 3)))))))


(define-constant +list-verbose-ctx+
  (make-instance 'monad-operators
    :fmap (lambda (f xs) (mapcar f xs))
    :mreturn (lambda (x) (list x))
    :flatmap (lambda (f xs)
               (labels ((recur (xs accum)
                          (if (null xs) (reverse accum)
                              (recur (cdr xs) (rappend (funcall f (car xs)) accum)))))
                 (recur xs nil)))
    :flatten (lambda (xss)
               (labels ((recur (xss accum)
                          (if (null xss) (reverse accum)
                              (recur (cdr xss) (rappend (car xss) accum)))))
                 (recur xss nil)))))

(test monad-context-verbose
  (is-false (typep +list-verbose-ctx+ 'trivial-operators))
  (is-true (typep +list-verbose-ctx+ 'monad-operators))
  (is-true (typep +list-verbose-ctx+ 'applicative-operators))
  (is-true (typep +list-verbose-ctx+ 'functor-operators))

  (is (equalp '(3) (ctx-run +list-verbose-ctx+ (pure 3))))
  (is (equalp '(3) (ctx-run +list-verbose-ctx+ (mreturn 3))))
  (is (equalp '(9) (ctx-run +list-verbose-ctx+ (fmap #'sqr (mreturn 3)))))
  (is (equalp '(9) (ctx-run +list-verbose-ctx+ (fmap #'sqr (list 3)))))
  (is (equalp '(9) (ctx-run +list-verbose-ctx+ (fapply (pure #'sqr) (pure 3)))))
  (is (equalp '(9) (ctx-run +list-verbose-ctx+ (fapply (list #'sqr) (pure 3)))))
  (is (equalp '(9) (ctx-run +list-verbose-ctx+ (fapply (pure #'sqr) (list 3)))))
  (is (equalp '(9) (ctx-run +list-verbose-ctx+ (fapply (list #'sqr) (list 3)))))
  (is (equalp '(3 3) (ctx-run +list-verbose-ctx+ (flatmap (lambda (x) (list x x)) (mreturn 3)))))
  (is (equalp '(3) (ctx-run +list-verbose-ctx+ (flatmap #'mreturn (mreturn 3)))))
  (is (equalp '(3) (ctx-run +list-verbose-ctx+ (flatten (mreturn (mreturn 3))))))
  (is (equalp '(3) (ctx-run +list-verbose-ctx+ (flatten (mreturn (list 3))))))
  (is (equalp '(3) (ctx-run +list-verbose-ctx+ (flatten (list (mreturn 3))))))
  (is (equalp '(3) (ctx-run +list-verbose-ctx+ (flatten (list (list 3))))))
  (is (equalp '(4 5 5 6)
              (ctx-run +list-verbose-ctx+
                (let*-mon ((x '(1 2))
                           (y '(3 4)))
                  (mreturn (+ x y))))))


  (is (equalp '(4 5 5 6)
              (ctx-run +list-verbose-ctx+
                (let-mon ((x '(1 2))
                          (y '(3 4)))
                  (mreturn (+ x y))))))

  ;; Below, the binding uses `X' bound inside of the
  ;; `LET-FUN' form to compute the expression bound to `Y',
  ;; not the binding to `X' outside the `LET-FUN' form.
  (is (equalp '(4 6)
              (ctx-run +list-verbose-ctx+
                (let ((x 10))
                  (declare (ignore x))
                  (let*-mon ((x '(1 2))
                             (y  (mreturn (+ x 2))))
                    (mreturn (+ x y)))))))

  ;; Below, the parallel binding uses `X' bound outside of the
  ;; `LET-FUN' form to compute the expression bound to `Y'.
  (is (equalp '(3 4)
              (ctx-run +list-verbose-ctx+
                (let ((x 0))
                  (let-mon ((x '(1 2))
                            (y  (mreturn (+ x 2))))
                    (mreturn (+ x y)))))))

  (is (equalp '(4 5 5 6)
              (ctx-run +list-verbose-ctx+
                (let-app ((x '(1 2))
                          (y '(3 4)))
                  (+ x y)))))
  (is (equalp '((4 5) (5 6))
              (ctx-run +list-verbose-ctx+
                (let*-fun ((x '(1 2))
                           (y '(3 4)))
                  (+ x y))))))



(eval-when (:load-toplevel :compile-toplevel)
  (defstruct repeat value)

 (deftype ziplist ()
   '(or list repeat))

 (defun repeat  (x)
   (make-repeat :value x))

 (defun ziplist-reverse (xs)
   (cond ((listp xs) (reverse xs))
         ((repeatp xs) xs)))

 (defun ziplist-fmap (f xs)
   (if (repeat-p xs)
       (repeat (funcall f (repeat-value xs)))
       (mapcar f xs)))

 (defun ziplist-product (xs ys)
   (cond ((and (repeat-p xs) (repeat-p ys))
          (repeat (list (repeat-value xs) (repeat-value ys))))
         ((repeat-p xs)
          (ziplist-fmap (lambda (y) (list (repeat-value xs) y)) ys))
         ((repeat-p ys)
          (ziplist-fmap (lambda (x) (list x (repeat-value ys))) xs))
         (t (mapcar (lambda (x y) (list x y)) xs ys))))

 (defun ziplist-fapply (fs xs)
   (labels ((recur (fs xs accum)
              (cond ((or (null fs) (null xs)) (ziplist-reverse accum))
                    ((and (repeat-p fs) (repeat-p xs))
                     (make-repeat :value (funcall (repeat-value fs) (repeat-value xs))))
                    ((repeat-p fs)
                     (recur fs (cdr xs) (cons (funcall (repeat-value fs) (car xs)) accum)))
                    ((repeat-p xs)
                     (recur (cdr fs) xs (cons (funcall (car fs) (repeat-value xs)) accum)))
                    (t (recur (cdr fs) (cdr xs) (cons (funcall (car fs) (car xs)) accum))))))
     (recur fs xs nil))))

(define-constant +ziplist-fapply-ctx+
    (make-instance 'applicative-operators
      :fapply #'ziplist-fapply
      :pure #'repeat))

(test applicative-context-fapply
  (is-false (typep +ziplist-fapply-ctx+ 'trivial-operators))
  (is-false (typep +ziplist-fapply-ctx+ 'monad-operators))
  (is-true (typep +ziplist-fapply-ctx+ 'applicative-operators))
  (is-true (typep +ziplist-fapply-ctx+ 'functor-operators))
  (is (equalp (repeat 3) (ctx-run +ziplist-fapply-ctx+ (pure 3))))
  (is (equalp (repeat 9) (ctx-run +ziplist-fapply-ctx+ (fmap #'sqr (repeat 3)))))
  (is (equalp '(1 4 9) (ctx-run +ziplist-fapply-ctx+ (fmap #'sqr '(1 2 3)))))
  (is (equalp '(1 4 9) (ctx-run +ziplist-fapply-ctx+ (fapply (pure #'sqr) '(1 2 3)))))
  (is (equalp (repeat 9) (ctx-run +ziplist-fapply-ctx+ (fapply (pure #'sqr) (repeat 3)))))
  (is (equalp '(9 6) (ctx-run +ziplist-fapply-ctx+ (fapply (list #'sqr #'twc) (repeat 3)))))
  (is (equalp '(1 4) (ctx-run +ziplist-fapply-ctx+ (fapply (list #'sqr #'twc) '(1 2)))))
  (is (equalp (repeat '(x y)) (ctx-run +ziplist-fapply-ctx+ (product (pure 'x) (pure 'y)))))
    (is (equalp '(4 6)
       (ctx-run +ziplist-fapply-ctx+
         (let-app ((x '(1 2))
                   (y '(3 4)))
           (+ x y)))))
  (is (equalp '((4 5) (5 6))
              (ctx-run +ziplist-fapply-ctx+
                (let*-fun ((x '(1 2))
                           (y '(3 4)))
                  (+ x y))))))



(define-constant +ziplist-product-ctx+
  (make-instance 'applicative-operators
    :fmap #'ziplist-fmap
    :pure #'repeat
    :product #'ziplist-product))

(test applicative-context-product
  (is-false (typep +ziplist-product-ctx+ 'trivial-operators))
  (is-false (typep +ziplist-product-ctx+ 'monad-operators))
  (is-true (typep +ziplist-product-ctx+ 'applicative-operators))
  (is-true (typep +ziplist-product-ctx+ 'functor-operators))
  (is (equalp (repeat 3) (ctx-run +ziplist-product-ctx+ (pure 3))))
  (is (equalp (repeat 9) (ctx-run +ziplist-product-ctx+ (fmap #'sqr (repeat 3)))))
  (is (equalp '(1 4 9) (ctx-run +ziplist-product-ctx+ (fmap #'sqr '(1 2 3)))))
  (is (equalp '(1 4 9) (ctx-run +ziplist-product-ctx+ (fapply (pure #'sqr) '(1 2 3)))))
  (is (equalp (repeat 9) (ctx-run +ziplist-product-ctx+ (fapply (pure #'sqr) (repeat 3)))))
  (is (equalp '(9 6) (ctx-run +ziplist-product-ctx+ (fapply (list #'sqr #'twc) (repeat 3)))))
  (is (equalp '(1 4) (ctx-run +ziplist-product-ctx+ (fapply (list #'sqr #'twc) '(1 2)))))
  (is (equalp (repeat '(x y)) (ctx-run +ziplist-product-ctx+ (product (pure 'x) (pure 'y)))))
  (is (equalp '(4 6)
       (ctx-run +ziplist-product-ctx+
         (let-app ((x '(1 2))
                   (y '(3 4)))
           (+ x y)))))
  (is (equalp '((4 5) (5 6))
              (ctx-run +ziplist-product-ctx+
                (let*-fun ((x '(1 2))
                           (y '(3 4)))
                  (+ x y))))))
