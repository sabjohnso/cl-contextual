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

(test functor-context
  (let ((context (make-instance 'functor-operators :fmap #'vector-map)))
    (is-false (typep context 'trivial-operators))
    (is-false (typep context 'monad-operators))
    (is-false (typep context 'applicative-operators))
    (is-true (typep context 'functor-operators))
    (let ((mx #(1 2 3 4)))
      (is (equalp (map 'vector #'sqr mx)
                  (ctx-run context
                    (fmap #'sqr #(1 2 3 4))))))
    (is (equalp #(#(4 5) #(5 6))
                (ctx-run context
                  (let*-fun ((x #(1 2))
                             (y #(3 4)))
                    (+ x y)))))
    (is (equalp #(#(4 5) #(5 6))
                (ctx-run context
                  (let-fun ((x #(1 2))
                            (y #(3 4)))
                    (+ x y)))))

    ;; Below, the binding uses `X' bound inside of the
    ;; `LET-FUN' form to compute the expression bound to `Y',
    ;; not the binding to `X' outside the `LET-FUN' form.
    (is (equalp #(#(1 1) #(4 4))
                (ctx-run context
                  (let ((x 0))
                    (declare (ignore x))
                    (let*-fun ((x #(1 2))
                               (y (vector x x)))
                      (* x y))))))

    ;; Below, the parallel binding uses `X' bound outside of the
    ;; `LET-FUN' form to compute the expression bound to `Y'.
    (is (equalp #(#(0 0) #(0 0))
                (ctx-run context
                  (let ((x 0))
                    (let-fun ((x #(1 2))
                              (y (vector x x)))
                      (* x y))))))))

(defstruct id value)


(defun sqr-id (x)
  (make-id :value (sqr x)))

(test trivial-context
  (let ((context
          (make-instance 'trivial-operators
            :wrap (lambda (x) (make-id :value x))
            :unwrap (lambda (mx) (id-value mx))
            #||#)))
    (is-true (typep context 'trivial-operators))
    (is-true (typep context 'monad-operators))
    (is-true (typep context 'applicative-operators))
    (is-true (typep context 'functor-operators))
    (is (equalp (make-id :value 3) (ctx-run context (pure 3))))
    (is (equalp (make-id :value 3) (ctx-run context (mreturn 3))))
    (is (equalp (make-id :value 3) (ctx-run context (wrap 3))))
    (is (equalp 3 (ctx-run context (unwrap (pure 3)))))
    (is (equalp (make-id :value 9) (ctx-run context (fmap #'sqr (pure 3)))))
    (is (equalp (make-id :value 9) (ctx-run context (fapply (pure #'sqr) (pure 3)))))
    (is (equalp (make-id :value 9) (ctx-run context (flatmap #'sqr-id (pure 3)))))
    (is (equalp (make-id :value 3) (ctx-run context (flatten (pure (pure 3))))))))

(test trivial-context-pure-and-flatten
  (let ((context
          (make-instance 'trivial-operators
            :pure (lambda (x) (make-id :value x))
            :flatten (lambda (mx) (id-value mx)))))
    (is-true (typep context 'trivial-operators))
    (is-true (typep context 'monad-operators))
    (is-true (typep context 'applicative-operators))
    (is-true (typep context 'functor-operators))
    (is (equalp (make-id :value 3) (ctx-run context (pure 3))))
    (is (equalp (make-id :value 3) (ctx-run context (mreturn 3))))
    (is (equalp (make-id :value 3) (ctx-run context (wrap 3))))
    (is (equalp 3 (ctx-run context (unwrap (pure 3)))))
    (is (equalp (make-id :value 9) (ctx-run context (fmap #'sqr (pure 3)))))
    (is (equalp (make-id :value 9) (ctx-run context (fapply (pure #'sqr) (pure 3)))))
    (is (equalp (make-id :value 9) (ctx-run context (flatmap #'sqr-id (pure 3)))))
    (is (equalp (make-id :value 3) (ctx-run context (flatten (pure (pure 3))))))
    (is (equalp 3 (ctx-run context (extract (pure 3)))))
    (is (equalp (make-id :value 3) (ctx-run context (extend #'extract (pure 3)))))
    (is (equalp (make-id :value 3) (ctx-run context (extend #'extract (make-id :value 3)))))
    (is (equalp (make-id :value (make-id :value 3))
                (ctx-run context
                  (duplicate (pure 3)))))
    (is (equalp 3 (expel context (pure 3))))))

(test trivial-context-pure-and-extract
  (let ((context
          (make-instance 'trivial-operators
            :pure (lambda (x) (make-id :value x))
            :extract (lambda (mx) (id-value mx)))))
    (is-true (typep context 'trivial-operators))
    (is-true (typep context 'monad-operators))
    (is-true (typep context 'applicative-operators))
    (is-true (typep context 'functor-operators))
    (is (equalp (make-id :value 3) (ctx-run context (pure 3))))
    (is (equalp (make-id :value 3) (ctx-run context (mreturn 3))))
    (is (equalp (make-id :value 3) (ctx-run context (wrap 3))))
    (is (equalp 3 (ctx-run context (unwrap (pure 3)))))
    (is (equalp (make-id :value 9) (ctx-run context (fmap #'sqr (pure 3)))))
    (is (equalp (make-id :value 9) (ctx-run context (fapply (pure #'sqr) (pure 3)))))
    (is (equalp (make-id :value 9) (ctx-run context (flatmap #'sqr-id (pure 3)))))
    (is (equalp (make-id :value 3) (ctx-run context (flatten (pure (pure 3))))))
    (is (equalp 3 (ctx-run context (extract (pure 3)))))
    (is (equalp (make-id :value 3) (ctx-run context (extend #'extract (pure 3)))))
    (is (equalp (make-id :value 3) (ctx-run context (extend #'extract (make-id :value 3)))))
    (is (equalp (make-id :value (make-id :value 3))
                (ctx-run context
                  (duplicate (pure 3)))))))

(test comonad-context-with-duplicate
  (let ((context
          (make-instance 'comonad-operators
            :fmap (lambda (f wx) (cons (funcall f (car wx))
                                       (cdr wx)))
            :extract #'car
            :duplicate (lambda (wx)
                         (cons wx (cdr wx))))))
    (is (equal (ctx-run context
                 (extend (lambda (wx) (sqr (car wx))) `(3 . 4)))
               `(9 . 4)))
    (is (equal 3 (ctx-run context
                   (extract '(3 . 4)))))
    (is (equal '((3 . 4) . 4)
               (ctx-run context
                 (duplicate `(3 . 4)))))))

(test comonad-context-with-extend
  (let ((context
          (make-instance 'comonad-operators
            :extract #'car
            :extend (lambda (f wx)
                      (cons (funcall f wx) (cdr wx))))))
    (is (equal (ctx-run context (extend (lambda (wx) (sqr (car wx))) `(3 . 4))) `(9 . 4)))
    (is (equal 3 (ctx-run context (extract '(3 . 4)))))
    (is (equal '((3 . 4) . 4) (ctx-run context (duplicate `(3 . 4)))))))

(defun rappend (xs ys)
  (labels ((recur (xs accum)
             (if (null xs) accum
                 (recur (cdr xs) (cons (car xs) accum)))))
    (recur xs ys)))

(test monad-context-flatten
  (let ((context
          (make-instance 'monad-operators
            :fmap (lambda (f xs) (mapcar f xs))
            :mreturn (lambda (x) (list x))
            :flatten (lambda (xss)
                       (labels ((recur (xss accum)
                                  (if (null xss) (reverse accum)
                                      (recur (cdr xss) (rappend (car xss) accum)))))
                         (recur xss nil))))))
    (is-false (typep context 'trivial-operators))
    (is-true (typep context 'monad-operators))
    (is-true (typep context 'applicative-operators))
    (is-true (typep context 'functor-operators))
    (is (equalp '(3) (ctx-run context (pure 3))))
    (is (equalp '(3) (ctx-run context (mreturn 3))))
    (is (equalp '(9) (ctx-run context (fmap #'sqr (mreturn 3)))))
    (is (equalp '(9) (ctx-run context (fmap #'sqr (list 3)))))
    (is (equalp '(9) (ctx-run context (fapply (pure #'sqr) (pure 3)))))
    (is (equalp '(9) (ctx-run context (fapply (list #'sqr) (pure 3)))))
    (is (equalp '(9) (ctx-run context (fapply (pure #'sqr) (list 3)))))
    (is (equalp '(9) (ctx-run context (fapply (list #'sqr) (list 3)))))
    (is (equalp '(3 3) (ctx-run context (flatmap (lambda (x) (list x x)) (mreturn 3)))))
    (is (equalp '(3) (ctx-run context (flatmap #'mreturn (mreturn 3)))))
    (is (equalp '(3) (ctx-run context (flatten (mreturn (mreturn 3))))))
    (is (equalp '(3) (ctx-run context (flatten (mreturn (list 3))))))
    (is (equalp '(3) (ctx-run context (flatten (list (mreturn 3))))))
    (is (equalp '(3) (ctx-run context (flatten (list (list 3))))))))

(test monad-context-flatten-with-pure
  (let ((context
          (make-instance 'monad-operators
            :fmap (lambda (f xs) (mapcar f xs))
            :pure (lambda (x) (list x))
            :flatten (lambda (xss)
                       (labels ((recur (xss accum)
                                  (if (null xss) (reverse accum)
                                      (recur (cdr xss) (rappend (car xss) accum)))))
                         (recur xss nil))))))
    (is-false (typep context 'trivial-operators))
    (is-true (typep context 'monad-operators))
    (is-true (typep context 'applicative-operators))
    (is-true (typep context 'functor-operators))
    (is (equalp '(3) (ctx-run context (pure 3))))
    (is (equalp '(3) (ctx-run context (mreturn 3))))
    (is (equalp '(9) (ctx-run context (fmap #'sqr (mreturn 3)))))
    (is (equalp '(9) (ctx-run context (fmap #'sqr (list 3)))))
    (is (equalp '(9) (ctx-run context (fapply (pure #'sqr) (pure 3)))))
    (is (equalp '(9) (ctx-run context (fapply (list #'sqr) (pure 3)))))
    (is (equalp '(9) (ctx-run context (fapply (pure #'sqr) (list 3)))))
    (is (equalp '(9) (ctx-run context (fapply (list #'sqr) (list 3)))))
    (is (equalp '(3 3) (ctx-run context (flatmap (lambda (x) (list x x)) (mreturn 3)))))
    (is (equalp '(3) (ctx-run context (flatmap #'mreturn (mreturn 3)))))
    (is (equalp '(3) (ctx-run context (flatten (mreturn (mreturn 3))))))
    (is (equalp '(3) (ctx-run context (flatten (mreturn (list 3))))))
    (is (equalp '(3) (ctx-run context (flatten (list (mreturn 3))))))
    (is (equalp '(3) (ctx-run context (flatten (list (list 3))))))))

(test monad-context-flatmap
  (let ((context
          (make-instance 'monad-operators
            :mreturn (lambda (x) (list x))
            :flatmap (lambda (f xs)
                       (labels ((recur (xs accum)
                                  (if (null xs) (reverse accum)
                                      (recur (cdr xs) (rappend (funcall f (car xs)) accum)))))
                         (recur xs nil))))))
    (is-false (typep context 'trivial-operators))
    (is-true (typep context 'monad-operators))
    (is-true (typep context 'applicative-operators))
    (is-true (typep context 'functor-operators))
    (is (equalp '(3) (ctx-run context (pure 3))))
    (is (equalp '(3) (ctx-run context (mreturn 3))))
    (is (equalp '(9) (ctx-run context (fmap #'sqr (mreturn 3)))))
    (is (equalp '(9) (ctx-run context (fmap #'sqr (list 3)))))
    (is (equalp '(9) (ctx-run context (fapply (pure #'sqr) (pure 3)))))
    (is (equalp '(9) (ctx-run context (fapply (list #'sqr) (pure 3)))))
    (is (equalp '(9) (ctx-run context (fapply (pure #'sqr) (list 3)))))
    (is (equalp '(9) (ctx-run context (fapply (list #'sqr) (list 3)))))
    (is (equalp '(3 3) (ctx-run context (flatmap (lambda (x) (list x x)) (mreturn 3)))))
    (is (equalp '(3) (ctx-run context (flatmap #'mreturn (mreturn 3)))))
    (is (equalp '(3) (ctx-run context (flatten (mreturn (mreturn 3))))))
    (is (equalp '(3) (ctx-run context (flatten (mreturn (list 3))))))
    (is (equalp '(3) (ctx-run context (flatten (list (mreturn 3))))))
    (is (equalp '(3) (ctx-run context (flatten (list (list 3))))))))

(test monad-context-verbose
  (let ((context
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
                         (recur xss nil))))))
  (is-false (typep context 'trivial-operators))
  (is-true (typep context 'monad-operators))
  (is-true (typep context 'applicative-operators))
  (is-true (typep context 'functor-operators))

  (is (equalp '(3) (ctx-run context (pure 3))))
  (is (equalp '(3) (ctx-run context (mreturn 3))))
  (is (equalp '(9) (ctx-run context (fmap #'sqr (mreturn 3)))))
  (is (equalp '(9) (ctx-run context (fmap #'sqr (list 3)))))
  (is (equalp '(9) (ctx-run context (fapply (pure #'sqr) (pure 3)))))
  (is (equalp '(9) (ctx-run context (fapply (list #'sqr) (pure 3)))))
  (is (equalp '(9) (ctx-run context (fapply (pure #'sqr) (list 3)))))
  (is (equalp '(9) (ctx-run context (fapply (list #'sqr) (list 3)))))
  (is (equalp '(3 3) (ctx-run context (flatmap (lambda (x) (list x x)) (mreturn 3)))))
  (is (equalp '(3) (ctx-run context (flatmap #'mreturn (mreturn 3)))))
  (is (equalp '(3) (ctx-run context (flatten (mreturn (mreturn 3))))))
  (is (equalp '(3) (ctx-run context (flatten (mreturn (list 3))))))
  (is (equalp '(3) (ctx-run context (flatten (list (mreturn 3))))))
  (is (equalp '(3) (ctx-run context (flatten (list (list 3))))))
  (is (equalp '(4 5 5 6)
              (ctx-run context
                (let*-mon ((x '(1 2))
                           (y '(3 4)))
                  (mreturn (+ x y))))))


  (is (equalp '(4 5 5 6)
              (ctx-run context
                (let-mon ((x '(1 2))
                          (y '(3 4)))
                  (mreturn (+ x y))))))

  ;; Below, the binding uses `X' bound inside of the
  ;; `LET-FUN' form to compute the expression bound to `Y',
  ;; not the binding to `X' outside the `LET-FUN' form.
  (is (equalp '(4 6)
              (ctx-run context
                (let ((x 10))
                  (declare (ignore x))
                  (let*-mon ((x '(1 2))
                             (y  (mreturn (+ x 2))))
                    (mreturn (+ x y)))))))

  ;; Below, the parallel binding uses `X' bound outside of the
  ;; `LET-FUN' form to compute the expression bound to `Y'.
  (is (equalp '(3 4)
              (ctx-run context
                (let ((x 0))
                  (let-mon ((x '(1 2))
                            (y  (mreturn (+ x 2))))
                    (mreturn (+ x y)))))))

  (is (equalp '(4 5 5 6)
              (ctx-run context
                (let-app ((x '(1 2))
                          (y '(3 4)))
                  (+ x y)))))
  (is (equalp '((4 5) (5 6))
              (ctx-run context
                (let*-fun ((x '(1 2))
                           (y '(3 4)))
                  (+ x y)))))))



(eval-when (:load-toplevel :compile-toplevel)
  (defstruct repeat value)

 (deftype ziplist ()
   '(or list repeat))

 (defun repeat  (x)
   (make-repeat :value x))

 (defun ziplist-reverse (xs)
   (cond ((listp xs) (reverse xs))
         ((repeat-p xs) xs)))

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


(test applicative-context-fapply
  (let ((context
          (make-instance 'applicative-operators
            :fapply #'ziplist-fapply
            :pure #'repeat)))
    (is-false (typep context 'trivial-operators))
    (is-false (typep context 'monad-operators))
    (is-true (typep context 'applicative-operators))
    (is-true (typep context 'functor-operators))
    (is (equalp (repeat 3) (ctx-run context (pure 3))))
    (is (equalp (repeat 9) (ctx-run context (fmap #'sqr (repeat 3)))))
    (is (equalp '(1 4 9) (ctx-run context (fmap #'sqr '(1 2 3)))))
    (is (equalp '(1 4 9) (ctx-run context (fapply (pure #'sqr) '(1 2 3)))))
    (is (equalp (repeat 9) (ctx-run context (fapply (pure #'sqr) (repeat 3)))))
    (is (equalp '(9 6) (ctx-run context (fapply (list #'sqr #'twc) (repeat 3)))))
    (is (equalp '(1 4) (ctx-run context (fapply (list #'sqr #'twc) '(1 2)))))
    (is (equalp (repeat '(x y)) (ctx-run context (product (pure 'x) (pure 'y)))))
    (is (equalp '(4 6)
                (ctx-run context
                  (let-app ((x '(1 2))
                            (y '(3 4)))
                    (+ x y)))))
    (is (equalp '((4 5) (5 6))
                (ctx-run context
                  (let*-fun ((x '(1 2))
                             (y '(3 4)))
                    (+ x y)))))))


(test applicative-context-product
 (let ((context (make-instance 'applicative-operators
                  :fmap #'ziplist-fmap
                  :pure #'repeat
                  :product #'ziplist-product)))
   (is-false (typep context 'trivial-operators))
   (is-false (typep context 'monad-operators))
   (is-true (typep context 'applicative-operators))
   (is-true (typep context 'functor-operators))
   (is (equalp (repeat 3) (ctx-run context (pure 3))))
   (is (equalp (repeat 9) (ctx-run context (fmap #'sqr (repeat 3)))))
   (is (equalp '(1 4 9) (ctx-run context (fmap #'sqr '(1 2 3)))))
   (is (equalp '(1 4 9) (ctx-run context (fapply (pure #'sqr) '(1 2 3)))))
   (is (equalp (repeat 9) (ctx-run context (fapply (pure #'sqr) (repeat 3)))))
   (is (equalp '(9 6) (ctx-run context (fapply (list #'sqr #'twc) (repeat 3)))))
   (is (equalp '(1 4) (ctx-run context (fapply (list #'sqr #'twc) '(1 2)))))
   (is (equalp (repeat '(x y)) (ctx-run context (product (pure 'x) (pure 'y)))))
   (is (equalp '(4 6)
               (ctx-run context
                 (let-app ((x '(1 2))
                           (y '(3 4)))
                   (+ x y)))))
   (is (equalp '((4 5) (5 6))
               (ctx-run context
                 (let*-fun ((x '(1 2))
                            (y '(3 4)))
                   (+ x y)))))))

(test lift
  (let ((context (make-instance 'trivial-operators
                   :wrap (lambda (x) (make-id :value x))
                   :unwrap #'id-value)))
    (is (equalp
         (make-id :value 4)
         (ctx-run context (lift #'sqr (pure 2)))))
    (is (equalp
         (make-id :value 3)
         (ctx-run context (lift2 #'+ (pure 1) (pure 2)))))
    (is (equalp
         (make-id :value 6)
         (ctx-run context (lift3 #'+ (pure 1) (pure 2) (pure 3)))))
    (is (equalp
         (make-id :value 10)
         (ctx-run context (lift4 #'+ (pure 1) (pure 2) (pure 3) (pure 4)))))
    (is (equalp
         (make-id :value 15)
         (ctx-run context (lift5 #'+ (pure 1) (pure 2) (pure 3) (pure 4) (pure 5)))))
    (is (equalp
         (make-id :value 21)
         (ctx-run context (lift6 #'+ (pure 1) (pure 2) (pure 3) (pure 4) (pure 5) (pure 6)))))
    (is (equalp
         (make-id :value 28)
         (ctx-run context (lift7 #'+ (pure 1) (pure 2) (pure 3) (pure 4) (pure 5) (pure 6) (pure 7)))))))

(defmacro is-error (expr)
  (let ((e (gensym "E")))
    `(is-true
      (handler-case
          (progn ,expr nil)
        (error (,e)
          (declare (ignore ,e))
          t)))))

(test missing-operators
  (is-error (make-instance 'trivial-operators :wrap #'(lambda (x) (make-id :value x))))
  (is-error (make-instance 'trivial-operators :unwrap #'id-value))
  (is-error (make-instance 'monad-operators
              :fmap (lambda (f xs) (mapcar f xs))
              :flatten (lambda (xss)
                         (labels ((recur (xss accum)
                                    (if (null xss) (reverse accum)
                                        (recur (cdr xss) (rappend (car xss) accum)))))
                           (recur xss nil)))))
  (is-error (make-instance 'monad-operators
              :mreturn (lambda (x) (list x))
              :flatten (lambda (xss)
                         (labels ((recur (xss accum)
                                    (if (null xss) (reverse accum)
                                        (recur (cdr xss) (rappend (car xss) accum)))))
                           (recur xss nil)))))
  (is-error (make-instance 'monad-operators
              :fmap (lambda (f xs) (mapcar f xs))
              :mreturn (lambda (x) (list x))))

  (is-error (make-instance 'applicative-operators
              :fapply #'ziplist-fapply))

  (is-error (make-instance 'applicative-operators
              :pure #'repeat
              :product #'ziplist-product))

  (is-error (make-instance 'functor-operators))

  (is-error (make-instance 'comonad-operators))
  (is-error (make-instance 'comonad-operators :extract #'funcall))

  (is-error (make-instance 'comonad-operators :extract #'funcall :duplicate #'list))
  (is-error (make-instance 'comonad-operators :extend (lambda (f wx) (funcall f wx)))))
