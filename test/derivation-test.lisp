(in-package :cl-user)

(defpackage :contextual-derivation-test
  (:use :cl :5am :contextual-derivation)
  (:export #:run-all-tests!))

(in-package :contextual-derivation-test)

(defun run-all-tests! ()
  (format t "~%~a~%" :contextual-derivation-test)
  (run! 'contextual-derivation-test))

(def-suite contextual-derivation-test)

(in-suite contextual-derivation-test)

(defstruct id value)
(defun id-pure (x)
  (make-id :value x))

(defun id-fmap (f mx)
  (id-pure (funcall f (id-value mx))))

(defun id-fapply (mf mx)
  (id-pure (funcall (id-value mf) (id-value mx))))

(defun id-product (mx my)
  (id-pure (list (id-value mx) (id-value my))))

(defun id-flatmap (f mx)
  (funcall f (id-value mx)))

(defun id-flatten (mmx)
  (id-value mmx))

(defun id-wrap (x)
  (id-pure x))

(defun id-unwrap (mx)
  (id-value mx))

(defun id-duplicate (wx)
  (make-id :value wx))

(defun id-extract (wx)
  (id-value wx))

(defun id-extend (f wx)
  (make-id :value (funcall f wx)))

(defun sqr (x)
  (* x x))


(defun/fapply-to-fmap fmap-from-fapply
  :fapply id-fapply
  :pure id-pure)

(test fmap-from-fapply
  (let* ((fmap (lambda/fapply-to-fmap :fapply #'id-fapply :pure #'id-pure))
         (mx (id-pure 3))
         (expected (id-value (id-fmap #'sqr mx))))
    (is (= expected (id-value (funcall fmap #'sqr mx))))
    (is (= expected (id-value (fmap-from-fapply #'sqr (id-pure 3)))))))

(defun/product-to-fapply fapply-from-product
  :product id-product
  :fmap id-fmap)

(test fapply-from-product
  (let* ((fapply (lambda/product-to-fapply :product #'id-product :fmap #'id-fmap))
         (mf (id-pure #'sqr))
         (mx (id-pure 3))
         (expected (id-fapply mf mx)))
    (is (equalp expected (funcall fapply mf mx)))
    (is (equalp expected (fapply-from-product mf mx)))))

(defun/fapply-to-product product-from-fapply
  :fapply id-fapply
  :pure id-pure)

(test product-from-fapply
  (let* ((product (lambda/fapply-to-product :fapply #'id-fapply :pure #'id-pure))
         (mx (id-pure 'x))
         (my (id-pure 'y))
         (expected (id-product mx my)))
    (is (equalp expected (funcall product mx my)))
    (is (equalp expected (product-from-fapply mx my)))))


(defun/flatmap-to-fapply fapply-from-flatmap
  :flatmap id-flatmap
  :pure id-pure)

(test fapply-from-flatmap
  (let* ((fapply (lambda/flatmap-to-fapply :flatmap #'id-flatmap :pure #'id-pure))
         (mf (id-pure #'sqr))
         (mx (id-pure 3))
         (expected (id-fapply mf mx)))
    (is (equalp expected (funcall fapply mf mx)))
    (is (equalp expected (fapply-from-flatmap mf mx)))))

(defun/flatmap-to-flatten flatten-from-flatmap
  :flatmap id-flatmap)

(test flatten-from-flatmap
  (let* ((flatten (lambda/flatmap-to-flatten :flatmap #'id-flatmap))
         (mmx (id-pure (id-pure 3)))
         (expected (id-flatten mmx)))
    (is (equalp expected (funcall flatten mmx)))
    (is (equalp expected (flatten-from-flatmap mmx)))))

(defun/flatten-to-flatmap flatmap-from-flatten
  :flatten id-flatten
  :fmap id-fmap)

(defun sqr-id (x)
  (id-pure (sqr x)))

(test flatmap-from-flatten
  (let* ((flatmap (lambda/flatten-to-flatmap :flatten #'id-flatten :fmap #'id-fmap))
         (mx (id-pure 3))
         (expected (id-flatmap #'sqr-id mx)))
    (is (equalp expected (funcall flatmap #'sqr-id mx )))
    (is (equalp expected (flatmap-from-flatten #'sqr-id mx)))))

(defun/wrap-and-unwrap-to-fmap fmap-from-wrap-and-unwrap
  :wrap id-wrap
  :unwrap id-unwrap)

(test fmap-from-wrap-and-unwrap
  (let* ((fmap (lambda/wrap-and-unwrap-to-fmap :wrap #'id-wrap :unwrap #'id-unwrap))
         (mx (id-pure 3))
         (expected (id-fmap #'sqr mx)))
    (is (equalp expected (funcall fmap #'sqr mx)))
    (is (equalp expected (fmap-from-wrap-and-unwrap #'sqr mx)))))


(defun/duplicate-to-extend extend-from-duplicate
  :duplicate id-duplicate
  :fmap id-fmap)

(test extend-from-duplicate
  (flet ((func (wx)
           (sqr (id-value wx))))
    (let* ((extend (lambda/duplicate-to-extend
                    :duplicate #'id-duplicate
                    :fmap #'id-fmap))
           (wx (id-pure 3))
           (expected (id-extend #'func wx)))
      (is (equalp expected (funcall extend #'func wx))
          (equalp expected (extend-from-duplicate #'func wx))))))

(contextual-derivation:defun/extend-to-duplicate duplicate-from-extend
  :extend id-extend)

(test duplicate-from-extend
  (let* ((duplicate (lambda/extend-to-duplicate :extend #'id-extend))
         (wx (id-pure 3))
         (expected (id-duplicate wx)))
    nil
    (is (equalp expected (funcall duplicate wx)))
    (is (equalp expected (duplicate-from-extend wx)))))

(declaim (inline bf-run bf-mreturn bf-flatmap bf-ask bf-local))

(defun bf-run (e mx)
  (funcall mx e))

(defun bf-mreturn (x)
  (lambda (e )(declare (ignore e)) x))

(defun bf-flatmap (f mx)
  (lambda (e)
    (bf-run e (funcall f (bf-run e mx)))))

(defun bf-ask ()
  (lambda (e) e))

(defun bf-local (f mx)
  (lambda (e)
    (bf-run (funcall f e) mx)))


(derive-environment-monad-interface bf
  :mreturn bf-mreturn
  :flatmap bf-flatmap
  :ask bf-ask
  :local bf-local)

(test derived-environment-monad-interface
   (let ((e '(:x 3 :y 4)))
     (flet ((by-key (k) (lambda (e) (getf e k))))
       (is (= 3 (bf-run e (bf-pure 3))))
       (is (= 3 (bf-run e (bf-lookup (by-key :x)))))
       (is (= 4 (bf-run e (bf-lookup (by-key :y)))))
       (is (= 9 (bf-run e (bf-fmap #'sqr (bf-lookup (by-key :x))))))
       (is (= 7 (bf-run e (bf-fapply (bf-fmap (lambda (x) (lambda (y) (+ x y)))
                                              (bf-lookup (by-key :x)))
                                     (bf-lookup (by-key :y))))))
       (is (= 7
              (bf-run e
                (bf-flatten
                 (let-fun/bf ((x (bf-lookup (by-key :x)))
                              (y (bf-lookup (by-key :y))))
                   (+ x y))))))
       (is (= 7
              (bf-run e
                (let-app/bf ((x (bf-lookup (by-key :x)))
                             (y (bf-lookup (by-key :y))))
                        (+ x y)))))

       (is (= 7
             (bf-run e
               (let-mon/bf ((x (bf-lookup (by-key :x)))
                            (y (bf-lookup (by-key :y))))
                 (bf-mreturn (+ x y)))))))))


(defun bs-run (s mx)
  (funcall mx s))

(defun bs-mreturn (x)
  (lambda (s) (list x s)))

(defun bs-flatmap (f mx)
  (lambda (s)
    (destructuring-bind (x s) (bs-run s mx)
      (bs-run s (funcall f x)))))

(defun bs-state (f) f)

(defun bs-mput (s)
  (lambda (ignored)
    (declare (ignore ignored))
    (list nil s)))

(derive-state-monad-interface bs
  :mreturn bs-mreturn
  :flatmap bs-flatmap
  :state bs-state
  :mput bs-mput)


(test derived-state-monad-interface-<state-and-mput>
  (is-true (functionp #'bs-fmap))
  (is-true (functionp #'bs-pure))
  (is-true (functionp #'bs-fapply))
  (is-true (functionp #'bs-product))
  (is-true (functionp #'bs-mreturn))
  (is-true (functionp #'bs-flatmap))
  (is-true (functionp #'bs-flatten))
  (is-true (functionp #'bs-mget))
  (is-true (functionp #'bs-select))
  (is-true (functionp #'bs-mput))
  (is-true (functionp #'bs-modify))
  (is-true (functionp #'bs-state))

  (is (equal '(x s) (bs-run 's (bs-mreturn 'x))))
  (is (equal '(x s) (bs-run 's (bs-pure 'x))))
  (is (equal '("X" s) (bs-run 's (bs-fmap #'symbol-name (bs-pure 'x)))))
  (is (equal '(s s) (bs-run 's (bs-mget))))
  (is (equal '("S" s) (bs-run 's (bs-select #'symbol-name))))
  (is (equal '(nil s*) (bs-run 's (bs-mput 's*))))
  (is (equal '(nil "S") (bs-run 's (bs-modify #'symbol-name))))
  (is (equal '(x s) (bs-run 's (bs-state (lambda (s) (list 'x 's))))))

  (let ((s0 '((x . 3) (y . 4))))
    (flet ((by-key (k) (lambda (s)
                         (assert (listp s))
                         (assert (not (null s)))
                         (cdr (assoc k s)))))
      (is (equal (list 7 s0)
           (bs-run s0
             (let-app/bs ((x (bs-select (by-key 'x)))
                          (y (bs-select (by-key 'y))))
               (+ x y)))))

      (is (equal (list 7 s0)
                 (bs-run s0
                   (bs-flatten
                    (let-fun/bs ((x (bs-select (by-key 'x)))
                                 (y (bs-select (by-key 'y))))
                      (+ x y))))))

      (is (equal (list 10 s0)
                 (bs-run s0
                   (bs-flatten
                    (let*-fun/bs ((x (bs-select (by-key 'x)))
                                  (y (bs-fmap (lambda (y) (+ x y)) (bs-select (by-key 'y)))))
                      (+ x y))))))

      (is (equal (list 7 s0)
                 (bs-run s0
                   (let-mon/bs ((x (bs-select (by-key 'x)))
                                (y (bs-select (by-key 'y))))
                     (bs-mreturn (+ x y))))))

      (is (equal (list 10 s0)
                 (bs-run s0
                   (let*-mon/bs ((x (bs-select (by-key 'x)))
                                (y (bs-fmap (lambda (y) (+ x y)) (bs-select (by-key 'y)))))
                     (bs-mreturn (+ x y)))))))))



(defun bs2-mreturn (x)
  (bs-mreturn x))

(defun bs2-flatmap (f mx)
  (bs-flatmap f mx))

(defun bs2-mget ()
  (bs-mget))

(defun bs2-mput (s)
  (bs-mput s))


(derive-state-monad-interface bs2
  :mreturn bs2-mreturn
  :flatmap bs2-flatmap
  :mget bs2-mget
  :mput bs2-mput)


(defun bs2-run (s mx)
  (bs-run s mx))

(test derived-state-monad-interface-<mget-and-mput>
  (is-true (functionp #'bs2-fmap))
  (is-true (functionp #'bs2-pure))
  (is-true (functionp #'bs2-fapply))
  (is-true (functionp #'bs2-product))
  (is-true (functionp #'bs2-mreturn))
  (is-true (functionp #'bs2-flatmap))
  (is-true (functionp #'bs2-flatten))
  (is-true (functionp #'bs2-mget))
  (is-true (functionp #'bs2-select))
  (is-true (functionp #'bs2-mput))
  (is-true (functionp #'bs2-modify))
  (is-true (functionp #'bs2-state))

  (is (equal '(x s) (bs2-run 's (bs2-mreturn 'x))))
  (is (equal '(x s) (bs2-run 's (bs2-pure 'x))))
  (is (equal '("X" s) (bs2-run 's (bs2-fmap #'symbol-name (bs2-pure 'x)))))
  (is (equal '(s s) (bs2-run 's (bs2-mget))))
  (is (equal '("S" s) (bs2-run 's (bs2-select #'symbol-name))))
  (is (equal '(nil s*) (bs2-run 's (bs2-mput 's*))))
  (is (equal '(nil "S") (bs2-run 's (bs2-modify #'symbol-name))))
  (is (equal '(x s) (bs2-run 's (bs2-state (lambda (s) (list 'x 's))))))

  (let ((s0 '((x . 3) (y . 4))))
    (flet ((by-key (k) (lambda (s)
                         (assert (listp s))
                         (assert (not (null s)))
                         (cdr (assoc k s)))))
      (is (equal (list 7 s0)
           (bs2-run s0
             (let-app/bs ((x (bs2-select (by-key 'x)))
                          (y (bs2-select (by-key 'y))))
               (+ x y)))))

      (is (equal (list 7 s0)
                 (bs2-run s0
                   (bs2-flatten
                    (let-fun/bs ((x (bs2-select (by-key 'x)))
                                 (y (bs2-select (by-key 'y))))
                      (+ x y))))))

      (is (equal (list 10 s0)
                 (bs2-run s0
                   (bs2-flatten
                    (let*-fun/bs ((x (bs2-select (by-key 'x)))
                                  (y (bs2-fmap (lambda (y) (+ x y)) (bs2-select (by-key 'y)))))
                      (+ x y))))))

      (is (equal (list 7 s0)
                 (bs2-run s0
                   (let-mon/bs ((x (bs2-select (by-key 'x)))
                                (y (bs2-select (by-key 'y))))
                     (bs2-mreturn (+ x y))))))

      (is (equal (list 10 s0)
                 (bs2-run s0
                   (let*-mon/bs ((x (bs2-select (by-key 'x)))
                                (y (bs2-fmap (lambda (y) (+ x y)) (bs2-select (by-key 'y)))))
                     (bs2-mreturn (+ x y)))))))))



(defun bs3-run (s mx)
  (funcall mx s))

(defun bs3-mreturn (x)
  (bs-mreturn x))

(defun bs3-flatmap (f mx)
  (bs-flatmap f mx))

(defun bs3-select (f)
  (lambda (s)
    (list (funcall f s) s)))

(defun bs3-modify (f)
  (lambda (s)
    (list nil (funcall f s))))


(derive-state-monad-interface bs3
  :mreturn bs3-mreturn
  :flatmap bs3-flatmap
  :select bs3-select
  :modify bs3-modify)



(test derived-state-monad-interface-<mget-and-mput>
  (is-true (functionp #'bs3-fmap))
  (is-true (functionp #'bs3-pure))
  (is-true (functionp #'bs3-fapply))
  (is-true (functionp #'bs3-product))
  (is-true (functionp #'bs3-mreturn))
  (is-true (functionp #'bs3-flatmap))
  (is-true (functionp #'bs3-flatten))
  (is-true (functionp #'bs3-mget))
  (is-true (functionp #'bs3-select))
  (is-true (functionp #'bs3-mput))
  (is-true (functionp #'bs3-modify))
  (is-true (functionp #'bs3-state))

  (is (equal '(x s) (bs3-run 's (bs3-mreturn 'x))))
  (is (equal '(x s) (bs3-run 's (bs3-pure 'x))))
  (is (equal '("X" s) (bs3-run 's (bs3-fmap #'symbol-name (bs3-pure 'x)))))
  (is (equal '(s s) (bs3-run 's (bs3-mget))))
  (is (equal '("S" s) (bs3-run 's (bs3-select #'symbol-name))))
  (is (equal '(nil s*) (bs3-run 's (bs3-mput 's*))))
  (is (equal '(nil "S") (bs3-run 's (bs3-modify #'symbol-name))))
  (is (equal '(x s) (bs3-run 's (bs3-state (lambda (s) (list 'x 's))))))

  (let ((s0 '((x . 3) (y . 4))))
    (flet ((by-key (k) (lambda (s)
                         (assert (listp s))
                         (assert (not (null s)))
                         (cdr (assoc k s)))))
      (is (equal (list 7 s0)
           (bs3-run s0
             (let-app/bs ((x (bs3-select (by-key 'x)))
                          (y (bs3-select (by-key 'y))))
               (+ x y)))))

      (is (equal (list 7 s0)
                 (bs3-run s0
                   (bs3-flatten
                    (let-fun/bs ((x (bs3-select (by-key 'x)))
                                 (y (bs3-select (by-key 'y))))
                      (+ x y))))))

      (is (equal (list 10 s0)
                 (bs3-run s0
                   (bs3-flatten
                    (let*-fun/bs ((x (bs3-select (by-key 'x)))
                                  (y (bs3-fmap (lambda (y) (+ x y)) (bs3-select (by-key 'y)))))
                      (+ x y))))))

      (is (equal (list 7 s0)
                 (bs3-run s0
                   (let-mon/bs ((x (bs3-select (by-key 'x)))
                                (y (bs3-select (by-key 'y))))
                     (bs3-mreturn (+ x y))))))

      (is (equal (list 10 s0)
                 (bs3-run s0
                   (let*-mon/bs ((x (bs3-select (by-key 'x)))
                                (y (bs3-fmap (lambda (y) (+ x y)) (bs3-select (by-key 'y)))))
                     (bs3-mreturn (+ x y)))))))))
