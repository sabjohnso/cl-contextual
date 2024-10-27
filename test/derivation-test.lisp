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
