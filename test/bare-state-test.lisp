(in-package :cl-user)

(defpackage :contextual-bare-state-test
  (:use :cl :fiveam :contextual :contextual-bare-state)
  (:export #:run-all-tests!))

(in-package :contextual-bare-state-test)

(defun run-all-tests! ()
  (run! 'bare-state))

(def-suite bare-state)

(in-suite bare-state)

(defun sqr (x)
  (* x x))

(test functions
  (is (equal '(x s) (bs-run 's (bs-mreturn 'x))))
  (is (equal '(x s) (bs-run 's (bs-pure 'x))))
  (is (equal '("X" s) (bs-run 's (bs-fmap #'symbol-name (bs-pure 'x)))))
  (is (equal '("X" s) (bs-run 's (bs-fapply (bs-pure #'symbol-name) (bs-pure 'x)))))
  (is (equal '((x y) s) (bs-run 's (bs-product (bs-pure 'x) (bs-pure 'y)))))
  (is (equal '("X" s) (bs-run 's (bs-flatmap (lambda (x) (bs-pure (symbol-name x))) (bs-pure 'x)))))
  (is (equal '(x s) (bs-run 's (bs-flatten (bs-pure (bs-pure 'x))))))
  (is (equal '(s s) (bs-run 's (bs-mget))))
  (is (equal '("S" s) (bs-run 's (bs-select #'symbol-name))))
  (is (equal '(nil s1) (bs-run 's0 (bs-mput 's1))))
  (is (equal '(nil "S") (bs-run 's (bs-modify #'symbol-name)))))


(defmacro show (expr)
  (let ((result (gensym "RESULT")))
    `(progn
       (let ((,result ,expr))
         (format t "~%~s~%" (list ',expr '=> ,result))
         ,result))))

(test binding-macros
  (let ((s0 '((x . 1) (y . 2))))
    (flet ((by-key (k)
             (lambda (s)
               (let ((kv (assoc k s)))
                 (if kv (cdr kv)
                     (error (format nil "Failed to find key ~s in ~s" k s))))))
           (set-key (k v) (lambda (s) (cons (cons k v) (remove k s :key #'car)))))



      (is (equal (list 7 s0)
                 (bs-run s0
                   (let-mon/bs ((x (bs-pure 3))
                                (y (bs-pure 4)))
                     (bs-mreturn (+ x y))))))
      (is (equal (list s0 s0)
                 (bs-run s0
                   (let-mon/bs ((s (bs-mget)))
                     (bs-mreturn s)))))


      (is (equal (list 1 s0)
                 (bs-run s0
                   (let-mon/bs ((x (bs-select (by-key 'x))))
                     (bs-mreturn x)))))

      (is (equal (list 3 s0)
                 (bs-run s0
                   (let-mon/bs ((x (bs-select (by-key 'x)))
                                (y (bs-select (by-key 'y))))
                     (bs-mreturn (+ x y))))))

      (is (equal (list 3 s0)
                 (bs-run s0
                   (let-app/bs ((x (bs-select (by-key 'x)))
                                (y (bs-select (by-key 'y))))
                     (+ x y)))))

      (is (equal (list 3 s0)
                 (bs-run s0
                   (bs-flatten
                    (let-fun/bs ((x (bs-select (by-key 'x)))
                                 (y (bs-select (by-key 'y))))
                      (+ x y))))))

      (is (equal (list 3 '((x . 4) (y . 5)))
                 (bs-run s0
                   (let-mon/bs ((x (bs-select (by-key 'x)))
                                (y (bs-select (by-key 'y))))
                     (bs-mput `((x . ,(+ x 3))
                                (y . ,(+ y 3))))
                     (bs-mreturn (+ x y))))))

      (is (equal (list 9 '((x . 4) (y . 5) (x . 1) (y . 2)))
                 (bs-run s0
                   (progn-mon/bs
                     (bs-modify (lambda (s) (append '((x . 4) (y . 5)) s)))
                     (let-mon/bs ((x (bs-select (by-key 'x)))
                                  (y (bs-select (by-key 'y))))
                       (bs-mreturn (+ x y)))))))
      (is (equal (list 9 '((x . 4) (y . 5) (x . 1) (y . 2)))
                 (bs-run s0
                   (progn-mon/bs
                     (bs-modify (lambda (s) (append '((x . 4) (y . 5)) s)))
                     (let-app/bs ((x (bs-select (by-key 'x)))
                                  (y (bs-select (by-key 'y))))
                       (+ x y)))))))))
