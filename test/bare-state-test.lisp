(in-package :cl-user)

(defpackage :contextual-bare-state-test
  (:use :cl :fiveam :trivia :contextual :contextual-bare-state)
  (:shadowing-import-from :contextual #:fail)
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
  (is (equal '(nil "S") (bs-run 's (bs-modify #'symbol-name))))
  (let ((s0 '((x . 1) (y . 2))))
           (flet ((by-key (k) (lambda (s) (cdr (assoc k s0)))))
             (is (equal (list 3 s0)
                        (bs-run s0
                          (bs-bind
                           (bs-select (by-key 'x))
                           (lambda (x)
                             (bs-bind
                              (bs-select (by-key 'y))
                              (lambda (y)
                                (bs-mreturn (+ x y))))))))))))


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


;; Update the state of a game base on the input. The state is a tripple
;; with a flag indicating if it is `ON' or `OFF' followed by the score
;; for `A' and followed by the score for `B'.  Valid inputs are one of
;; the symbol `A', `B', `ON' or `OFF'. Invalid inputs are just ignored
;; and the state is
(defun update-state (input)
  (lambda (state)
    (match (list input state)
           ((list 'on  (list 'off a b)) (list 'on  a b))
           ((list 'off (list 'on  a b)) (list 'off a b))
           ((list 'a   (list 'on  a b)) (list 'on  (1+ a) b))
           ((list 'b   (list 'on  a b)) (list 'on  a (1+ b)))
           ((list _ state) (format t "~s" `(:input ,input :state ,state)) state))))

;; Construct the stateful computation of a game based on the input listn
(defun game (inputs)
  (declare (type list inputs))
  (if (null inputs) (bs-mreturn nil)
      (destructuring-bind (input . more-inputs) inputs
        (progn-mon/bs
          (bs-modify (update-state input))
          (game more-inputs)))))

(test game
  (let ((initial-state '(off 0 0)))
    (is (eq nil (bs-exec initial-state (game '(on)))))
    (is (equal '(on 0 0) (bs-eval initial-state (game '(on)))))
    (is (equal '(off 1 0) (bs-eval initial-state (game '(on a off)))))
    (is (equal '(off 2 1) (bs-eval initial-state (game '(on a b a off)))))))
