(in-package :contextual-list)

(eval-when (:load-toplevel :compile-toplevel)
  (defun rappend (xs ys)
    (declare (type list xs ys))
    (the list
      (labels ((recur (xs ys)
                 (if xs (recur (cdr xs) (cons (car xs) ys))
                     ys)))
        (recur xs ys))))

 (defun list-flatmap (f xs)
   (declare (type function f) (type list xs))
   (the list
     (labels ((recur (xs accum)
                (if (null xs) (reverse accum)
                    (destructuring-bind (x . xs) xs
                      (recur xs (rappend (funcall f x) accum))))))
       (recur xs nil)))))

(define-constant +list+
    (make-instance 'monad-operators
      :pure #'list
      :flatmap #'list-flatmap))
