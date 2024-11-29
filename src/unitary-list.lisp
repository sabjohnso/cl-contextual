(in-package :contextual-unitary-list)

(defun unitary-list-p (arg)
  "Return `T' if `ARG' is `NIL' or single element list."
  (or (null arg)
      (and (listp arg)
           (= 1 (length arg)))))

(deftype unitary-list ()
  '(satisfies unitary-list-p))

(defun make-unitary-list-context ()
  "Return the monad operators for a unitary list.

A unitary list is a list with 1 or zero elements"
  (make-instance 'monad-operators
    :mreturn #'list
    :flatmap (lambda (f mx)
               (and mx (funcall f (car mx))))))
