(in-package :contextual)

(defunion either
  (left value)
  (right value))

(deftype either-constructor ()
  '(function (t) either))

(defun either-flatmap (f mx)
  (declare (type either-constructor f)
           (type either mx))
  (the either
    (etypecase mx
      (left mx)
      (right (funcall f (right-value mx))))))

(defun either-fail (string)
  (declare (type string string))
  (the either (left string)))

(defun either-fmap (f mx)
  (declare (type function f)
           (type either mx))
  (the either
    (etypecase mx
      (left mx)
      (right (right (funcall f (right-value mx)))))))

(defun either-swap (mx)
  (etypecase mx
    (left  (right (left-value mx)))
    (right (left (right-value mx)))))

(defun either-untag (mx)
  (etypecase mx
    (left (left-value mx))
    (right (right-value mx))))

(defun make-either-context ()
  (make-instance 'monad-fail-operators
    :fmap #'either-fmap
    :pure #'right
    :flatmap #'either-flatmap
    :fail #'left))
