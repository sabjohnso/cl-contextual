(in-package :contextual-thunk)

(defmacro thunk (&rest exprs)
  `(lambda () ,@exprs))

(defun thunk-wrap (x)
  (thunk x))

(defun thunk-unwrap (thunk)
  (funcall thunk))

(defun make-thunk-context ()
  (make-instance 'trivial-operators
    :wrap #'thunk-wrap
    :unwrap #'thunk-unwrap))
