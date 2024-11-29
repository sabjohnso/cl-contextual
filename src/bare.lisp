(in-package :contextual-bare)

(defun make-bare-context ()
  "Return a trivial context for bare values"
  (make-instance 'trivial-operators
    :wrap #'identity
    :unwrap #'identity))
