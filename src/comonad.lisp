(in-package :contextual)

(defgeneric extract-func (context))
(defgeneric duplicate-func (context))
(defgeneric extend-func (context))

(defun ask-extract ()
  (ctx-asks #'extract-func))

(defun ask-duplicate ()
  (ctx-asks #'duplicate-func))

(defun ask-extend ()
  (ctx-asks #'extend-func))

(defun extract (cwx)
  "Return a contextual expression extracting the value from the embellishment."
  (let-app/ctx ((extract (ask-extract))
                (wx (ctx-injest cwx)))
    (funcall extract wx)))

(defun expel (ctx cwx)
  (ctx-run ctx (extract cwx)))

(defun duplicate (cwx)
  "Return a contextual expression with the context's embellishment duplicated on the input"
  (let-app/ctx ((duplicate (ask-duplicate))
                (wx (ctx-injest cwx)))
    (funcall duplicate wx)))

(defun extend (f cwx)
  "Return a contextual expression with the context's embellishment extended back over
the value extracted from the embellishment."
  (let-app/ctx ((ctx (ctx-ask))
                (extend (ask-extend))
                (wx (ctx-injest cwx)))
    (funcall extend (lambda (wx)
                      (let ((result (funcall f wx)))
                        (if (contextual-p result)
                            (ctx-run ctx result)
                            result)))
             wx)))

(defclass comonad-operators (functor-operators)
  ((extract :initarg :extract :type function :reader extract-func)
   (duplicate :initarg :duplicate :type function :reader duplicate-func)
   (extend :initarg :extend :type function :reader extend-func)))

(defmethod initialize-instance ((obj comonad-operators) &rest args)
  (let ((extract (get-argument-or-slot-value args :extract obj 'extract)))
    (if extract
        (setf (slot-value obj 'extract) extract)
        (error "`EXTRACT' was not provided and could not be derived fro `TRIVIAL-OPERATORS'")))

  (let ((extend (get-argument-or-slot-value args :extend obj 'extend)))
    (if extend
        (setf (slot-value obj 'extend) extend)
        (let ((fmap (get-argument-or-slot-value args :fmap obj 'fmap))
              (duplicate (get-argument-or-slot-value args :duplicate obj 'duplicate)))
          (if (and fmap duplicate)
              (setf (slot-value obj 'extend)
                    (lambda/duplicate-to-extend :duplicate duplicate :fmap fmap))))))

  (let ((duplicate (get-argument-or-slot-value args :duplicate obj 'duplicate)))
    (if duplicate (setf (slot-value obj 'duplicate) duplicate)
        (let ((extend (get-argument-or-slot-value args :extend obj 'extend)))
          (setf (slot-value obj 'duplicate)
                (lambda/extend-to-duplicate :extend extend)))))

  (let ((extend (get-argument-or-slot-value args :extend obj 'extend))
        (extract (get-argument-or-slot-value args :extract obj 'extract)))
    (setf (slot-value obj 'fmap)
          (lambda (f wx)
            (funcall extend (lambda (wx) (funcall f (funcall extract wx))) wx))))

  (call-next-method)

  (assert (slot-value obj 'fmap))
  (assert (slot-value obj 'extract))
  (assert (slot-value obj 'duplicate))
  (assert (slot-value obj 'extend)))
