(in-package :contextual)
;; FIXME: Put the applicative code here

(defgeneric pure-func    (context))
(defgeneric fapply-func  (context))
(defgeneric product-func (context))


(defun ask-pure ()
  (ctx-asks #'pure-func))

(defun ask-fapply ()
  (ctx-asks #'fapply-func))

(defun ask-product ()
  (ctx-asks #'product-func))

(defun pure (cx)
  "Return a contextual expression with the input value injected into
the embellishment."
  (let-app/ctx ((pure (ask-pure))
                (x (ctx-injest cx)))
    (funcall pure x)))


(defun fapply (cmf cmx)
  "Given an embellished function and an embellished value,
return a contextual expression with the embellishment of the result
of applying the function to the value"
  (let-app/ctx ((fapply (ask-fapply))
                (mf (ctx-injest cmf))
                (mx (ctx-injest cmx)))
    (funcall fapply mf mx)))

(defun product (cmx cmy)
  "Given two embellished values, return a contextual expression with
an embellished list holding the two values."
  (let-app/ctx ((product (ask-product))
                (mx (ctx-injest cmx))
                (my (ctx-injest cmy)))
    (funcall product mx my)))


(defclass applicative-operators (functor-operators)
  ((pure :initarg :pure :type function :reader pure-func)
   (fapply :initarg :fapply :type function :reader fapply-func)
   (product :initarg :product :type function :reader product-func)))

(defmethod initialize-instance ((obj applicative-operators) &rest args &key &allow-other-keys)
    (let ((pure (get-argument-or-slot-value args :pure obj 'pure)))
    (if pure
        (setf (slot-value obj 'pure) pure)
        (error "`PURE' was not provide and cannot be derived for `APPLICATIVE-OPERATORS'")))

  (let ((fapply (get-argument-or-slot-value args :fapply obj 'fapply)))
    (if fapply (setf (slot-value obj 'fapply) fapply)
        (let ((product (get-argument-or-slot-value args :product obj 'product))
              (fmap (get-argument-or-slot-value args :fmap obj 'fmap)))
          (if (and product fmap)
              (setf (slot-value obj 'fapply)
                    (lambda/product-to-fapply :product product :fmap fmap))
              (error "`FAPPLY' was not provided and cannot be derived for `APPLICATIVE-OPERATORS'")))))

  (let ((product (get-argument-or-slot-value args :product obj 'product)))
    (if product (setf (slot-value obj 'product) product)
        (let ((fapply (get-argument-or-slot-value args :fapply obj 'fapply))
              (pure (get-argument-or-slot-value args :pure obj 'pure)))
          (assert fapply) ;; fapply should be guaranteed at this point
          (assert pure)   ;; pure should be guaranteed at this point
          (setf (slot-value obj 'product) (lambda/fapply-to-product :fapply fapply :pure pure)))))

  ;; Derive the function for `FMAP'.
  (setf (slot-value obj 'fmap)
        (lambda/fapply-to-fmap
         :fapply (slot-value obj 'fapply)
         :pure (slot-value obj 'pure)))

  (call-next-method))
