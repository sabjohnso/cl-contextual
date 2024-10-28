(in-package :cl-user)

(defpackage :contextual-utility
  (:use :cl)
  (:export #:define-constant))

(in-package :contextual-utility)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))
