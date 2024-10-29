(in-package :cl-user)

(defpackage :contextual-utility
  (:use :cl)
  (:export #:define-constant
           #:defunion))

(in-package :contextual-utility)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(eval-when (:load-toplevel :compile-toplevel :execute)

  (declaim (ftype (function (symbol) keyword) symbol-to-keyword))

  (defun symbol-to-keyword (sym)
    (if (keywordp sym) sym
        (read-from-string (concatenate 'string ":" (symbol-name sym)))))

  (defun field-decl-to-field-name (field-decl)
    (if (symbolp field-decl) field-decl
        (car field-decl)))

  (defun union-doc (doc-and-variants)
    (and doc-and-variants
         (let ((head (car doc-and-variants)))
           (and (stringp head) (list head)))))

  (defun union-variants (doc-and-variants)
    (and doc-and-variants
         (let ((head (car doc-and-variants)))
           (if (stringp head) (cdr doc-and-variants)
               doc-and-variants))))

  (defun variant-head (variant)
    (car variant))

  (defun variant-name (variant)
    (let ((head  (variant-head variant)))
      (if (symbolp head) head
          (car head))))

  (defun variant-doc (variant)
    (and (> (length variant) 1)
         (let ((head (cadr variant)))
           (and (stringp head) (list head)))))

  (defun variant-fields (variant)
    (cond
      ((= (length variant) 1) nil)
      ((stringp (cadr variant)) (cddr variant))
      (t (cdr variant))))

  (defun field-name (field-decl)
    (if (listp field-decl)
        (car field-decl)
        field-decl))

  (defun variant-field-names (variant)
    (if (stringp (cadr variant))
        (loop for field in (cddr variant)
              collecting (field-decl-to-field-name field))
        (loop for field in (cdr variant)
              collecting (field-decl-to-field-name field))))

  (defun make-constructor-def (name field-names)
    (let ((make-name (intern (concatenate 'string "MAKE-" (symbol-name name))))
          (keyword-bindings
            (loop for field-name in field-names
                  appending (list (symbol-to-keyword field-name) field-name))))
      `(defun ,name (,@field-names)
         (,make-name ,@keyword-bindings))))

  (defun make-union-typedef (name variant-names)
    `(deftype ,name ()
       '(or ,@variant-names)))

  (defun make-union-type-predicate (name)
    (let ((predicate-name (intern (concatenate 'string (symbol-name name) "-P"))))
      (let ((arg (gensym "ARG")))
      `(defun ,predicate-name (,arg)
         (typep ,arg ',name))))))

(defmacro defunion (name &rest variants)
  (let* ((variant-struct-defs
           (loop for variant in variants
                 collecting `(defstruct ,@variant)))
         (variant-names
           (loop for variant in variants
                 collecting (variant-name variant)))
         (variant-field-names
           (loop for variant in variants
                 collecting (variant-field-names variant)))
         (variant-constructor-defs
           (loop for name in variant-names
                 for field-names in variant-field-names
                 collecting (make-constructor-def name field-names)))
         (union-type-def (make-union-typedef name variant-names))
         (union-type-predicate (make-union-type-predicate name)))

    `(progn
       ,@variant-struct-defs
       ,@variant-constructor-defs
       ,union-type-def
       ,union-type-predicate)))
