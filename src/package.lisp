(in-package :cl-user)

(defpackage :contextual-utility
  (:use :cl)
  (:export
   #:with-init-lookup
   #:get-argument-or-slot-value
   #:with-syms
   #:when-missing
   #:define-constant
   #:defunion
   #:format-symbol
   #:read-string))

(defpackage :binding-syntax-helpers
  (:use :cl :contextual-utility)
  (:export
   #:make-monad-progn
   #:make-sequential-functor-binding
   #:make-sequential-monad-binding
   #:make-parallel-monad-binding
   #:make-parallel-applicative-binding
   #:make-parallel-functor-binding

   #:make-sequential-functor-binding-ez
   #:make-parallel-functor-binding-ez
   #:make-parallel-applicative-binding-ez
   #:make-monad-progn-ez
   #:make-sequential-monad-binding-ez
   #:make-parallel-monad-binding-ez))

(in-package :cl-user)
(defpackage :contextual-internal
  (:use :cl :binding-syntax-helpers)
  (:export #:contextual-p
           #:ctx-run
           #:ctx-return
           #:ctx-injest
           #:ctx-fmap
           #:ctx-fapply
           #:ctx-product
           #:ctx-flatmap
           #:ctx-flatten
           #:ctx-ask
           #:ctx-asks
           #:let*-fun/ctx
           #:let-app/ctx
           #:let*-mon/ctx))

(defpackage :contextual-derivation
  (:use :cl :contextual-utility :binding-syntax-helpers)
  (:export
   #:defun/flatmap-to-fmap         #:lambda/flatmap-to-fmap
   #:defun/wrap-and-unwrap-to-fmap #:lambda/wrap-and-unwrap-to-fmap
   #:defun/flatmap-to-flatten      #:lambda/flatmap-to-flatten
   #:defun/flatten-to-flatmap      #:lambda/flatten-to-flatmap
   #:defun/flatmap-to-fapply       #:lambda/flatmap-to-fapply
   #:defun/fapply-to-product       #:lambda/fapply-to-product
   #:defun/product-to-fapply       #:lambda/product-to-fapply
   #:defun/fapply-to-fmap          #:lambda/fapply-to-fmap
   #:defun/duplicate-to-extend     #:lambda/duplicate-to-extend
   #:defun/extend-to-duplicate     #:lambda/extend-to-duplicate
   #:defun/ask-to-lookup           #:lambda/ask-to-lookup
   #:defun/lookup-to-ask           #:lambda/lookup-to-ask

   #:derive-functor-interface
   #:derive-applicative-interface
   #:derive-monad-interface
   #:derive-environment-monad-interface
   #:derive-trivial-interface
   #:derive-comonad-interface
   #:derive-state-monad-interface))

(defpackage :contextual
  (:use :cl :binding-syntax-helpers :contextual-utility :contextual-internal :contextual-derivation)
  (:export
   #:fmap
   #:pure #:fapply #:product
   #:mreturn #:flatmap #:flatten
   #:wrap #:unwrap
   #:extract #:duplicate #:extend
   #:expel
   #:ask #:lookup #:local

   #:fmap-func
   #:pure-func #:fapply-func #:product-func
   #:flatmap-func #:flatten-func
   #:wrap-func #:unwrap-func
   #:extract-func #:duplicate-func #:extend-func
   #:ask-func #:asks-func #:local-func

   #:fail
   #:fail-func
   #:mzero #:mplus
   #:mzero-func #:mplus-func

   #:mempty
   #:mappend


   #:progn-mon
   #:let*-fun #:let-fun #:let-app #:let*-mon #:let-mon
   #:lift #:lift2 #:lift3 #:lift4 #:lift5 #:lift6 #:lift7

   #:monoid-operators
   #:functor-operators
   #:applicative-operators
   #:monad-operators
   #:comonad-operators
   #:trivial-operators
   #:monad-environment-operators
   #:monad-fail-operators
   #:monad-plus-operators

   #:ctx-run)

  ;; Monad State
  (:export
   #:monad-state-operators
   #:state
   #:mget
   #:mput
   #:select
   #:modify))

(defpackage :contextual-bare
  (:use :cl :contextual)
  (:export #:make-bare-context))

(defpackage :contextual-bare-function
  (:use :cl :binding-syntax-helpers :contextual :contextual-utility)
  (:export #:make-bare-function-context
           #:+bare-function+
           #:bf-run
           #:bf-fmap
           #:bf-pure
           #:bf-fapply
           #:bf-mreturn
           #:bf-flatmap
           #:bf-flatten
           #:bf-ask
           #:bf-lookup
           #:bf-local
           #:let*-fun/bf
           #:let-fun/bf
           #:let-app/bf
           #:let*-mond/bf
           #:let-mon/bf))

(defpackage :contextual-bare-state
  (:nicknames :bs)
  (:use :cl :binding-syntax-helpers :contextual-utility :contextual-derivation :contextual)
  (:export
   #:make-bare-state-context
   #:+bare-state+
   #:bs-run
   #:bs-exec
   #:bs-eval
   #:bs-fmap
   #:bs-pure
   #:bs-fapply
   #:bs-product
   #:bs-mreturn
   #:bs-flatmap
   #:bs-bind
   #:bs-flatten
   #:bs-mget
   #:bs-mput
   #:bs-select
   #:bs-modify
   #:let*-fun/bs
   #:let-fun/bs
   #:let-app/bs
   #:progn-mon/bs
   #:let*-mon/bs
   #:let-mon/bs))

(defpackage :contextual-list
  (:use :cl :contextual-utility :contextual)
  (:export #:+list+))

(defpackage :contextual-optional
  (:use :cl :trivia :contextual-utility :contextual)
  (:export #:optional #:optional-p
           #:just #:just-p
           #:none #:none-p
           #:make-optional-context))

(defpackage :contextual-thunk
  (:use :cl :contextual)
  (:export #:thunk #:thunk-wrap #:thunk-unwrap #:make-thunk-context))

(defpackage :contextual-unitary-list
  (:use :cl :contextual)
  (:export #:make-unitary-list-context #:unitary-list-p #:unitary-list))
