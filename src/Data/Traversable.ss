;; -*- mode: scheme -*-

(library (Data.Traversable foreign)
  (export traverseArrayImpl)
  (import (only (rnrs base) define lambda)
          (prefix (purs runtime lib) rt:)
          (prefix (purs runtime srfi :214) srfi:214:))

  (define kons
    (lambda (x)
      (lambda (ys)
        (srfi:214:flexvector-append (rt:make-array x) ys))))

  (define traverseArrayImpl
    (lambda (apply)
      (lambda (map)
        (lambda (pure)
          (lambda (f)
            (lambda (xs)
              (srfi:214:flexvector-fold-right
                (lambda (s x) ((apply ((map kons) (f x))) s))
                (pure (rt:make-array))
                xs)))))))

)
