;; -*- mode: scheme -*-

(library (Data.Traversable foreign)
  (export traverseArrayImpl)
  (import (only (rnrs base) define lambda cons quote)
          (prefix (purs runtime lib) rt:)
          (prefix (purs runtime srfi :214) srfi:214:))

  (define kons
    (lambda (x)
      (lambda (ys)
        (cons x ys))))

  (define traverseArrayImpl
    (lambda (apply)
      (lambda (map)
        (lambda (pure)
          (lambda (f)
            (lambda (xs)
              ;; We fold to a list and then convert back to flexvector
              ((map srfi:214:list->flexvector)
               (srfi:214:flexvector-fold-right
                 (lambda (s x) ((apply ((map kons) (f x))) s))
                 (pure '())
                 xs))))))))

)
