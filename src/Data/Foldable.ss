;; -*- mode: scheme -*-

(library (Data.Foldable foreign)
  (export foldrArray
          foldlArray)
  (import (only (rnrs base) define lambda)
          (prefix (purs runtime srfi :214) srfi:214:))

  (define foldrArray
    (lambda (f)
      (lambda (init)
        (lambda (xs)
          (srfi:214:flexvector-fold-right (lambda (s n) ((f n) s)) init xs)))))

  (define foldlArray
    (lambda (f)
      (lambda (init)
        (lambda (xs)
          (srfi:214:flexvector-fold (lambda (s n) ((f s) n)) init xs)))))

)
