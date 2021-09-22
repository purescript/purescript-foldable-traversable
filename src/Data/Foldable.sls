;; -*- mode: scheme -*-

(library (Data.Foldable foreign)
  (export foldrArray
          foldlArray)
  (import (only (rnrs base) define lambda error))

  (define foldrArray
    (lambda (f)
      (lambda (init)
        (lambda (xs)
          (error #f "Data.Foldable:foldrArray not implemented.")))))

  (define foldlArray
    (lambda (f)
      (lambda (init)
        (lambda (xs)
          (error #f "Data.Foldable:foldlArray not implemented.")))))

)
