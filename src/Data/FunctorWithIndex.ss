;; -*- mode: scheme -*-

(library (Data.FunctorWithIndex foreign)
  (export mapWithIndexArray)
  (import (only (rnrs base) define lambda error))

  (define mapWithIndexArray
    (lambda (f)
      (lambda (xs)
        (error #f "Data.FunctorWithIndex:mapWithIndexArray not implemented."))))

)
