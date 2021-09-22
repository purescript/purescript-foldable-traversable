;; -*- mode: scheme -*-

(library (Data.Traversable foreign)
  (export traverseArrayImpl)
  (import (only (rnrs base) define lambda error))

  (define traverseArrayImpl
    (lambda (apply)
      (lambda (map)
        (lambda (pure)
          (lambda (f)
            (lambda (array)
              (error #f "Data.Traversable:traverseArrayImpl not implemented.")))))))

)
