(library (Test.Main foreign)
  (export arrayFrom1UpTo
          arrayReplicate
          mkNEArray
          foldMap1NEArray)
  (import (only (rnrs base) define lambda error))

  (define arrayFrom1UpTo
    (lambda (n)
      (error #f "Test.Main:arrayFrom1UpTo not implemented.")))

  (define arrayReplicate
    (lambda (n)
      (lambda (x)
        (error #f "Test.Main:arrayReplicate not implemented."))))

  (define mkNEArray
    (lambda (nothing)
      (lambda (just)
        (lambda (arr)
          (error #f "Test.Main:mkNEArray not implemented.")))))

  (define foldMap1NEArray
    (lambda (append)
      (lambda (f)
        (lambda (arr)
          (error #f "Test.Main:foldMap1NEArray not implemented.")))))

)
