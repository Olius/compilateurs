(define-library (utils)
  (export car+cdr
          list->values
          flatten
          if-npop
          let-list
          for
          let-lazy
          split)
  (import (scheme base)
          (scheme case-lambda)
          (scheme char))
  (include "utils.scm"))