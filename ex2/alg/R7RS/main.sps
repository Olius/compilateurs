(import (scheme base)
        (scheme write)
        (alg)
        (utils)
        (rules))

; 5 + ( ( 4 + 7 ) * 3 + 8 ) * ( 5 + 4 )

(define c
  (let ((nb (term string->number)))
    (rules E
      (E (((T t) (D d)      ) (+ t d)))
      (D (( "+"  (E e)      ) e)
         ((                 ) 0))
      (T (((F f) (G g)      ) (* f g)))
      (G (( "*"  (T t)      ) t)
         ((                 ) 1))
      (F (( "("  (E e)  ")" ) e)
         (((nb n)           ) n)))))

#;(define cv
  (let ((nb (term string->number))
        (id (term string->symbol)))
    (rules PROG

      (PROG    (((LISTVAR l) (FORM    f)) (evl l f) ))
      (LISTVAR (((DECLVAR d) (LISTVAR l)) (add d l) )
               ((                       ) empty-dict))
      (DECLVAR (( "#" (id i) "=" (nb n) ) `(,i . ,n)))
      (FORM    (((E e)                  ) e))

      (E (((T t) (D d)      ) `(E ,t ,d)))
      (D (( "+"  (E e)      ) `(D ,e)   )
         ((                 ) `(D   )   )
      (T (((F f) (G g)      ) (* f g)))
      (G (( "*"  (T t)      ) t)
         ((                 ) 1))
      (F (( "("  (E e)  ")" ) e)
         (((nb n)           ) n)
         (((id i)           ) i))))))

(define (calc str)
  (cond ((assq '() (alg c (split str))) => cadr)
        (else "syntax error")))

(define (main)
  (let ((line (read-line)))
    (unless (eof-object? line)
      (display (calc line))
      (newline)
      (main))))

(main)
