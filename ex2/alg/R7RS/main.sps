(import (scheme base)
        (scheme write)
        (alg)
        (utils)
        (rules))

(define s
  #;(let-lazy

  ; symbol   ; form                                      ; process

    ((T+ `(( (,T* ,L+)                             ,(lambda (x y) (+ x y)))))

     (T* `(( (,Tn ,L*)                             ,(lambda (x y) (* x y)))))

     (Tn `(( (,(gen-term "(") ,T+ ,(gen-term ")")) ,(lambda (x y z) y))
           ( (,(lambda (x)
                 (cond ((string->number x) => list)
                       (else '()))))               ,(lambda (x) x))))

     (L+ `(( (,(gen-term "+") ,T+)                 ,(lambda (x y) y))
           ( ()                                    ,(lambda () 0))))

     (L* `(( (,(gen-term "*") ,T*)                 ,(lambda (x y) y))
           ( ()                                    ,(lambda () 1)))))

    T+)
    
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

#;(define t `((() ,(lambda () #f))
            ((,(lambda (s) '(#t))) ,(lambda (x) x))))

;5 + ( ( 4 + 7 ) * 3 + 8 ) * ( 5 + 4 )

(define (calc str)
  (cond ((assq '() (alg s (split str))) => cadr)
        (else "syntax error")))

(define (main)
  (let ((line (read-line)))
    (unless (eof-object? line)
      (display (calc line))
      (newline)
      (main))))

(main)
