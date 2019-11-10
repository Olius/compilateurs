(import (scheme base)
        (scheme write)
        (alg)
        (utils))

(define (gen-term s)
  (lambda (x)
    (if (equal? x s)
        (list x)
        '())))

(define s
  (let-lazy

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

    T+))

#;(define t `((() ,(lambda () #f))
            ((,(lambda (s) '(#t))) ,(lambda (x) x))))

;5 + ( ( 4 + 7 ) * 3 + 8 ) * ( 5 + 4 )

(define (calc str)
  (cond ((assq '() (alg s (split str))) => cadr)
        (else "syntax error")))

(let loop ()
  (let ((line (read-line)))
    (unless (eof-object? line)
      (display (calc line))
      (newline)
      (loop))))
