(import (scheme base)
        (scheme case-lambda)
        (scheme char)
        (scheme write))

(define (car+cdr pair)
  (values (car pair) (cdr pair)))

(define (list->values list)
  (apply values list))

(define (flatten list)
  (apply append list))

#;(define (o . procs)
  (if-pop procs -> g
          (let ((f (o procs)))
            (lambda (x) (g (f x))))
          values))

(define-syntax if-pop
  (syntax-rules (-> :)
    ((_ list -> head consequent alternate)
     (if-pop list -> head : list consequent alternate))
    ((_ list -> head : tail consequent alternate)
     (if (null? list)
         alternate
         (let-values (((head tail) (car+cdr list)))
           consequent)))))

(define-syntax let-list
  (syntax-rules (=)
    ((_ (var ...) = list expr ...)
     (let-values (((var ...) (list->values list)))
       expr ...))
    ((_ var = list expr ...)
     (let ((var list))
       expr ...))))

(define-syntax for
  (syntax-rules ()
    ((_ var <- list expr ...)
     (map (lambda (x)
            (let-list var = x
                      expr ...))
          list))))

(define-syntax let-lazy
  (syntax-rules ()
    ((_ ((var val) ...) expr ...)
     (let ((var (list #f)) ...)
       (set-car! var val) ...
       (let-values (((car cdr) (car+cdr (car var))))
         (set-car! var car)
         (set-cdr! var cdr))
       ...
       expr
       ...))))

(define (alg sym words)
  (if (procedure? sym)
      (if-pop words -> word
              (for r <- (sym word)
                (list words r))
              '())
      (flatten (for (syms proc) <- sym
                 (for (words rs) <- (match syms words)
                   (list words (apply proc rs)))))))

(define (match syms words)
  (if-pop syms -> sym
          (flatten (for (words r) <- (alg sym words)
                     (for (words rs) <- (match syms words)
                       (list words (cons r rs)))))
          (list (list words '()))))

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

(define split
  (case-lambda
    ((str) (split str char-whitespace?))
    ((str sep?) (let split ((start 0)
                            (end   0))
                 (let ((len (string-length str)))
                   (if (>= start len)
                       '()
                       (let ((end+1 (+ end 1)))
                         (if (or (>= end len)
                                 (sep? (string-ref str end)))
                             (let ((rest (split end+1 end+1)))
                               (if (= end start)
                                   rest
                                   (cons (substring str start end)
                                         rest)))
                             (split start end+1)))))))))

(let loop ()
  (let ((line (read-line)))
    (unless (eof-object? line)
      (display (cond ((assq '() (alg s (split line))) => cadr)
                     (else "syntax error")))
      (newline)
      (loop))))
