; Defines a rules object.
(define-syntax rules
  (syntax-rules ()
    ((_ init
        (def (svs . exprs) ...)
        ... )
     (let-lazy ((def (rules+ ((svs () () () exprs) ...) ()))
                ... )
       init))))

(define-syntax rules+
  (syntax-rules ()
    ((_ () ((bools (sym ...) vars exprs) ...))
     `(( (,sym ...) ,(lambda args
                        (let-list vars = (filter 'bools args)
                          . exprs)) )
       ... ))
    ((_      (( () . bsve ) . rules) (done ...))
     (rules+ rules (done ... bsve)))
    ((_      (( ((s v) . svs)
                ( bool ... )
                ( sym  ... )
                ( var  ... )
                exprs                 ) . rules)
             done)
     (rules+ (( svs
                ( bool ... #t )
                ( sym  ...  s )
                ( var  ...  v )
                exprs                 ) . rules)
             done))
    ((_      (( (tok . svs)
                ( bool ... )
                ( sym  ... )
                vars
                exprs                 ) . rules)
             done)
     (rules+ (( svs
                ( bool ... #f          )
                ( sym  ... (sterm tok) )
                vars
                exprs                 ) . rules)
             done))))

(define (filter bs xs)
  (apply append
    (map (lambda (b x)
           (if b `(,x) '()))
         bs
         xs)))

(define (sterm tok)
  (term (lambda (x)
          (equal? x tok))))

; Value returns #f if not given the right token, otherwise potentially useful info.
(define (term value)
  (lambda (x)
    (cond ((value x) => list)
          (else '()))))
