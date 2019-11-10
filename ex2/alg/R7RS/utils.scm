(define (car+cdr pair)
  (values (car pair) (cdr pair)))

(define (list->values list)
  (apply values list))

(define (flatten list)
  (apply append list))

#;(define (o . procs)
  (if-npop procs -> g
          values
          (let ((f (o procs)))
            (lambda (x) (g (f x))))))

; If list is null then if-null else if-popped with head and tail defined.
(define-syntax if-npop
  (syntax-rules (-> :)
    ((_ list -> head if-null if-popped)
     (if-npop list -> head : list if-null if-popped))
    ((_ list -> head : tail if-null if-popped)
     (if (null? list)
         if-null
         (let-values (((head tail) (car+cdr list)))
           if-popped)))))

; Like let, but optionally destructures one list at one level.
(define-syntax let-list
  (syntax-rules (=)
    ((_ (var ...) = list expr ...)
     (let-values (((var ...) (list->values list)))
       expr ...))
    ((_ var = list expr ...)
     (let ((var list))
       expr ...))))

; Friendlier syntax for map, with let-list.
(define-syntax for
  (syntax-rules ()
    ((_ var <- list expr ...)
     (map (lambda (x)
            (let-list var = x
                      expr ...))
          list))))

; Allows self-referential structures. All vals must be pairs.
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

; Splits a string into a list non-empty substrings on sep? chars.
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
