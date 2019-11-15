(define (alg sym words)
  (if (procedure? sym)
      (if-npop words -> word
        '()
        (for r <- (sym word)
          (list words r)))
      (flatten (for (syms proc) <- sym
                 (for (words rs) <- (match syms words)
                   (list words (apply proc rs)))))))

(define (match syms words)
  (if-npop syms -> sym
    (list (list words '()))
    (flatten (for (words r) <- (alg sym words)
               (for (words rs) <- (match syms words)
                 (list words (cons r rs)))))))
