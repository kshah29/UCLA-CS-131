; checks if the given argument is a keyword
(define (is_keyword x)
    (cond 
        [(equal? x 'lambda) #t]
        [(equal? x 'λ) #t]
        [(equal? x 'if) #t]
        [else #f]
))

; checks if given argument lambda
(define (is_lambda x)
    (cond 
        [(equal? x 'lambda) #t]
        [(equal? x 'λ) #t]
        [else #f]
))
;   (member x '(let if)) )

; creates x!y create_symb
(define (create_symb x y)
    (string->symbol (string-append (symbol->string x) "!" (symbol->string y))) )

; substitutes x!y in expr
(define (substitute expr expr_opp x modified) 
	(cond
		[(equal? expr '()) '()]
		; if expr a list 
		[(list? (car expr)) (cons (substitute (car expr) expr_opp x modified) (substitute (cdr expr) expr_opp x modified))]
		; substitute all occ
		[(member (car expr) x) 
		    (cons (let ((index (index-of x (car expr)))) (list-ref modified index)) (substitute (cdr expr) expr_opp x modified))]
		[else (cons (car expr) (substitute (cdr expr) expr_opp x modified))]
	)
)

; list 1st substituted 
(define (sub_one list x modified)
	(cond 
        [(equal? list '()) '()]
        ; substitute first
		[(member (car (car list)) x)
		    (cons (let ((i (index-of x (car (car list)) ))) (cons (list-ref modified i) (cdr (car list)))) (sub_one (cdr list) x modified ))]
		[else (cons (car list) (sub_one (cdr list) x modified))]
	)
)


(define (let_bind x y a b c)
	(cond 
		[(and (equal? x '()) (equal? y '())) (list a b c)]
		[(not (equal? (car (car x)) (car (car y))))
		 (let_bind (cdr x) (cdr y) (cons (create_symb (car (car x)) (car (car y))) a) (cons (car (car x)) b) (cons (car (car y)) c))]
		[else (let_bind (cdr x) (cdr y) a b c)]
	)
)

(define (bind_lam x y a b c)
	(cond
		[(and (equal? x '()) (equal? y '()) ) (list a b c)]
		[(not (equal? (car x) (car y)))
		(bind_lam (cdr x) (cdr y) (cons (create_symb (car x) (car y)) a) (cons (car x) b) (cons (car y) c))]
		[else (bind_lam (cdr x) (cdr y) a b c)]
	)
)


; the main fn that check for two expressions 
(define (expr-compare x y)
  (cond 
    ; if the expressions are equal
    [(equal? x y) x]
    ; if the boolean expressions are not equal
    [(and (boolean? x) (boolean? y))
        (if x '% '(not %))]
    ; check if one of them is a pair OR empty list
    [(or  
        (or (not (pair? x)) (not (pair? y)))
        (not (= (length x) (length y))) )  
            (list 'if '% x y) ]
    ; check if one of them is a quote 
    [(and 
        (and (list? x) (list? y)) 
        (or (equal? (car x) 'quote) (equal? (car y) 'quote)) ) 
        (list 'if '% x y) ]
    ; if first element in the list is list 
    [(and 
        (and (list? x) (list? y)) 
        (and (list? (car x)) (list? (car y))) )
            (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
    ; both of them are list and first element is boolean 
    [(and 
        (and (list? x) (list? y))
        (and (boolean? (car x)) (boolean? (car y))) ) 
            (if (car x) 
                (append (list '%) (expr-compare (cdr x) (cdr y)) )
                (append (list '(not %)) (expr-compare (cdr x) (cdr y))) )
    ]
    ; both of them are list and the first element is lambda in both 
    [(and 
        (and (list? x) (list? y))
        (and (is_lambda (car x)) (is_lambda (car y)))  )
            (cond
                [(not (and (list? (car (cdr x))) (list? (car (cdr y)))))
                    (list 'if '% x y)]
                [(not (equal? (length (car (cdr x))) (length (car (cdr y)))))  ; original
                        (list 'if '% x y)] 
                [(or (equal? (car x) 'λ) (equal? (car y) 'λ))
                    (let ((modified (bind_lam (car (cdr x)) (car (cdr y)) '() '() '()))) 
                        (cons 'λ
                        (expr-compare
                        ; substitute x!y
                            (substitute (cdr x) (cdr y) (car (cdr modified)) (car modified)) 
                            (substitute (cdr y) (cdr y) (car (cdr (cdr modified))) (car modified)))) ; OR
                            ; (substitute (cdr y) (cdr y) (car (cdr modified)) (car modified))))
                    )]
                [else 
                    (let ((modified (bind_lam (car (cdr x)) (car (cdr y)) '() '() '()))) 
                        (cons (car x)
                            (expr-compare
                                (substitute (cdr x) (cdr y) (car (cdr modified)) (car modified)) 
                                (substitute (cdr y) (cdr y) (car (cdr (cdr modified))) (car modified)))) ; OR  
                                ; (substitute (cdr y) (cdr y) (car (cdr modified)) (car modified))))
                    )]
        )]
    ; both of them are list and first element is a keyword lambda - equal one 
    [(and 
        (and (list? x) (list? y))
        ; (and (is_keyword (car x)) (is_keyword (car y)))
        (and (equal? (car x) 'let) (equal? (car y) 'let)) )
                (cond
                    ; num of variables not saem 
                    [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) 
                        (list 'if '% x y)]
                    [else 
                        ; (car x)
                        (let ((modified (let_bind (car (cdr x)) (car (cdr y)) '() '() '() ))) 
                            (cons 'let
                                (expr-compare 
                                ; substitute x!y
                                ; x
                                    (cons 
                                    (sub_one (car (cdr x)) (car (cdr modified)) (car modified))
                                    (substitute (cdr (cdr x)) (cdr (cdr y)) (car (cdr modified)) (car modified)))
                                    ; y
                                    (cons 
                                    (sub_one (car (cdr y)) (car (cdr (cdr modified))) (car modified))		 	
                                    (substitute (cdr (cdr y)) (cdr (cdr x)) (car (cdr (cdr modified))) (car modified))) ; OR 
                                    ; (substitute (cdr (cdr y)) (cdr (cdr y)) (car (cdr modified)) (car modified)))
                                )))
                    ]
    )]
    ; both the expressions are list then check for equality, is_keyword 
    [(and (list? x) (list? y))
        (cond
            [(equal? (car x) (car y)) (cons (car x) (expr-compare (cdr x) (cdr y))) ]
            [(and (not (is_keyword (car x))) (is_keyword (car y)) ) (list 'if '% x y)]
            [(and (not (is_keyword (car y))) (is_keyword (car x)) ) (list 'if '% x y)]
            [else (cons (list 'if '% (car x) (car y)) (expr-compare (cdr x) (cdr y))) ]
    )]
    ; if the expressions which are not list are not equal 
    [(not (equal? x y)) 
        (list 'if '% x y) ]

    )
)



(define (test-expr-compare x y)
    (and (equal? (eval (list 'let '((% #t)) (expr-compare x y))) (eval x)) 
         (equal? (eval (list 'let '((% #f)) (expr-compare x y))) (eval y)) )
)


(define test-expr-x
    (list 
        12
        12
        #t
        #f
        #t
        #f
        'a
        '(cons a b)
        '(cons a b)
        '(cons (cons a b) (cons b c))
        '(cons a b)
        '(list)
        ''(a b)
        '(quote (a b))
        '(quoth (a b))
        '(if x y z)
        '(if x y z)
        '((lambda (a) (f a)) 1)
        '((lambda (a) (f a)) 1)
        '((lambda (a) a) c)
        ''((λ (a) a) c)
        '(+ #f ((λ (a b) (f a b)) 1 2))
        '((λ (a b) (f a b)) 1 2)
        '((λ (a b) (f a b)) 1 2)
        '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a)))) (lambda (b a) (b a)))
    )
)


(define test-expr-y
    (list 
        12
        20
        #t
        #f
        #f
        #t
        '(cons a b)
        '(cons a b)
        '(cons a c)
        '(cons (cons a c) (cons a c))
        '(list a b)
        '(list a)
        ''(a c)
        '(quote (a c))
        '(quoth (a c))
        '(if x z z)
        '(g x y z)
        '((lambda (a) (g a)) 2)
        '((λ (a) (g a)) 2)
        '((lambda (b) b) d)
        ''((lambda (b) b) d)
        '(+ #t ((lambda (a c) (f a c)) 1 2))
        '((λ (a b) (f b a)) 1 2)
        '((λ (a c) (f c a)) 1 2)
        '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))  (lambda (a b) (a b)))
    )
)

; (test-expr-compare test-expr-x test-expr-y )