

(define intersection (lambda (m1 m2)
	(if (or (null? m2) (null? m1))
		'()
		 (intersection-helper '() m1 m2)
		)
	))

(define intersection-helper (lambda (p m1 m2)
	(if (null? m2)
		p
		(if (member (car m2) m1)
			 (intersection-helper (append p (list (car m2))) m1 (cdr m2))
			 (intersection-helper p m1 (cdr m2))
		)
	)
))


(define subset? (lambda (s1 s2)
	(if (null? s1)
		#t
		(if (member (car s1) s2)
			(subset? (cdr s1) s2)
			#f
			)
		)
))

(define set? (lambda (list)
	(cond [(null? list) #t]
		  [(member (car list) (cdr list)) #f ]
		  [else (set? (cdr list))]
		)
	))

(define relation? (lambda (l)
	(if (list? l)
		(cond 
		  [(null? l) #t]
		  [(not (list? (car l))) #f]
		  [(not (equal? (length (car l)) 2)) #f]
		  [(not (set? l)) #f]
		  [else (relation? (cdr l))]
		)
		#f
	)
))

(define domain (lambda (r)
	(if (null? r)
	'()
	(domian_helper (list (car (car r))) (cdr r))
	)
))

(define domian_helper (lambda (l r)
	(if (null? r)
		l
		(if (member (caar r) l)
			(domian_helper l (cdr r))
			(domian_helper (append (list (caar r)) l) (cdr r))
		))
	))

(define range (lambda (r)
	(if (null? r)
	'()
	(range_helper (list (cadar r)) (cdr r))
	)
))

(define range_helper (lambda (l r)
	(if (null? r)
		l
		(if (member (cadar r) l)
			(range_helper l (cdr r))
			(range_helper (append (list (cadar r)) l) (cdr r))
		))
	))


(define reflexive? (lambda (r)
		(reflexive_helper (append (domain r) (range r)) r)
	))

(define reflexive_helper(lambda (r l)
	 (if (null? r)
	 	#t
	 	(if (member (list (car r) (car r)) l)
	 		(reflexive_helper (cdr r) l)
	 		#f
	 		)
	 	)
	))


(define hailstone-step-count (lambda (n)
	(if (equal? n 1)
		0
		(if (odd? n)
			(+ 1 (hailstone-step-count (+ 1 (* 3 n))))
			(+ 1 (hailstone-step-count (/ n 2)))
			)
		)
	))

(define multi-set? (lambda (s)
    (if (relation? s)
    	(if (null? s)
    		#t
    		(and (integer? (cadar s)) (positive? (cadar s)) 
    			(not (integer? (caar s))) 
    			(equal? (length (domain s)) (length s))
    		    (multi-set? (cdr s)))
    	)
    #f
    )
))

(define ms-size (lambda (ms)
	(apply + (range ms))
	))

(define last (lambda (ls)
	(if (null? (cdr ls))
		(car ls)
		(last (cdr ls))
		)
	))

(define all-but-last (lambda (ls)
	(if (null? (cdr ls))
		'()
		(cons (car ls) (all-but-last (cdr ls)))
		)
	))

