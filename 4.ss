(define matrix-ref (lambda (m row col)
	
		(if (= row 0)
			(matrix-ref-helper (car m) col)
			(matrix-ref (cdr m) (- row 1) col)
		)
	))

(define matrix-ref-helper (lambda (m col)
	(if (= col 0)
		(car m)
		(matrix-ref-helper (cdr m) (- col 1))
		)
))

(define matrix? (lambda (m)
	(if (and (list? m) (list? (car m)) (not(equal? (car m) '())) (not (letter-l (car m))))
		(if (null? (cdr m))
		#t
		(and (equal? (length (car m)) (length (cadr m))) (matrix? (cdr m))))
	#f
	)
))


(define letter-l (lambda (m)
	(if (null? (cdr m))
		#f
		(if (number? (car m))
			(letter-l (cdr m))
			(or (char-alphabetic? (string-ref (symbol->string (car m)) 0)) (letter-l (cdr m)))
		)
	)
))

(define matrix-transpose (lambda (m)
	(matrix-transpose-helper m (- (length (car m)) 1) 0)	
))

(define matrix-transpose-helper (lambda (m l n)
	(if (< l n)
	 	'()
	 	(cons (getlistlist m n) (matrix-transpose-helper m l (+ 1 n)))
	)
))

(define getlist (lambda (m n)
	(if (= n 0)
		(car m)
		(getlist (cdr m) (- n 1))
		)
))

(define getlistlist (lambda (m n)
	(if (null? m)
		'()
		(cons (getlist (car m) n) (getlistlist (cdr m) n))
		)
))

(define filter-in (lambda (f ls)
	(if (null? ls)
		'()
		(if (f (car ls))
		(cons (car ls) (filter-in f (cdr ls)))
		(filter-in f (cdr ls))
		))
))


(define filter-out (lambda (f ls)
	(if (null? ls)
		'()
		(if (not (f (car ls)))
		(cons (car ls) (filter-out f (cdr ls)))
		(filter-out f (cdr ls))
		))
))


(define invert-helper (lambda (m)
	(append (cdr m) (list (car m)))
	))

(define invert (lambda (m)
	(map invert-helper m)
))

(define list-index (lambda (pred ls)
	(list-index-helper pred 0 ls)
))

(define list-index-helper (lambda (pred n ls)
	(if (null? ls)
		#f
		(if (pred (car ls))
			n
			(list-index-helper pred (+ 1 n) (cdr ls))
		)
	)
))


(define pascal-triangle (lambda (n)
	(cond [(< n 0) '() ]
  		  [(= n 0) '((1))]	
  	      [else (append (pascal-trianglec n) '((1)))]
  	)
))

(define pascal-trianglec (lambda (n)
  	(if (= n 1) 
  		'((1 1)) 	
  		 (let ([r (pascal-trianglec (- n 1))]) (cons (cons '1(pascal-triangleline (car r))) r))
		)
	)
  )

(define pascal-triangleline (lambda (n)
  	(if (null? (cdr n))
  		'(1)
  		(cons (+ (car n) (cadr n)) (pascal-triangleline (cdr n)))
  		)
  ))