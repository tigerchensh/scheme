

(define fact
	(lambda (n)
		(if (negative? n)
			"error"
			(if (zero? n)
				1
				(* n (fact (- n 1)))))))

(define choose
	(lambda(n k)
		(/ (fact n) (* (fact k) (fact (- n k))))


		))


(define sum-of-squares 
	(lambda (lon)
		(if (null? lon) 
			0 
			(+ (* (car lon) (car lon)) (sum-of-squares (cdr lon)))
			)
		))

(define range (lambda (m n)
	(if (>= m n)
		'()
		(cons m (range (+ m 1) n))
		)
	))

(define set? (lambda (list)
	(cond [(null? list) #t]
		  [(member (car list) (cdr list)) #f ]
		  [else (set? (cdr list))]
		)
	))

(define union (lambda (s1 s2)
	(if (null? s2)
		s1
		(if (member (car s2) s1)
		(union s1 (cdr s2))
		(union (append s1 (list (car s2))) (cdr s2))
		)
	)
))


(define cross-product (lambda (v1 v2)
	(let* ([a1 (car v1)] [a2 (cadr v1)] [a3 (caddr v1)] [b1 (car v2)]
		[b2 (cadr v2)] [b3 (caddr v2)])
		 (list (-(* a2 b3) (* a3 b2)) (-(* a3 b1) (* a1 b3)) (-(* a1 b2) (* a2 b1))) 
	)
))

(define parallel? (lambda (v1 v2)
(equal? '(0 0 0) (cross-product v1 v2))
))

(define make-vec-from-points (lambda (l1 l2)
  (list (- (car l2) (car l1)) (- (cadr l2) (cadr l1)) (- (caddr l2) (caddr l1)))
  ))

(define collinear? (lambda (v1 v2 v3)
(and (parallel? (make-vec-from-points v1 v2) (make-vec-from-points v2 v3)) (parallel? (make-vec-from-points v2 v3) (make-vec-from-points v1 v3)))
))


(define magnitude (lambda (l)
  (sqrt(+(+ (* (car l) (car l)) (* (cadr l) (cadr l))) (* (caddr l) (caddr l))))

  ))

(define distance (lambda (l1 l2)
  (magnitude
    (make-vec-from-points l1 l2)
    )

  ))

(define nearest-point (lambda (p l)
	
		(nearest-point-helper (car l) (distance p (car l)) p (cdr l))
		
	)
)


(define nearest-point-helper (lambda (np m op l)
	(if (null? l)
		np
		(if (> m (distance op (car l)))
			(nearest-point-helper (car l) (distance op (car l)) op (cdr l))
			(nearest-point-helper np m op (cdr l))
			)

		)

	))



