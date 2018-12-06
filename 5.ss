(define interval-intersects?
  (lambda (m n)
    (cond ((> (car n) (cadr m)) #f)
           ((> (car m) (cadr n)) #f)
      (else #t)           
 )))

(define interval-union
  (lambda (l1 l2)
    (if (interval-intersects? l1 l2) 
        (list(list (min(car l1 ) (car l2)) (max(cadr l1) (cadr l2))
       )) (list l1 l2))))

(define minimize-interval-list (lambda (ls)
	(minimize-list (sort (lambda (x y)(< (car x)(car y))) ls))
))

(define minimize-list (lambda (ls)
	(if (null?(car ls))
		'()
		(if (null? (cdr ls))
			ls
			(let ([unli (interval-union (car ls) (cadr ls))])
				(if (= (length unli) 2)
					(cons (car unli) (minimize-list  (cdr ls)))
					 (minimize-list (append unli (cddr ls)))
			)
		))
	)
))

(define exists? (lambda (pred ls)
	(if (null? ls)
		#f
		(if (null? (car ls))
			#f
			(or (pred ls) (exists pred (cdr ls)))
		)
	)
))

(define vector-index (lambda (pred v)
	(vector-index0-helper pred v 0 (vector-length v))
	))

(define vector-index0-helper (lambda (pred v p l)
	(if (= p l)
		#f
		(if (pred (vector-ref v p))
			p
			(vector-index0-helper pred v (+ p 1) l)
		)
	)
))

(define product (lambda (set1 set2)
	(if (null? set1)
		'()
		(append (product-helper (car set1) set2) (product (cdr set1) set2))
		)
))

(define product-helper (lambda(n s2)
	(if (null?  s2)
		'()
		(cons (list n (car s2)) (product-helper n (cdr s2)))
		)
))

(define replace (lambda (old new ls)
	(map (lambda (n) (if (equal? n old) new n)) ls)
))

(define remove-last (lambda (element ls)
	(if (member element ls)
	(reverse (remove-first element (reverse ls)))
	ls
	)
))

(define remove-first (lambda (element ls)
	(if (equal? element (car ls))
		(cdr ls)
		(cons (car ls) (remove-first element (cdr ls)))
		)
))