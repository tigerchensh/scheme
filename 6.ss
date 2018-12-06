
(define curry2(lambda (f)
	(lambda (m)
	(lambda (n)
		(f m n)
		))
))

(define curried-compose (lambda (f1)
	(lambda (f2)
		(lambda (l)
			(f1(f2 l))
			)
		)
))

(define compose 
	(lambda functions
	(lambda (l)
		(compose-helper (reverse functions) l)
		)
))

(define compose-helper (lambda (functions l)
	(if (null? functions)
		l
		(compose-helper (cdr functions) ((car functions) l))
		)
))


(define make-list-c (lambda (n)
	(lambda (l)
		(make-list n l)		
		)
))

(define makelist (lambda (n l)
	(if (= n 0)
		'()
		(cons l (makelist (- n 1) l))
		)
))


(define reverse-it (lambda (lst)
	(reverse-helper '() lst)
))

(define reverse-helper (lambda (r lst)
	(if (null? lst)
		r
		(reverse-helper (cons (car lst) r) (cdr lst) )
		)
	))

(define map-by-position (lambda (fn-list arg-list)
	(map (lambda (f l) (f l)) fn-list arg-list )
))

(define (empty-BST)  
	'()
)
(define empty-BST? (lambda (obj)
	(null? obj)
	))

(define BST-left (lambda (node)
	(cadr node)
	))
(define BST-right (lambda (node)
	(caddr node)
	))
(define BST-element (lambda (node)
	(car node)
	))

(define BST-insert (lambda (num bst)
	(cond [(null? bst) (list num '() '())]
		  [(= num (BST-element bst)) bst]
		  [(< num (BST-element bst)) 
		   (list (BST-element bst) (BST-insert num (BST-left bst)) (BST-right bst))
		  ]
		  [(> num (BST-element bst))
		  (list (BST-element bst)(BST-left bst) (BST-insert num (BST-right bst)))
		  ]
	)	
))

(define BST-inorder (lambda (bst)
(if (empty-BST? bst)
	'()
	(append (BST-inorder (BST-left bst))
              (list (BST-element bst))
              (BST-inorder (BST-right bst)))
	)
))

(define BST? (lambda (bst)
	(and (BST-helper? bst) (equal? (BST-inorder bst) (sort < (BST-inorder bst))))

	))

(define BST-helper? (lambda (bst)
    (if (list? bst)
    	(if (null? bst)
    		#t
    		(and 
    		(node? bst)
    		(node? (BST-helper? (BST-left bst)))
    		(node? (BST-helper? (BST-right bst))))    										   				
        )
    #f
    )
))

(define node? (lambda (node)
	(if (eq? node #t)
		#t
		(cond [(null? node) #t]
		      [(not(list? node)) #f]
		      [(not(real? (car node))) #f]
		      [(= (length node) 3) #t]
		      [else #f]
		)
	)
))

(define BST-insert-nodes (lambda (bst nums)
	(if (null? nums)
		bst
		(BST-insert-nodes (BST-insert (car nums) bst) (cdr nums))
		)
))

(define BST-height (lambda (bst)
	(cond [(empty-BST? bst) -1]
		  [else (+ 1 (max (BST-height (BST-left bst)) (BST-height (BST-right bst))))]
		)
))

(define BST-contains? (lambda (bst num)
	 (list? (member num (BST-inorder bst)))
))