
(define interval-contains?
  (lambda (ls n)
    (and (<= (car ls) n)
         (>= (cadr ls) n))))

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

(define first
  (lambda (list)
    (car list)
    ))

(define second
  (lambda (list)
    (cadr list)))

(define third
  (lambda (list)
    (caddr list)))
  

(define make-vec-from-points (lambda (l1 l2)
  (list (- (car l2) (car l1)) (- (cadr l2) (cadr l1)) (- (caddr l2) (caddr l1)))
  ))

(define dot-product (lambda (l1 l2)
  (+(+ (* (car l1) (car l2)) (* (cadr l1) (cadr l2))) (* (caddr l1) (caddr l2)))

  ))

(define magnitude (lambda (l)
  (sqrt(+(+ (* (car l) (car l)) (* (cadr l) (cadr l))) (* (caddr l) (caddr l))))

  ))

(define distance (lambda (l1 l2)
  (magnitude
    (make-vec-from-points l1 l2)
    )

  ))