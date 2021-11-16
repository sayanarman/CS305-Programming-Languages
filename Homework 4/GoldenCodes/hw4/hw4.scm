(define (twoOperatorCalculator lst)
	(if (null? (cdr lst))
                (car lst)
	(if (eq? '+ (cadr lst))
		(twoOperatorCalculator(cons (+ (car lst) (caddr lst)) (cdddr lst)))
		(if (eq? '- (cadr lst))
			(twoOperatorCalculator(cons (- (car lst) (caddr lst)) (cdddr lst)))))))


(define (fourOperatorCalculator lst)
	(if (null? (cdr lst))
        	lst
        (if (eq? '* (cadr lst))
                (fourOperatorCalculator(cons (* (car lst) (caddr lst)) (cdddr lst)))
                (if (eq? '/ (cadr lst))
                	(fourOperatorCalculator(cons (/ (car lst) (caddr lst)) (cdddr lst)))
			(cons (car lst) (fourOperatorCalculator (cdr lst)))))))


(define (calculatorNested lst)
	(if (checkOperators lst) 
	(map (lambda (ele)
		(if (list? ele)
                (twoOperatorCalculator(fourOperatorCalculator(calculatorNested ele)))
                ele)) lst)
		#f))

(define (checkOperators lst)
	(if (or (null? lst) (not (list? lst)))
        #f
	(if (and (number? (car lst)) (null? (cdr lst)))
        #t
        (if (and (list? (car lst)) (null? (cdr lst)))
        (checkOperators (car lst))
	(if (and (number? (car lst)) (or (eq? '+ (cadr lst)) (eq? '- (cadr lst)) (eq? '* (cadr lst)) (eq? '/ (cadr lst))))
        (checkoperators (cddr lst))
	(if (and (list? (car lst)) (or (eq? '+ (cadr lst)) (eq? '- (cadr lst)) (eq? '* (cadr lst)) (eq? '/ (cadr lst))))
	(and (checkOperators (car lst)) (checkOperators (cddr lst)))
	#f))))))

(define (calculator lst)
	(if (checkOperators lst)
		(twoOperatorCalculator(fourOperatorCalculator(calculatorNested lst)))
		#f))
