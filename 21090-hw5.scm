(define calculatorNested (lambda (e)
(cond
((number? e) e)
((and (number? (car e)) (null? (cdr e)))  (car e))

((and (list? (car e)) (null? (cdr e))) 
(twoOperatorCalculator  (fourOperatorCalculator (calculatorNested (car e)))))


((and (not (null? (cdddr e)))  (list? (car e)) (list? (caddr e)))
(append (list (twoOperatorCalculator (fourOperatorCalculator (calculatorNested (car e))))
(cadr e) (twoOperatorCalculator (fourOperatorCalculator (calculatorNested (caddr e)))) (cadddr e))
(calculatorNested (cddddr e))
))

((and (null? (cdddr e)) (list? (car e)) (list? (caddr e)))
(list (twoOperatorCalculator (fourOperatorCalculator (calculatorNested (car e))))
(cadr e) (twoOperatorCalculator (fourOperatorCalculator (calculatorNested (caddr e))))
))

((and (list? (car e)) (number? (caddr e)) )
(append (list
 (twoOperatorCalculator (fourOperatorCalculator (calculatorNested (car e))))) (cdr e)))

( (and (number? (car e)) (not (null? (cdddr e))) (list? (caddr e)) (list? (calculatorNested (cddddr e) ))  )
(append (list (car e) (cadr e) 
(twoOperatorCalculator (fourOperatorCalculator (calculatorNested (caddr e)))) (cadddr e))
(calculatorNested (cddddr e) ) ))

( (and (number? (car e)) (null? (cdddr e)) (list? (caddr e)) )
(list (car e) (cadr e) 
(twoOperatorCalculator (fourOperatorCalculator (calculatorNested (caddr e))))))


( (and (number? (car e)) (not (null? (cdddr e))) (list? (caddr e)) (number? (calculatorNested (cddddr e) )))
(list (car e) (cadr e) 
(twoOperatorCalculator (fourOperatorCalculator (calculatorNested (caddr e)))) (cadddr e)
(calculatorNested (cddddr e) ) ))

((and (not (null? (cddr e))) (number? (caddr e)) (number? (car e))
(list? (calculatorNested (cddr e)) ))
(append (list (car e) (cadr e))
(calculatorNested (cddr e)) ))

((and (not (null? (cddr e))) (number? (caddr e)) (number? (car e))
(number? (calculatorNested (cddr e))))  
(list (car e) (cadr e) (calculatorNested (cddr e)) ))

((and (not (null? (cddr e))) (number? (caddr e)) (number? (car e)))
 (list (car e) (cadr e)
(calculatorNested (cddr e)) )
)


)))




(define (checkOperators lis)
   (cond
   ((null? lis) #t)
   ((and (number? (car lis)) (null? (cdr lis))) #t)
   ((and (not (null? (cdr lis))) (null? (cddr lis))) #f)
   ((and (list? (car lis)) (null? (cdr lis))) (checkOperators (car lis)))
   ((and (list? (car lis)) (not (null? (cddr lis))) (or (eq? (cadr lis) '*) (eq? (cadr lis) '/) (eq? (cadr lis) '+ ) (eq? (cadr lis) '-)))
    (and (checkOperators (car lis)) (checkOperators (cddr lis))) )
   ((and (number? (car lis)) (or (eq? (cadr lis) '*) (eq? (cadr lis) '/) (eq? (cadr lis) '+ ) (eq? (cadr lis) '-)) )
  (checkOperators (cddr lis)))
    (else #f)))
   


(define calculator (lambda (lis)
(cond
((eq? #t (checkOperators lis)) 
(twoOperatorCalculator (fourOperatorCalculator (calculatorNested lis))))
(else
#f
)
)))

(define twoOperatorCalculator (lambda (e)
(cond
((null? e) ())
((number? e) e)
((null? (cdr e))  e)

((and (number? (car e)) (equal? '+ (cadr e)) (not (null? (cdddr e)) ) )
(twoOperatorCalculator (cons (+ (car e) (caddr e))
(cdr (cdr (cdr e)))) ))

((and (number? (car e)) (equal? '- (cadr e)) (not (null? (cdddr e)) ) )
(twoOperatorCalculator (cons (- (car e) (caddr e))
(cdr (cdr (cdr e))))))

((and (number? (car e)) (equal? '+ (cadr e)))
(+  (car e) (twoOperatorCalculator (caddr e))))


((and (number? (car e)) (equal? '- (cadr e)) )
(-  (car e) (twoOperatorCalculator (caddr e))))

((and (not (null? (cdr e))) (list? (twoOperatorCalculator (cddr e))))
(append
(list (car e) (cadr e)) (twoOperatorCalculator (cddr e))))

((and (not (null? (cdr e))) (number? (twoOperatorCalculator (cddr e))))
(list (car e) (cadr e) (twoOperatorCalculator (cddr e))))

(else e))))

(define fourOperatorCalculator (lambda (e)
(cond
((null? e) ())
((number? e) e)
((null? (cdr e))  e)

((and (number? (car e)) (equal? '/ (cadr e)) (not (null? (cdddr e)) ) )
(fourOperatorCalculator (cons (/ (car e) (caddr e))
(cdr (cdr (cdr e)))) ))

((and (number? (car e)) (equal? '* (cadr e)) (not (null? (cdddr e)) ) )
(fourOperatorCalculator (cons (* (car e) (caddr e))
(cdr (cdr (cdr e))))))

((and (number? (car e)) (equal? '/ (cadr e)))
(/  (car e) (fourOperatorCalculator (caddr e))))


((and (number? (car e)) (equal? '* (cadr e)) )
(*  (car e) (fourOperatorCalculator (caddr e))))

((and (not (null? (cdr e))) (list? (fourOperatorCalculator (cddr e))))
(append
(list (car e) (cadr e)) (fourOperatorCalculator (cddr e))))

((and (not (null? (cdr e))) (number? (fourOperatorCalculator (cddr e))))
(list (car e) (cadr e) (fourOperatorCalculator (cddr e))))

(else e))))
