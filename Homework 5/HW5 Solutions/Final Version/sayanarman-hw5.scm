(define get-operator 
  (lambda (op-symbol env)
  	(cond
    		((equal? op-symbol '+) +)
    		((equal? op-symbol '-) -)
    		((equal? op-symbol '*) *)
    		((equal? op-symbol '/) /)
    		(else 
                 (let
                  (
                   (dummy (display "cs305: ERROR\n\n"))
                  )
                  (repl env)
                 ) 
                )
	)
  )
)

(define define-stmt? 
  (lambda (e)
     (and 
       (list? e) 
       (= (length e) 3) 
       (eq? (car e) 'define)
       (symbol? (cadr e))
     )
  )
)

(define if-expr?
  (lambda (e)
    (and
      (list? e)
      (= (length e) 4)
      (eq? (car e) 'if)
    )
  )
)

;;(define check-cond-params? ;; returns #t if the condition is satisfied for the cond-params
;;  (lambda (e)
;;    ;; at this stage we will check that params for cond is <conditional_list> <else_condition>
;;    (if (null? e)
;;      #f
;;      ;;(let 
;;	;; start checking the first <conditional>
;;      ;;  (isList (list? (car e))) ;; since (car e) is <conditional> -> (<expr> <expr>) which should be list
;;     ;;  (isLength2 (= (length (car e)) 2)) ;; the length of (car e) which is <conditional> -> (<expr> <expr>) should be equal to 2
;;      ;;)
;;      	(if (and (list? (car e)) (= (length (car e)) 2))) 
;;		;; check whether the next list is <conditional_list><else_condition> or <else_condition>
;;
;;		;; check whether (car e) is <conditional> or <else_condition>
;;       		(if (eq? (caar e) 'else)
;;		
;;			;; check whether <else_condition> is at the very end
;;			(if (null? (cdr e))
;;				#t
;;				#f
;;			)
;;		
;;			;; since it is not an else statement it should be <conditional> 
;;			;; do we need to check the length again???
;;			(check-cond-params? (cdr e))
;;		)
;;		#f
;;      	)
;;    )
;;  )
;;)

(define check-cond-params2? ;; returns #t if the condition is satisfied for the cond-params
  (lambda (e)	
    ;; at this stage we will check that params for cond is <conditional_list> <else_condition>
    (if (not (null? e))
      (if (and (list? (car e)) (= (length (car e)) 2))	
        (if (eq? (caar e) 'else) ;; check whether (car e) is <conditional> or <else_condition>
          (if (null? (cdr e)) ;; check whether <else_condition> is at the very end
            #t
            #f
          )
	  (check-cond-params2? (cdr e))  ;; since it is not an else statement it should be <conditional>
          ;; do we need to check the length again???
        )
        #f
      )
      #f
    )
  )
)

(define cond-expr?
  (lambda (e)
    (and
      (list? e)
      (> (length e) 2)
      (eq? (car e) 'cond)
      ;; check parameters of cond-expr
      (check-cond-params2? (cdr e))
    )
  )
)

(define check-var-binding-list? ;; returns #t if the condition is satisfied for the <var_binding_list>
  (lambda (e)
    (if (list? e)
      (if (null? e) ;; check whether the <var_binding_list> is empty or not since it can be empty
        #t
        (if (and (list? (car e))  (= (length (car e)) 2)  (symbol? (caar e))) ;; check for ( IDENT <expr> )
          (check-var-binding-list? (cdr e)) ;; check the other  <var_binding_list> at the right hand recursively
          #f
        )
      )
      #f
    ) 
  )
)

(define let-expr?
  (lambda (e)
    (and
      (list? e)
      (= (length e) 3)
      (eq? (car e) 'let)
      (list? (cadr e)) ;;check for ( <var_binding_list> )
      (check-var-binding-list? (cadr e)) ;; check for <var_binding_list>
    )
  )
)

(define letstar-expr?
  (lambda (e)
    (and
      (list? e)
      (= (length e) 3)
      (eq? (car e) 'let*)
      (list? (cadr e)) ;;check for ( <var_binding_list> )
      (check-var-binding-list? (cadr e)) ;; check for <var_binding_list>
    )
  )
)

(define get-value 
   (lambda (var env big-env)
      (cond
        ((null? env)
         (let
          (
           (dummy (display "cs305: ERROR\n\n"))
          )
          (repl big-env)
         )
        )
        ((eq? var (caar env)) (cdar env))
        (else (get-value var (cdr env) big-env))
      )
   )
)

(define unbind
  (lambda (var old-env)
     (cond
       ((null? old-env) '())
       ((eq? (caar old-env) var) (cdr old-env))
       (else (cons (car old-env) (unbind var (cdr old-env))))
     )
  )
)

(define update-env 
  (lambda (var val old-env)
     (cons (cons var val) (unbind var old-env))
  )
)

(define hw5-interpret
  (lambda (e env)
    (cond
	;; if the expression e is a NUMBER
      ((number? e) e)

	;; if the expression e is a TIDENT
      ((symbol? e) (get-value e env env))

	;; At this stage, we need to validate
	;; whether expression e is a list or not
      ((not (list? e))
       (let
        (
         (dummy (display "cs305: ERROR\n\n"))
        )
        (repl env)
       )
      )


	;; if the expression e is a <if>
      ((if-expr? e)
	;; check whether the value of first <expr> is equal to 0 or not
       (if (eq? (hw5-interpret (cadr e) env) 0)
	   (hw5-interpret (cadddr e) env) ;; then the return value should be the value of third <expr>
	   (hw5-interpret (caddr e) env)  ;; otherwise the return value should the value of second <expr>
       )
      )

	;; if the expression e is a <cond>
      ((cond-expr? e)
       ;; check whether the length of <conditional_list> <else_condition> is 2 or bigger than 2
       (if (= (length (cdr e)) 2)

         (if (eq? (hw5-interpret (caadr e) env) 0) ;; check the value of first <expr> inside <conditional>
           (hw5-interpret (car (cdaddr e)) env)
       	   (hw5-interpret (cadadr e) env)
         )

         (if (eq? (hw5-interpret (caadr e) env) 0) ;; check the value of first <expr> inside <conditional>
           (hw5-interpret (cons (car e) (cddr e)) env)
	   (hw5-interpret (cadadr e) env)
         )
       )
      )

	;; if the expression e is a <let>
      ((let-expr? e)
	(let*
          (
           (varExprs  (map cadr  (cadr e))) ;; first get the expressions which are at the right hand side and evaluate them
           (varValues (map (lambda (expr) (hw5-interpret expr env)) varExprs)) ;; then calculate the values of the expressions
           (varNames  (map car (cadr e))) ;; then get the variable names which are the left hand side
           ;;(let-env   '()) ;; create an empty environment for the the let variables
           ;;(let-env (map (lambda (name value) (update-env name value let-env)) varNames varValues))
           (let-env   (map cons varNames varValues)) ;; create the environment for let-expr itself but this will only contains bindings from let-expr, at next step append the env to the very end
           (let-env (append let-env env)) ;; add the bigger env to the very end of list to use other variables that are declared at the bigger scope
          )
          (hw5-interpret (caddr e) let-env)
        )
      )

	;; if the expression e is a <letstar>
      ((letstar-expr? e)
	(if (< (length (cadr e)) 2)  ;; if the length of bindings is smaller than 2, then the (let* ((v1 exp1)) expr) == (let ((v1 exp1)) expr)
          (hw5-interpret (cons 'let (cdr e)) env)
          (let*
           (
            ;; (let* ((v1 exp1) (v2 exp2) ... (vn expn)) expr) == (let ((v1 exp1)) (let* ((v2 exp2) ... (vn expn)) expr))
            ;;(beginning (cons 'let (cons (caadr e) '())))
            (beginning (cons 'let (cons (cons (caadr e) '()) '())))
            (new-expr (cons 'let* (cons (cdadr e) '())))
            (new-expr (append new-expr (cddr e)))
            (new-letstar (append beginning (cons new-expr '())))
           )
           (hw5-interpret new-letstar env)
          )
        )
      )

	;; at this stage we know that the expression e
	;; is a calculation expression like (<operator> <actuals>)
      (else
        (let
          (
           (operator (get-operator (car e) env))
           (operands (map hw5-interpret (cdr e) (make-list (length (cdr e)) env)))
          )
          (apply operator operands)
        )
      )
    )
  )
)


(define repl 
  (lambda (env)
    (let*
      (
        (dummy1 (display "cs305> "))
        (expr (read))
        (new-env (if (define-stmt? expr)
                     (update-env (cadr expr) (hw5-interpret (caddr expr) env) env)
                     env))
        (val (if (define-stmt? expr)
                 (cadr expr) 
                 (hw5-interpret expr env)))
        (dummy2 (display "cs305: "))
        (dummy3 (display val))
        (dummy4 (newline))
        (dummy5 (newline))
      )
      (repl new-env)
    )
  )
)

(define cs305 (lambda () (repl '())))
