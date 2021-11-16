(define twoOperatorCalculator
        (lambda (infixExpr)
                (let    (
                         (firstNum  (car infixExpr))
                        )
                        (if (null? (cdr infixExpr))
                                firstNum
                                (let    (
                                         (firstOp   (cadr infixExpr))
                                         (secondNum (caddr infixExpr))
                                         (rest      (cdddr infixExpr))
                                        )
                                        (if (eq? '+ firstOp)
                                                (let* (
                                                       	(result (+ firstNum secondNum))
                                                        (newInfixExpr (cons result rest))
                                                      )
                                                      (twoOperatorCalculator newInfixExpr)
                                                )
                                                (let* (
                                                       	(result (- firstNum secondNum))
                                                        (newInfixExpr (cons result rest))
                                                      )
                                                      (twoOperatorCalculator newInfixExpr)
                                                )
                                        )
                                )
                        )
                )
        )
)
(define fourOperatorCalculator
        (lambda (infixExpr)
                (let    (
                         (firstNum (car infixExpr))
                        )
                        ;; check whether there is only 1 number or not
                        (if (null? (cdr infixExpr))
                                (cons firstNum '())
                                (let    (
                                         (firstOp   (cadr infixExpr))
                                         (rest      (cddr infixExpr))
                                        )
                                        ;; check whether the operator is + or -, if it is then add it to the list
                                        ;; otherwise calculate the result by using the same structure in twoOperatorCalculator
                                        (cond
                                             	;;first condition
                                                ( (eq? '+ firstOp)
                                                  (cons firstNum (cons firstOp (fourOperatorCalculator rest)))
                                                )
                                                ;;second condition
                                                ( (eq? '- firstOp)
                                                  (cons firstNum (cons firstOp (fourOperatorCalculator rest)))
                                                )
                                                ;;third condition
                                                ( (eq? '* firstOp)
                                                  (let* (
                                                         (secondNum (car rest))
                                                         (result (* firstNum secondNum))
                                                         (newInfixExpr (cons result (cdr rest)))
                                                        )
                                                        (fourOperatorCalculator newInfixExpr)
                                                  )
                                                )
                                                ( else
                                                  (let* (
                                                         (secondNum (car rest))
                                                         (result (/ firstNum secondNum))
                                                         (newInfixExpr (cons result (cdr rest)))
                                                        )
                                                        (fourOperatorCalculator newInfixExpr)
                                                  )
                                                )
                                        )
                                )
                        )
                )
        )
)
(define calculatorNested
	(lambda	(infixExpr)
		(let	(
			 (leftOperand (car infixExpr))
			 (rest (cdr infixExpr))
			)
			(cond
				( (list? leftOperand)
				  (let*	(
					 (newLeftOperand (calculatorNested leftOperand))
					 (afterFour (fourOperatorCalculator newLeftOperand))
					 (afterTwo (twoOperatorCalculator afterFour))
					 (rest (cdr infixExpr))
					 (newInfixExpr (cons afterTwo rest))
					)
					(calculatorNested newInfixExpr)
				  )
				)
				
				( (null? rest)
				  (cons leftOperand '())
				)

				( else
				  (cons leftOperand (calculatorNested (cdr infixExpr)))
				)
			)
		)
	)
)
(define checkOperators
	(lambda (infixExpr)
		(let	(
			 (isExprList (list? infixExpr))
			)
			(if isExprList
				(let	(
					 (isEmptyList (null? infixExpr))
					)
					(if isEmptyList
						(and isExprList (not isEmptyList))
						(let*	(
							 (leftOperand (car infixExpr))
							 (isLeftList (list? leftOperand))
							 (isLeftNumber (number? leftOperand))
							)
							(cond
								( isLeftList
								  (let	(
									 (isInnerCorrect (checkOperators leftOperand))
									)
									(if isInnerCorrect
										(let*	(
											 (rest (cdr infixExpr))
											 (isRestEmpty (null? rest))
											)
											(if isRestEmpty
												isInnerCorrect
												(let*	(
													 (operator (car rest))
													 (isOperator+ (eq? '+ operator))
													 (isOperator- (eq? '- operator))
													 (isOperator* (eq? '* operator))
													 (isOperator/ (eq? '/ operator))
													 (isOperatorCorrect (or isOperator+ (or isOperator- (or isOperator* isOperator/))))
													)
													(if isOperatorCorrect
														(checkOperators (cdr rest))
														isOperatorCorrect
													)
												)
											)
										)
										isInnerCorrect
									) 
								  )
								)

								( isLeftNumber
								  (let*	(
									 (rest (cdr infixExpr))
									 (isRestEmpty (null? rest))
									)
									(if isRestEmpty 
										isLeftNumber
										(let*	(
											 (operator (car rest))
											 (isOperator+ (eq? '+ operator))
											 (isOperator- (eq? '- operator))
											 (isOperator* (eq? '* operator))
											 (isOperator/ (eq? '/ operator))
											 (isOperatorCorrect (or isOperator+ (or isOperator- (or isOperator* isOperator/))))
											)
											(if isOperatorCorrect
												(checkOperators (cdr rest))
												isOperatorCorrect
											)
										)
									)									
								  )
								)
								
								( else
								  (and isLeftList isLeftNumber)
								)
							)
						)
					)
				)
				isExprList
			)
		)
	)
)
(define calculator
	(lambda (infixExpr)
		(let	(
			 (isCorrect (checkOperators infixExpr))
			)
			(if isCorrect
				(let*	(
					 (newInfixExpr (calculatorNested infixExpr))
					 (afterFour (fourOperatorCalculator newInfixExpr))
					)
					(twoOperatorCalculator afterFour)
				)
				isCorrect
			)
		)
	)
)
