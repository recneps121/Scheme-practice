; The gcd function computes the greatest common divisor of m and n.
; Inputs - m First number
;          n Second number
; Output - the GCD
; Examples
;    (gcd 4 2) returns 2
;    (gcd 7 3) returns 1
(define (gcd m n)
  (cond ((< m n) (gcd m (- n m)))
        ((< n m) (gcd (- m n) n))
        (else m)))

; countingNumbers returns a list of values 1 to limit. Limit being supplied by the user.
(define (countingNumbers limit)
	(if (= limit 1) '(1) (append (countingNumbers (- limit 1)) (list limit)))
)

; evenNumbers creates a list from 2 to limit, with only the even values.
(define (evenNumbers limit)	
	(cond
		((= limit 1) '())
		((= limit 2) '(2))
		((even? limit) (append (evenNumbers (- limit 2)) (list limit)))
		((odd? limit) (append (evenNumbers (- limit 1)) '()))
	)
)

; primeNumbers creates a list from 2 to limit, with only the prime values
(define (primeNumbers limit)
  (filter prime?
    (remove 1 (countingNumbers limit))
  )
)

; This is a helper function that determines if a value is prime or not.
(define (prime? n)
  (let loop ((d 2))
    (cond (
      (< n (* d d)) #t)
      ((zero? (mod n d)) #f)
      ((= n 1) #f)
      (else (loop (+ d 1)))
    )
  )
)

; merge combines two lists together in sorted order, assuming the lists are already sorted.
(define (merge listOne listTwo)
  (cond 
    ((null? listOne) listTwo)
    ((null? listTwo) listOne)
    ((>= (car listOne) (car listTwo)) (cons (car listTwo) (merge listOne (cdr listTwo))))
    (else (cons (car listOne) (merge (cdr listOne) listTwo)))
  )
)

; wrap shifts the first value in a list to the end a given number of times.
(define (wrap numberToWrap aList)
  (cond
    ((null? aList) aList)
    ((= numberToWrap 0) aList)
    (else (wrap (- numberToWrap 1) (append (cdr aList) (list (car aList)))))
  )
)

; subLists recurses through a list and creates a sublist of each possible length.
(define (subLists aList)
  (if (null? aList) '() (append (subLists (reverse (cdr (reverse aList))))(list aList)))
)


; My attempt at reduceLists. Couldn't quite figure it out. :\

;(define (reduceLists func initialValue listOfLists)
;  (if (null? listOfLists) initialValue
;    (cons (fold-left func initialValue (list (car listOfLists))) (reduceLists func initialValue ;(cdr listOfLists)))  
;  )
;)
