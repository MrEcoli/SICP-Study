

(define (even? n) (= (remainder n 2) 0))

(define (sq n) (* n n))

; ex1.16

(define (fast-pow b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-pow (sq b) (/ n 2) a))
        (else (fast-pow b (- n 1) (* a b)))))



; ex1.17

(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (* a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) a)
        ((even? b) (* (double a) (halve b)))
        (else
         (+ a (* a (- b 1))))))


; ex 1.18
(define (fast-mul a b res)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) (+ res a))
        ((even? b) (fast-mul (double a) (halve b) res))
        (else
         (fast-mul a (- b 1) (+ res a)))))


(define (fib n)
  (fib-iter 1 0 0 1 n))
 
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))  ; p'
                   (+ (* 2 p q)  (square q))  ; q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))



; ex 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q n)
  (cond ((= n 0) b)
        ((even? n)
         (fib-iter a
                   b
                   (+ (sq p) (sq q))
                   (+ (sq q) (* 2 (* p q)))
                   (/ n 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- n 1)))))


; gcd

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


; fermat test

(define (expmod base exp m)
	(cond ((= exp 0) 1)
		((even? exp)
			(remainder (expmod base (/ exp 2) m) m))
		(else
			(remainder (* base (expmod base (- exp 1) m)) m))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; small divisor

(define (small-divisor n)
  (sd n 2))

(define (sd n x)
  (cond ((> (square x) n) n)
        ((= 0 (remainder n x)) x)
        (else
         (sd n (+ x 1)))))


;runtime test

(define (small-divisor n)
  (sd n 2))

(define (sd n x)
  (cond ((> (square x) n) n)
        ((= 0 (remainder n x)) x)
        (else
         (sd n (+ x 1)))))


(define (time-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time))




#lang planet neil/sicp

(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (expmod base exp m)
	(cond ((= exp 0) 1)
		((even? exp)
			(remainder (square(expmod base (/ exp 2) m)) m))
		(else
			(remainder (* base (expmod base (- exp 1) m)) m))))


; ex 1.24
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


(define (small-divisor n)
  (sd n 2))

(define (sd n x)
  (cond ((> (square x) n) n)
        ((= 0 (remainder n x)) x)
        (else
         (sd n (+ x 1)))))

(define (prime? n)
  (if (fast-prime? n 100)
      n
      (prime? (+ n 1))))

(define (time-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time))


; sum function

(define (sum term a nxt b)
  (if (> a b)
      0
      (+ (term a) (sum term (nxt a) nxt b))))


;integral function

(define (integral f a b dx)
  (define (integral-next x) ;
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) integral-next b)))


; ex 1.29

(define (integral-simpson f a b n)
  (define (h)
    (/ (- b a) n))
  (define (simpson-term a)
    (+ (* 2 (f a)) (* 4 (f (a+h)))))
  (define (simpson-next a)
    (+ a (* 2 h)))
  (* (+ (f a) (f b) (* 4 (f (+ a h))) (sum simpson-term (+ a (* 2 h)) simpson-next b) (/ h 3)))

; ex 1.30


(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))


; recursive product

(define (product term a next b res)
  (if (> a b)
    res
    (* res (term a) (product term (next a) b res))))

(define (factorial a b)
  (define (fac-term a)
    (/ (* (+ a 1) (- a 1)) (square a)))
  (define (fac-next a)
    (+ a 2))
  (product fac-term a fac-next b 1.0))

; 迭代形式实现


(define (product term a next b res)
  (define (iter x result)
    (if (> x b)
      result
      (iter next(x) (* result (term x)))))
  (iter a 1.0))

; ex 1.32

(define (accumulate combiner nulval term a next b)
  (if (> a b)
    nulval)
  (combiner (term a) (accumulate combiner nulval term (next a) next b)))

(define (product term a next b res)
  (accumulate * res term a next b))



;ex 1.33

(define (filter-accumulate combiner nulval term a next b filter)
  (define (iter a res)
    (cond ((> a b) res)
      ((filter a) (iter (next a) (combiner res (term a))))
      (else
        (iter (next a) res))))
  (iter a nulval))



; ex 1.33.1
(define (small-divisor n)
  (sd n 2))

(define (sd n x)
  (cond ((> (square x) n) n)
        ((= 0 (remainder n x)) x)
        (else
         (sd n (+ x 1)))))


(define (isprime x)
  (= (small-divisor x) x))



(define (inc x) (+ x 1))

(define (same x) (x))

(filter-accumulate + 0 same 3 inc 14 isprime)


; ex 1.33.2

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (gcd-filter a b) (= 1 (gcd a b)))


(define (relative-prime-product a b)
  (define (relative-prime? x)
    (gcd-filter x b))
  (filter-accumulate * 1 same a inc b relative-prime?))

(relative-prime-product 3 9)



;1.3.2 lambda function


(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))


(define (pi-sum a b)
	(sum (lambda (x) (/ 1.0 (* x (+ x 2))))
		a
		(lambda (x) (+ x 4))
		b))

; f(x, y) = x(1+xy)^2 + y (1 - y) + (1 + xy)(1 - y);
; a = 1 + xy
; b = 1-y
; f(x ,y) = xa^2 + yb + ab

(define (f x y)
	(define (fhelper a b)
		(+ (* x (square a))
			(* y b)
			(* a b))
		(fhelper (+ 1 (* x y)) (- 1 y))))

(define (f x y)
	((lambda (a b)
		(+ (* x (square a))
			(* y b)
			(* a b)))
	(+ 1 (* x y))
	(- 1 y)))

(define (f x y)
	(let ((a (+ 1 (* x y)))
		(b (- 1 y)))
	(+ (* x (square a))
		(* y b)
		(* a b))))

; ex 1.34

(define (f g)
	(g 2))


; binary search for zero

(define (average x y) (/ (+ x y) 2))


(define (search f neg pos)
	(let ((mid (average neg pos)))
		(if (close-enough? neg mid)
			mid
			(let ((test-value (f mid)))
				(cond ((> test-value 0)
					(search f neg mid))
				((< test-value 0)
					(search f mid pos))
				(else
					mid))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (g x) (- x 10))

(search g -11 101.12)


; 不动点

; 采用 f(x), f(f(x)),f(f(f(x)));



(define (fix-point f)
	(define (close-enough? a b) (< (abs (- a b)) 0.001))
	(define (try-it cur)
		(let ((nxt (f cur)))
			(if (close-enough? nxt cur)
				nxt
				(try-it nxt))))
	(try-it 1.0))

(fix-point cos)

(fix-point (lambda (y) (+ (sin y) (cos y))))

; ex 1.35

(fix-point (lambda (x) (+ 1 （/ 1 x)))


; ex 1.36

(define (fix-point f begin)
	(define (close-enough? a b) (< (abs (- a b)) 0.001))
	(define (try-it cur)
		(let ((nxt (f cur)))
                  (display "***")
                  (display (newline))
                  (display cur)
                  (display (newline))
			(if (close-enough? nxt cur)
				nxt
				(try-it nxt))))
	(try-it begin))

(fix-point (lambda (x) (/ (log 1000) (log x))) 7)

(define (fix-point2 f begin)
	(define (close-enough? a b) (< (abs (- a b)) 0.001))
	(define (try-it cur)
		(let ((nxt (f cur)))
                  (display "***")
                  (display (newline))
                  (display cur)
                  (display (newline))
			(if (close-enough? nxt cur)
				nxt
				(try-it (/ (+ nxt cur) 2)))))
	(try-it begin))

(display "-----------------------")

(fix-point2 (lambda (x) (/ (log 1000) (log x))) 7)


; ex 1.37

(define (cont-frac n d k)
	(if (= 0 k)
		1
		(+ (d) (/ (n) (cont-frac n d (- k 1))))))


(define (cont-frac-iterator n d k)
	(define (cont-f reslt cur)
		(if (= cur k)
			(/ (n cur) (+ (d cur) reslt))
			(cont-f (/ (n cur) (+ (d cur) reslt)) (+ cur 1))))
	(cont-f 0.0 1))



; ex 1.38
(define (cont-frac n d k) 
   (define (cont-frac-iter i result) 
     (if (= i 0) 
         result 
         (cont-frac-iter (- i 1)  
                         (/ (n i) (+ (d i) result))))) 
   (cont-frac-iter k 0.0)) 

(cont-frac (lambda (i) 1.0)
           (lambda (i) 
           	(if (or (= 1 (remainder i 3)) (= 0 (remainder i 3)))
           		1.0
           		(/ (+ i 1) 1.5)))
           10000)

; ex 1.39

(define (tan-cf x k)
	(cont-frac (lambda (i)
		(if (= i 1)
			x
			(* -1.0 x x)))
	(lambda (i) (- (* 2 i) 1))
	k)



; Newton-method x1 = x0 - f(x)/f(x)'

; ex 1.40

(define (fix-point f begin)
	(define (close-enough? a b) (< (abs (- a b)) 0.001))
	(define (try-it cur)
		(let ((nxt (f cur)))
                  (display "***")
                  (display (newline))
                  (display cur)
                  (display (newline))
			(if (close-enough? nxt cur)
				nxt
				(try-it nxt))))
	(try-it begin))

(define (dx) 0.0001)

(define (derived g)
	(lambda (x)
		(/ (- (g (+ x dx)) (g x)) dx)))


(define (newton-transform g)
	(lambda (x)
		(- x (/ (g x) ((derived g) x)))))

(define (newton-method g guess)
	(fix-point (newton-transform g) guess))

(define (cubic a b c)
	(lambda (x)
		(+ (* x x x) (* a x x) (* b x) c)))

(newton-method (cubic 1 -1 -2 10) 1)


;ex 1.41

(define (double f)
	(lambda (x) (f ( f x))))


(define (inc x) (+ x 1))
(((double (double double)) inc) 5) ;return 21


;ex 1.42
(define (compose f g)
	(lambda (x) (f (g x))))

((compose square inc) 6)


;ex 1.43

(define (repeat f n)
	(define (repeat-n cur)
	(if (= cur n)
		f
		(compose f (repeat-n (+ cur 1))))
	(repeat-n 1))

((repeat square 2) 5)


;ex 1.44
(define dx 0.0001)


(define (smooth f n)
	(repeat (lambda(x)
		(/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)) n))

;ex 1.45

