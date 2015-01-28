

(define (make-withdraw balance)
	(lambda (amount)
		(if (>= balance amount)
			(begin
				(set! balance (- balance amount))
				balance)
			"Insufficient funds")))


;ex 3.1

(define (make-accumulator start)
	(lambda (val)
		(begin
			(set! start (+ val start))
			start)))



;ex 3.2

(define (make-monitored f)
	(let ((count 0))
		(define (get cmd)
		(cond ((eq? cmd 'reset-count)
			(set! count 0)
			count)
		((eq? 'how-many cmd)
			count)
		(else
			(set! count (+ count 1))
			(f cmd))))
		get))

(define h (make-monitored sqrt))

(h 100)

(h 'how-many)

(h 'reset-count)

(h 'how-many)



;ex 3.3 and ex 3.4

(define (make-account total password)
	(let ((pw_count 0))
		(define (call-the-cops)
			"error password for too many time")
		(define (get input_pass cmd)
			(lambda (val)
				(if (eq? input_pass password)
					(cond ((eq? cmd 'withdraw) (set! total (- total val)) total) ;;还需加上一段，存款余额不足。
						((eq? cmd 'deposit) (set! total (+ total val)) total)
						(else
							"Invilid cmd"))
					(begin
						(set! pw_count (+ pw_count 1))
						(if (>= pw_count 3)
							(call-the-cops)
							"Incorrect password")))))
		get))


(define acc (make-account 100 'passw))

((acc 'passw 'withdraw) 40)
((acc 'passw 'withdraw) 30)

((acc 'passw 'deposit) 30)

((acc 'pasa 'deposit) 30)
((acc 'pasa 'deposit) 30)
((acc 'pasa 'deposit) 30)
((acc 'pasa 'deposit) 30)




(define (rand)
	(let ((x random-init))
		(lambda ()
			(set! x (rand-update x))
			x)))

(define (make-rand a b m)
	(lambda (x)
		(begin
			(remainder (+ (* a x) b) m))))

(define cur (make-rand 1123 1112 1123123))

(cur 113)
(cur 123)
(cur 12331)


;ex 3.5


(define (mente-carlp trials experiment)
	(define (iter trials-remaining trials-success)
		(cond ((= trials-remaining 0)
			(/ (* 1.0 trials-success) trials))
		((experiment)
			(iter (- trials-remaining 1) (+ 1 trials-success)))
		(else
			(iter (- trials-remaining 1) trials-success))))
  (iter trials 0))


(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (* range (random)))))


(define (calc-pi)
	(let ((x (random-in-range 2 8))
		(y (random-in-range 4 10)))
		(< (+ (square (- x 5)) (square (- y 7))) 9)))

(define val (* 4 (mente-carlp 10000 calc-pi)))




;ex 3.6

(define (make-rand a b m val)
	(define (getrand cmd)
		(cond ((eq? cmd 'generate)
			(set! val (remainder (+ b (* a val)) m))
			val)
		((eq? cmd 'reset)
		(lambda (x)
			(set! val x)))
		(else
			"unknow cmd!!!")))
	getrand)

(define rand (make-rand 113 13 10 1))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 1)

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

;result
; 3
; 106
; 60
; 28
; 3
; 106
; 60
; 28


;ex 3.8

(define (make-f)
	(let ((x 0)
		(y 0))
	(lambda (val)(begin
		(set! x y)
		(set! y val)
		x))))


(define f (make-f))

(f 0)

(f 1)

(f 2)



;ex 3.10

(define (make-withdraw initial-amount)
	(let ((balance initial-amount))
		(lambda (amount)
			(if (>= balance amount)
				(begin
					(set! balance (- balance amount))
					balance)
				"Insufficient funds"))))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

(w1 13)
(w1 32)

(w2 30)
(w2 10)


;let形式的上述make-withdraw可以写作以下的给定参数的lambda表达式
;外部的lambda函数给定了参数，即使initial-amount，内部的lambda函数有一个指向外部lamgbda函数环境的指针，能够获得第一个lambda函数的参数init
(define (make-withdraw initial-amount)
	((lambda (init)
			(lambda (amount) (if (>= init amount)
				(begin
					(set! init (- init amount))
					init)
				"Insufficient funds"))) initial-amount))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

(w1 13)
(w1 32)

(w2 30)
(w2 10)


; ex 3.12

;第一个response为 (b)
;第二个response是(b c d)

;append实现的操作时重新构造一个表，而append!实现的则是在原来的表后进行数据修改


; ex 3.13

;将链表头尾相连了
;无穷循环。

; ex 3.13


(define (mystery x)
	(define (loop x y)
		(if (null? x)
			y
			(let ((tmp (cdr x)))
				(set-cdr! x y)
				(loop tmp x))))   ;反转链表
	(loop x '()))


(define v '(a b c d e f))

(define w (mystery v))

(display w)



(define x (list 'a 'b))

(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
	(set-car! (car x) 'wow))
	x)

z1

z2



;ex 3.17

;DFS

(define (count-pair pair)
	(let ((visited '()))
	(define (dfs curnode)
		(cond ((not (pair? curnode)) 0)
			(else
				(cond ((find? curnode visited)
					0)
					((not (find? curnode visited))
						(set! visited (append visited (list curnode))) ;;增加已访问的序列
                                                ; (display curnode)
                                                ; (newline)
                                                ; (display "visited   ")
                                                ; (display visited)
                                                ; (newline)
						(+ (dfs (car curnode)) (dfs (cdr curnode)) 1))))))
	(dfs pair)))


(define (find? item visited)
	(cond ((null? visited) #f)
		((eq? (car visited) item) #t)
		(else
			(find? item (cdr visited)))))

(define (append! x y)
	(set-cdr! (last-pair x) y))

(define (last-pair x)
	(if (null? (cdr x))
		x
		(last-pair (cdr x))))


;ex 3.18 && 3.19

;;使用两个指针

(define (isCyclye curlist)
	(if (or (null? curlist) (null? (cdr curlist)))
		#f
		(let ((fast (cdr (cdr curlist)))
			(slow curlist))
		(define (iter s f)
			(cond ((eq? s f) #f)
				((or (null? f) (null? (cdr f))) #t)
				(else
					(iter (cdr s) (cdr (cdr f))))))
		(iter slow fast))))



;; 3.3.2 队列的表示

(define )



