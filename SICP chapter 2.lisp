#lang planet neil/sicp

(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))

(car (car z))

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b)))

(define (numr x) (car x))
(define (demo x) (cdr y))



; ex 2.1
(define (make-rat x y)
	(let ((p (gcd x y)))
		(cond ((and (< x 0) (> y 0))
			(cons (- (/ x p)) (- (/ y p))))
		(else
			(cons (/ x p) (/ y p))))))


(define u (make-rat -87 -99))

(difine v (make-rat -87 99))

(define w (make-rat 99 -87))

(define (print x)
	(display (newline))
	(display (car x))
	(display "/")
	(display (cdr x))
	(display (newline)))

(print u)

(print v)

(print w)

;ex 2.2


(define (make-segment sx sy ex ey)
	(cons (make-point sx sy) (make-point ex ey)))

(define (start-segment seg)
	(car seg))

(define (end-segment seg)
	(cdr seg))

(define (make-point x y)
	(cons x y))

(define (x-point point)
	(car point))

(define (y-point point)
	(cdr point))


(define (mid-point seg)
	(make-point 
		(/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2.0) 
		(/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2.0)))

(define (print-point p)
	(newline)
	(display "(")
		(display (x-point p))
		(display (","))
			(display (y-point p))
			(newline))

(print-point (mid-point (make-segment 0 -1 100 98)))



;ex 2.3, 一种以对角线上的两个点表示，另一种方法已一个顶点，长度，宽度表示

(define (make-rec x1 y1 x2 y2)
	(make-segment x1 y1 x2 y2))


;ex 2,4

(define (cons x y)
	(lambda (m) (m x y)))

(define (car c)
	(c (lambda (p q) p)))

(define (cdr c)
	(c (lambda (p q) q)))

(cdr (cons 1 2))

;ex 2.5 证明: 3与5互素，逐除获得a和b

(define (iseven a)
	(= 0 (remainder a 2)))

(define (square x) (* x x))

(define (pow base n)
	(cond ((= 0 n) 1)
		((iseven n)
			(square (pow base (/ n 2))))
		(else
			(* base (pow base (- n 1))))))

(define (cons3 a b)
	(* (pow 2 a) (pow 3 b)))

(define (car3 c)
	(define (try-it x n)
		(if (= 0 (remainder x 2))
			(try-it (/ x 2) (+ n 1))
			(- n 1)))
	(try-it c 1))

(define (cdr3 c)
	(define (try-it x n)
		(if (= 0 (remainder x 3))
			(try-it (/ x 3) (+ n 1))
			(- n 1)))
	(try-it c 1))


(car3 (cons3 3 5))
(cdr3 (cons3 3 5))



; ex 2.6

(define zero
	(lambda (f)
		(lambda (x) x)))

(define (add-1 n)
	(lambda (f)
		(lambda (x) (f ((n f) x)))))


; one 等同于 (add-1 zero)

(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f x)))

(define one
	(lambda (f)
		(lambda (x) (f x))))

; two 等同于 (add1 one)

(lambda (f) (lambda (x) (f ((one f) x))))

(lambda (f) (lambda (x) (f (f x))))

(define two
	(lambda (f)
		(lambda (x) (f (f x)))))

(define (lambdaplus n m)
	(lambda (f)
		(lambda (x)
			((n f) ((m f) x)))))

;test
(((lambdaplus one two) inc) 1)

(((lambdaplus one (lambdaplus one two)) inc) 1)



;ex 2.7


(define (make-interval a b) (cons a b))

(define (upper-bound a) (cdr a))
(define (lower-bound a) (car a))


;ex 2.8

(define (sub-interval a b)
	(make-interval
		(- (lower-bound a) (upper-bound b))
		(- (upper-bound a) (lower-bound b))))



; ex 2.8



;ex 2.10 剔除0，两边趋向正负无穷大
; 加上检验条件，除数跨越0触发异常


;ex 2.11
;; 太多的分支，其实并不能提高效率


;ex 2.12 


(define (make-center-percent center percent)
	(let ((diff (/ (* center percent) 100.0)))
		(make-interval
			(- center diff)
			(+ center diff))))

(define (print-interval interval)
	(newline)
	(display "[")
	(display (lower-bound interval))
	(display ", ")
	(display (upper-bound interval))
	(newline))


(define (center interval)
	(/ (+ (lower-bound interval) (upper-bound interval)) 2))

(define (percent interval)
	(* 100 (/ (- (center interval) (lower-bound interval)) (center interval))))

(print-interval (make-center-percent 88 15))


;list operator

(define (curlist) (list 1 2 3 4))


(define (list-ref items n)
	(if (= n 0)
		(car items)
		(list-ref (cdr items) (- n 1))))

(list-ref curlist 3)

(define (length curlist)
	(if (null? curlist)
		0
		(+ 1 (length (cdr curlist)))))

(define (length-iter curlist)
	(define (try-it res curnode)
		(if (null? curnode)
			res
			(try-it (+ 1 res) (cdr curnode))))
	(try-it 0 curlist))

(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2))))


; list append 

(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2))))


;ex 2.17

(define (last-pair curlist)
	(cond ((null? curlist) -1)
		((null (cdr curlist)) (car curlist))
		(else
			(last-pair (cdr curlist)))))


;ex 2.18

(define (reverse curlist)
	(define (try-it res curnode)
		(if (null? curnode)
			res
			(try-it (cons (car curnode) res) (cdr curnode))))
	(try-it (list) curlist))



;ex 2.19 不影响

(define us-coins (list 50 25 10 5 1)
)
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-val)
	(cond ((= amount 1) 1)
		((or (< amount 0) (nomore coin-val)) 0)
		(else
			(+
				(cc amount (except-first coin-val))
				(cc (- amount (fisrt-coin coin-val)) coin-val)))))

(define (nomore coin) (null? coin))
(define (except-first coin) (cdr coin))
(define (fisrt-coin coin) (car coin))

(cc 100 us-coins)
(cc 100 uk-coins)


;ex 2.20

(define (same-parity x . y)
	(let ((r (remainder x 2)))
		(define (get-item curnode)
			(cond ((null? curnode) nil)
				((= r (remainder (car curnode) 2))
					(cons (car curnode) (get-item (cdr curnode))))
				(else
					(get-item (cdr curnode)))))
		(cons x (get-item y))))

; test
(display (same-parity 1 3 4 1 4 5 6))



;map 与python的map function类似

(define (map curlist func)
	(if (null? curlist)
		nil
		(cons (func (car curlist) (map  (cdr curlist) func)))))



(display (map (list 1 2 3 4 5) square))

; ex 2.21

(define (square-list items)
	(if (null? items)
		nil
		(cons (square (car items)) (squar-list (cdr items)))))

(define (square-list items)
	(map items square))

(display (square-list (list 1 2 3 4 5)))

;ex 2.22
;两个都是错误的，第一个reverse了，第二个cons的第一个元素不是数值，而是序列对


;ex 2.23

(define (for-each func items)
	(define (for_iter cur)
		(cond ((null? cur) (newline))
			(else
				(func (car items))
				(for_iter (cdr items)))))
	(for_iter items))


;或者使用begin

(define (for-each func items)
	(define (for_iter cur)
          (if (not (null? cur))
              (begin			;;多条命令逐步执行
                (func (car cur))
                (for_iter (cdr cur)))))
  (for_iter items))


(for-each (lambda (x) (newline) (display (* x x)))
	(list 1 3 4 5))



;层次结构序列，如树

(define (length t)
	(if (null? t)
		0
		(+ 1 (length (cdr t)))))

(define (count-leaves t)
	(if (null? t)
		1
		(+
			(count-leaves (cdr t))
			(count-leaves (car t)))))

(define x (cons (list 1 2) (list 3 4)))

(length x)
(count-leaves x)


(display (cons (cons 1 (cons 2 nil)) (cons 3 (cons 4 nil))))


; ex 2.24

(1 (2 (3 4)))
	/ \
  	1  (2 (3 4))
  	   / \
  	  2   (3 4)
  	  	  / \
  	  	 3   4

 ; 2.25
 (define curlist1 (list 1 3 (list 5 7) 9))

 (cdr (car (cdr (cdr curlist))))

 (define curlist2 (list (list 7)))

 (car (car curlist2))

 (define curlist3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr curlist3))))))))))))


;ex 2.26

(define x (list 1 2 3))
(define y (list 4 56 13))

(append x y)

(cons x y)

(list x y)

;(1 2 3 4 56 13)   append 是将两个链表连接，cons和list等同，都是合并两个链表。
;((1 2 3) 4 56 13)
;((1 2 3) (4 56 13))


; ex 2.27


(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2))))

(define (reverse curlist)
	(define (try-it res curnode)
		(if (null? curnode)
			res
			(try-it (cons (car curnode) res) (cdr curnode))))
	(try-it (list) curlist))


(define (deep-reverse tree)
    (cond ((null? tree)         
            '())
          ((not (pair? tree))  
            tree)
          (else
            (reverse (list (deep-reverse (car tree))            
                           (deep-reverse (cadr tree)))))))



(define x (list (list 1 2) (list 3 4)))
(display (reverse x))
(display (deep-reverse x))



;ex 2.28

(define (fringe tree)
	(cond ((null? tree) nil)
		((not (pair? tree))
			(list tree))
		(else
			(append (fringe (car tree))
				(fringe (cdr tree))))))

(display (fringe (list (list 2 3) (list 5 1))))


;ex 2.29

(define (make-mobile left right)
	(list left right))

(define (make-branch length structure)
	(list length structure))

(define (left-branch mobile)
	(car mobile))

(define (right-branch mobile)
	(car (cdr mobile)))

(define (branch-length branch)
	(car branch))

(define (branch-structure branch)
	(car (cdr branch)))

;b total-weight
(define (total-weight mobile)
	(define (bracnweight curbranch)
		(cond ((null? curbranch) 0)
			((pair? (branch-structure curbranch))
				(total-weight (branch-structure curbranch)))
			(else
				(branch-structure curbranch))))
	(+ (bracnweight (left-branch mobile)) (bracnweight (right-branch mobile))))


;c isbalance

(define (bracnweight curbranch)
	(cond ((null? curbranch) 0)
		((pair? (branch-structure curbranch))
			(total-weight (branch-structure curbranch)))
		(else
			(branch-structure curbranch))))


(define (torque branch)
	(* (branch-length branch) (bracnweight branch)))

(define (isbalance tree)
	(cond ((or (null? tree) (not (pair? tree))) #t)
		((and (isbalance (branch-structure (left-branch tree)))
			(isbalance (branch-structure (right-branch tree)))
			(= (torque (left-branch tree)) (torque (right-branch tree)))) #t)
		(else
			#f)))

;d 对取分支，取重量，以及长度的函数进行修改


; map to tree

(define (map-tree func tree)
	(cond ((null? tree) nil)
		((not (pair? tree))
			(func tree))
		(else
			(cons (map-tree func (car tree)) (map-tree func (cdr tree))))))

(display (map-tree square (list 1 2 (list 3 4 (list 30)) 4 list (20))))


; ex 2.30

(define (square-tree tree)
	(map-tree square tree))

;ex 2.31

(define (map-tree func tree)
	(cond ((null? tree) nil)
		((not (pair? tree))
			(func tree))
		(else
			(cons (map-tree func (car tree)) (map-tree func (cdr tree))))))

(display (map-tree square (list 1 2 (list 3 4 (list 30)) 4 list (20))))


;ex 2.32
; 迭代，rest与 (rest + (car s)) 组成所有的组合

(define (subsets s)
	(if (null? s)
		(list nil)
		(let ((rest (subsets (cdr s))))
			(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(display (subsets (list 1 2 3)))




;;抽象 过程

;ex 2.33

;accumulate 原型

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))

(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
	(accumulate cons seq1 seq2))


(define (length sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;ex 2.34

(define (horner-eval x coefficient-sequence)
	(accumulate 
		(lambda (this-coff high-terms) (+ this-coff (* x high-terms)))
		0
		coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;ex 2.35

(define (fringe tree)
	(cond ((null? tree)
		nil)
		((not (pair/ tree))
			(list tree))
		(else
			(append (fringe (car tree)) (fringe (cdr tree))))))


(define (count-leaves tree)
	(accumulate
		(lambda (x y) (+ 1 y))
		0
		(fringe tree)))

(count-leaves curlist1)
(count-leaves curlist2)

;ex 2.36


(define (accumulate-n op init seqs)
    (if (null? (car seqs)) ;;这里不能用 car seq， 最后的一个无意义的seqs，是由多个nil组成的一个list，不能通过car seqs进行判断。
         nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

(define aa (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(display (accumulate-n + 0 aa))



; ((1 2 3) (4 5 6) (7 8 9) (10 11 12))
; ----------------------
; ((2 3) (5 6) (8 9) (11 12))
; ----------------------
; ((3) (6) (9) (12))
; ----------------------
; (() () () ())



;ex 2.37


(define u (list (list 1 2) (list 3 4)))
(define v (list (list 5 6) (list 7 8)))

(define (dot-product v w)
	(accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
	(map
		(lambda (x) (dot-product x v))
		m)


(define (transpose mat)
	(accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
	(map (lambda (r) (matrix-*-vector cols r)) m)))

;; 一步完成

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (col-of-m)
                 (map (lambda (col-of-cols)
                          (accumulate + 0 (map * col-of-cols col-of-m)))
                      cols))
             m)))


(display (matrix-*-matrix u v))


;ex 2.38

(define (fold-left op initial seq)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest))
				(cdr rest))))
	(iter initial seq))

(define (fold-right op initial seq)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op (car rest) result))
			(cdr rest)))
	(iter initial seq))

(fold-right / 1 (list 1 2 3))

(fold-left / 1 (list 1 2 3))

(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

;;满足交换律

;ex 2.39

(define (reverse sequence)
	(fold-right (lambda (x y) (cons x y)) nil sequence))

(define (reverse sequence)
	(fold-left (lambda (x y) (cons y x)) nil sequence))


(display (reverse (list 1 2 3 4)))

;(4 3 2 1)


(define (enumerate-interval x y)
	(if (> x y)
		nil
		(cons x (enumerate-interval (+ x 1) y))))
(display (enumerate-interval 3 10))

(define n 5)

(display (accumulate append nil
	(map 
		(lambda (x) (map
			(lambda (y) (cons x y)
				(enumerate-interval 1 (- x 1)))))
		(enumerate-interval 1 n)))
)

(define (min-divisor n)
	(define (try-it x)
		(cond ((> (* x x) n) n)
			((= 0 (remainder n x)) x)
			(else
				(try-it (+ x 1)))))
	(try-it 2)

(define (primer? x)
	(= x (min-divisor x)))

(define (pair-plus x)
	(+ (car x) (cadr x)))


(define (primePair x)
	(primer? (pair-plus x)))

(define (makepair x)
	(list (car x) (cadr x) (+ (car x) (cadr x))))


(display (map makepair
	(filter primePair (accumulate append '()
		            (map (lambda (x)
                   (map (lambda (y) 
                          (list x y))
				(enumerate-interval 1 (- x 1))))
		(enumerate-interval 1 n))))))




(define (flatmap proc seq)
	(accumulate append '() seq))

(define (permutation s)
	(if (null? s)
		(list '())
		(flatmap
			(lambda (x)
				(map (lambda (p) (cons x p))
					(permutation (remove x s))))
			s)))

;ex 2.40

(define (unique-pairs n)
	(accumulate append '()
		(map (lambda (x) (
			map (lambda (y)
				(list x y))
			(enumerate-interval 1 (- x 1))))
		(enumerate-interval 1 n))))

(unique-pairs 5)

;ex 2.41


(define (filter proc seq)
  (cond ((null? seq) '())
        ((proc (car seq))
         (cons (car seq) (filter proc (cdr seq))))
        (else
         (filter proc (cdr seq)))))


(define (threesum n)
	(accumulate append '()
		(map (lambda (x) (
			map (lambda (y) 
				(if (and (> (- n (+ x y)) 0) (> y (- n (+ x y))))
					(list x y (- n (+ x y)))))
			(enumerate-interval 1 (- x 1)))
		) (enumerate-interval 3 (- n 3)))))

(threesum 9)


;result
; (#<void> #<void> #<void> #<void> (4 3 2) #<void> #<void> (5 3 1) #<void> #<void> (6 2 1) #<void> #<void> #<void>)



(define (threesum n)
	(accumulate append '()
		(map (lambda (x)(map
			(lambda (y)(
				map (lambda (z)
					(list x y z))
				(filter (lambda (w) (and (< w x) (< w y) (= n (+ w x y)))) (enumerate-interval 1 (- y 1)))))
			(enumerate-interval 2 (- x 1))))
			(enumerate-interval 3 (- n 3)))))
;result
; (() () ((4 3 2)) () ((5 3 1)) () ((6 2 1)) () () ())

(define (threesum n)
	(accumulate append '()
		(map (lambda (x)
			(map (lambda (y) (list x y (- n (+ x y))))
				(filter (lambda (cur) (and (< 0 (- n (+ x cur))) (> cur (- n (+ x cur)))))
					(enumerate-interval 2 (- x 1)))))
			(enumerate-interval 3 (- n 3)))))

;result
;((4 3 2) (5 3 1) (6 2 1))


;ex 2.42

;缺点，不能排除旋转后一致重合的组合

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list '())
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (rest-of-queens)
						(map (lambda (new-row)
							(adjoin-position new-row k rest-of-queens))
						(enumerate-interval 1 board-size)))
					(queen-cols (- k 1))))))
	(queen-cols board-size))


(define (adjoin-position new-row k rest-of-queens)
	(cons new-row rest-of-queens))


(define (safe? k positions)
	(let ((val (car positions)))
		(define (iter x others)
			(cond ((null? others)
				#t)
				((or
					(= (car others) val) ;等于k的值
					(= (abs (- x k)) (abs (- val (car others))))) ;在同一斜线上
				#f)
				(else
					(iter (+ x 1) (cdr others)))))
	(iter (+ k 1) (cdr positions))))

(queens 8)


;ex 2.43

;每次添加一个数值，都要重新计算下 (queen-cols (- k 1))

;T^7


;一个图形语言

; 使用DrRacket进行此部分的操作需要添加以下模块

; #lang racket

; ( require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1))) 


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(paint (right-split einstein 3))


(define (corner-split painter n)
	(if (= n 0)
		painter
		(let ((up (up-split painter (- n 1)))
			(right (right-split painter (- n 1))))
		(let ((top-left (beside up up))
			(bottom-right (below right right))
			(corner (corner-split painter (- n 1))))
		(beside (below painter top-left)
			(below bottom-right corner))))))

;ex 2.44

(define (up-split painter n)
	(if (= n 0)
		painter
		(let ((upsmaller (up-split painter (- n 1))))
			(below painter (beside upsmaller upsmaller)))))

(paint (up-split einstein 3))

(paint (corner-split einstein 3))




;高阶操作

(define (suqare-of-four tl tr bl br)
	(lambda (painter) (let ((top (beside (tl painter) (tr painter)))
			(bottom (beside (bl painter) (br painter))))
		(below bottom top))))


(define (flipped-pairs painter)
	(let ((combine4 (suqare-of-four identity flip-vert identity flip-vert)))
		(combine4 painter)))

(paint (flipped-pairs einstein))

;ex 2.45

(define (split last-place first-place)
	(define (split-iter)
		(lambda (pic n)
			(if (= n 0)
				pic
				(let ((smaller ((split-iter) pic (- n 1)))) ;;split-iter 在这里是一个过程，必须括号后才能生成lambda函数
					(last-place pic (first-place smaller smaller))))))
	(split-iter))

(define right-split (split beside below))

(paint (right-split einstein 3))

(paint (rotate90 einstein))


;vector 框架

(define (frame-coord-map frame)
	(lambda (v)
		(add-vect
			(origin-frame frame)
			(add-vect (scale-vect (xcor-vect v)
				(edge1-frame frame))
			(scale-vect (ycor-vect v)
				(edge2-frame frame))))))

((frame-coord-map a-frame) (make-vect 0 0))


;ex 2.46

(define (make-vect x y)
	(cons x y))

(define (xcor-vect vec)
	(car vec))

(define (ycor-vect vec)
	(cdr vec))

(define (add-vect v1 v2)
	(cons (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
	(cons (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s vec)
	(cons (* s (xcor-vect vec)) (* s (ycor-vect vec))))



;ex 2.47

;a
(define (make-frame origin edge1 edge2)
	(list origin edge1 edge2))

(define (select-origin frame)
	(car frame))

(define (select-first frame)
	(cadr frame))

(define (select-second frame)
	(car (cdr (cdr frame))))


;b
(define (make-frame origin edge1 edge2)
	(cons origin (cons edge1 edge2)))

(define (select-origin frame)
	(car frame))

(define (select-first frame)
	(cadr frame))

(define (select-second frame)
	(cdr (cdr frame))))


;;2.3 符号数据


(define (memq item curlist)
	(cond ((null? curlist1) #f)
		((eq? (car curlist) item) curlist)
		(else
			(memq item (cdr curlist)))))


;ex 2.53

;easy

;ex 2.54

(define (equal? list1 list2)
	(cond ((null? list1) (null? list2))
		((null? list2) (null? list1))
		((eq? (car list1) (car list2))
			(equal (cdr lis1) (cdr list2)))
		(else
			#f)))



;ex 2.55

(car 'sddads)
;; 第一个单引号是引号符号，解释器把它当做一种指示，第二个单引号表示数据

;求导

(define (deriv exp var)
	(cond ((number? exp) 0)
		((variable? exp)
			(if (same-variable? exp var) 1 0))
		((sum? exp)
			(make-sum (deriv (append exp) var)
				(deriv (augend exp) var)))
		((product? exp)
			(make-sum
				(make-product (multiplier exp)
					(deriv (multiplicand exp) var))
				(make-product (deriv (multiplier exp) var)
					(multiplicand exp))))
		(else
			(error "unknow expression type --- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
	(and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
	(and (pair? x) (eq? (car x) '*)))


(define (product? x)
	(and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))



;ex 2.56


(define (exponentiation? s)
	(and (pair? s) (eq? (car x) '**)))

(exponentiation? '(** x y))


(define (base s)
	(cadr s))

(define (exponent s)
	(caddr s))


;ex 2.57


;ex 2.58




;ex 2.59


(define (element-of-set? x set)
	(cond ((null? set) false)
		((eq? x (car set)) true)
		(else
			(element-of-set? x (cdr set)))))


(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)))

(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
			(cons (car set1) (intersection-set (cdr set1) set2)))
		(else
			(intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
	(cond ((null? set1) set2)
		((null? set2) set1)
		((element-of-set? (car set1) set2)
			(union-set (cdr set1) set2))
		(else
			(cons (car set1) (union-set (cdr set1) set2)))))



(define s1 (list 1 2 4 5 6))
(define s2 (list 1 4 5 7 8 9))
(display (union-set s1 s2))

(display (intersection-set s1 s2))


;ex 2.60

; 不变 O(n)
(define (element-of-set? x set)
	(cond ((null? set) false)
		((eq? x (car set)) true)
		(else
			(element-of-set? x (cdr set)))))

;直接添加 O(1)

(define (adjoin-set x set)
	(cons x set))

;不用去除重复集合 O(n)
(define (union-set set1 set2)
	(append set1 set2))

;不变 O(n^2)
(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
			(cons (car set1) (intersection-set (cdr set1) set2)))
		(else
			(intersection-set (cdr set1) set2))))

;;在数据量大，而重复较少的情况下，使用后一种操作



;ex 2.61
;function of sorted set

(define (element-of-set? x set)
	(cond ((null? set) false)
		((= (car set) x) true)
		((< x (car set)) false)
		(else
			(element-of-set? x (cdr set)))))


(define (intersection-set set1 set2)
	(if (or (null? set1) (null? set2)) '()
		'()
		(let ((x1 (car set1))
			(x2 (car set2)))
		(cond ((= x1 x2)
			(cons x1 (intersection-set (cdr set1) (cdr set2))))
		((< x1 x2)
			(intersection-set (cdr set1) set2))
		(else
			(intersection-set set1 (cdr set2)))))))



; answer
(define (adjoin-set x set)
	(cond ((null? set)
		(list x))
	((< x (car set))
		(cons x set))
	((> x (car set))
		(cons (car set) (adjoin-set x (cdr set))))
	(else
		set)))

;ex 2.62
; union-set of sorted set

(define (union-set set1 set2)
	(cond ((null? set1) set2)
		((null? set2) set1)
		((< (car set1) (car set2))
			(cons (car set1) (union-set (cdr set1) set2)))
		((> (car set1) (car set2))
			(cons (car set2) (union-set set1 (cdr set2))))
		(else
			(union-set (cdr set1) set2))))


(define s1 (list 1 2 4 5 6))
(define s2 (list 1 4 5 7 8 9))

(display (union-set s1 s2))




; Set as binary tree


(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
	(list entry left right))

(define (element-of-set? x set)
	(cond ((null? set) false)
		((= x (car set)) true)
		((> x (car set))
			(element-of-set? x (right-branch set)))
		((< x (car set))
			(element-of-set? x (left-branch set)))))


(define (adjoin-set x set)
	(cond ((null? set)
		(make-tree x '() '()))
	((> x (car set))
		(make-tree (entry set)
			(left-branch set)
			(adjoin-set x (right-branch set))))
	((< x (car set))
		(make-tree (entry set)
			(adjoin-set x (left-branch set))
			(right-branch set)))))

(define (print-tree set)
	(cond ((not (null? set))
		(display (entry set))
		(print-tree (right-branch set))
		(print-tree (left-branch set)))))


;ex 2.63

(define (tree-list-1 tree)
	(if (null? tree)
		'()
		(append (tree-list-1 (left-branch tree))
			(cons (entry tree)
				(tree-list-1 (right-branch tree))))))


(define (tree-list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
			result-list
			(copy-to-list (left-branch tree)
				(cons (entry tree)
					(copy-to-list (right-branch tree)
						result-list)))))
	(copy-to-list tree '()))


(define t1 (make-tree 7 '() '()))

(define t2 (adjoin-set 1 (adjoin-set 5 (adjoin-set 11 (adjoin-set 9 (adjoin-set 3 t1))))))

(tree-list-1 t2)
(tree-list-2 t2)

;a) 结构一致

;b) 由于append操作需要重新连接一次链表，效率较低


;ex 2.64

(define (list->tree elements)
	(car (partial-tree elements (length elements))))

(define (partial-tree elts n)
	(if (= n 0)
		(cons'() elts)
		(let ((left-size (quotient (- n 1) 2)))
			(let ((left-result (partial-tree elts left-size)))
				(let ((left-tree (car left-result))
					(non-left-result (cdr left-result))
					(right-size (- n (+ left-size 1))))
					(let ((this-entry (car non-left-result))
						(right-result (partial-tree (cdr non-left-result)
							right-size)))
					(let ((right-tree (car right-result))
						(remaining-elts (cdr right-result)))
					(cons (make-tree this-entry left-tree right-tree)
						remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))


;二叉平衡树的构建

; result
; '(5
;   (1 () (3 () ()))
;   (9 (7 () ()) (11 () ())))
;每一个元素make-tree一次，复杂度为O(n)

;ex 2.65
;union-tree and intersection-tree

(define (union-tree t1 t2)
	(list->tree (union-set (tree-list-2 t1) (tree-list-2 t2))))


(define (intersection-tree t1 t2)
	(list->tree (intersection-set (tree-list-2 t1) (tree-list-2 t2))))



; ex 2.66

(define (lookup key set-tree)
	(cond ((null? set-tree) #f)
		((> key (car set-tree))
			(lookup key (right-branch set-tree)))
		((< key (car set-tree))
			(lookup key (left-branch set-tree)))
		((= key (car set-tree))
			#t)))


;Huffman tree

(define (make-leaf symbol weight)
	(list 'leaf symbol weight))

(define (leaf? object)
	(eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
	(list left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)))


(define (weight tree)
	(if (leaf? tree)
		(weight-leaf tree)
		(cadddr tree)))


;;decode of Huffman tree

(define (decode bits tree)
	(define (decode-inner bits current)
		(if (null? bits)
			'()
			(let ((next-branch (choose-branch (car bits) current)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch)
						(decode-inner (cdr bits) tree))
					(decode-inner (cdr bits) next-branch)))))
	(decode-inner bits tree))

(define (choose-branch bit branch)
	(cond ((= bit 1) (right-branch branch))
		((= bit 0) (left-branch branch))
		(else
			(error "bad bit" bit))))

(define (adjoin-set x set)
	(cond ((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		((> (weight x) (weight (car set)))
			(cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
	(if (null? pairs)
		'()
		(let ((p (car pairs)))
			(adjoin-set (make-leaf (car p)
				(cadr p))
			(make-leaf-set (cdr pairs))))))

;ex 2.67

(define sample-tree
	(make-code-tree (make-leaf 'A 4)
		(make-code-tree
			(make-leaf 'B 2)
			(make-code-tree (make-leaf 'D 1)
				(make-leaf 'C 1)))))


(define sample-messge '( 0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-messge sample-tree))
;result
;(A D A B B C A)


;ex 2.68

(define (find? x items)
	(cond ((null? items) #f)
		((eq? (car items) x) #t)
		(else
			(find? x (cdr items)))))

(deinfe (encode message tree)
	(if (null? message)
		'()
		(append (encode-symbol (car message) tree)
			(encode （cdr message) tree)))



(define (encode-symbol symbol tree)
    (cond ((leaf? tree)                                         
            '())
          ((find? symbol (symbols (left-branch tree)))          
            (cons 0
                  (encode-symbol symbol (left-branch tree))))
          ((find? symbol (symbols (right-branch tree)))        
            (cons 1
                  (encode-symbol symbol (right-branch tree))))
          (else                                                
            (error "This symbol not in tree: "))))


(display (encode '(A B C D) sample-tree))


;ex 2.69 

(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

(define (length curlist)
	(if (null? curlist)
		0
		(+ 1 (length (cdr curlist)))))

(define (successive-merge curset)
	(cond ((= (length curset) 2)
		(make-code-tree (car curset)
			(cadr curset)))
	((> (length curset) 2)
	(successive-merge (adjoin-set (make-code-tree (car curset) (cadr curset)) (cdr (cdr curset)))))
	(else
		(error " less pair of symbol"))))



(display (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

;ex 2.70


(define songcode (generate-huffman-tree '((A 2) (NA 2) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

;ex 2.71

;失衡的二叉树
;最频繁的只用1位，而最不频繁的使用(n-1)位

;ex 2.72

;encode的复杂度为O(n*logn)
;上述结果基于树是平衡的，树的高度为(logn),而查找符号表的消耗为(n)

;基于 ex 2.71的模型，最频繁的需要O(n), 而最不频繁的则需要O(n^2)





