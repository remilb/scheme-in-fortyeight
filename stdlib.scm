(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

(define (list . objs) objs)
(define (id obj) obj)

(define (flip func) (lambda (x y) (func y x)))
(define (curry func arg1)  (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g) (lambda (arg) (f (apply g arg))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define (foldr f end lst)
    (if (null? lst)
        end
        (f (car lst) (foldr f end (cdr lst)))))

(define (foldl f accum lst)
    (if (null? lst)
        accum
        (foldl f (f accum (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

(define (unfold f init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold f (f init) pred))))

(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst) (fold && #t lst))
(define (or . lst) (fold || #f lst))

(define (max first . rest) (fold (lambda (acc x) (if (> acc x) acc x)) first rest))
(define (min first . rest) (fold (lambda (acc x) (if (< acc x) acc x)) first rest))

(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))

(define (reverse lst) (reduce ((lambda (x acc) (cons x acc))) '() lst))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map f lst) (foldr (lambda (x y) (cons (f x) y)) '() lst))
(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))
