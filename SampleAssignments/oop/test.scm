;;;; Following are all the test codes for the tool

;;; Tutorial exercise 2

; definition of the class <vector>
(define-class <vector> <object> xcor ycor)

; add a new method to generic function: *
(define-method * ((v1 <vector>) (v2 <vector>))
  (+ (* (get-slot v1 'xcor)
        (get-slot v2 'xcor))
     (* (get-slot v1 'ycor)
        (get-slot v2 'ycor))))

; add a new method to generic function: +
(define-method + ((v1 <vector>) (v2 <vector>))
  (make <vector> 
        (xcor (+ (get-slot v1 'xcor)
                          (get-slot v2 'xcor)))
        (ycor (+ (get-slot v1 'ycor)
                 (get-slot v2 'ycor)))))

; add a new method to generic function: *
(define-method * ((v <vector>) (n <number>))
  (make <vector> 
        (xcor (* (get-slot v 'xcor) n))
        (ycor (* (get-slot v 'ycor) n))))
(define-method * ((n <number>) (v <vector>))
  (make <vector> 
        (xcor (* (get-slot v 'xcor) n))
        (ycor (* (get-slot v 'ycor) n))))

; define generic functions: square abs length
(define-generic-function square)
(define-generic-function abs)
(define-generic-function length)

; add a new method to generic function: square
(define-method square ((n <number>)) (* n n))

; add a new method to generic function: abs
(define-method abs ((n <number>))
  (cond ((< n 0) (- 0 n))
        (else n)))

; add a new method to generic function: length
(define-method length ((v <vector>))
  (sqrt (+ (square (get-slot v 'xcor))
           (square (get-slot v 'ycor)))))

(define-method length ((n <number>)) (abs n))

; several tests
(define v1 (make <vector> (xcor 2) (ycor 3)))
(define v2 (make <vector> (xcor 1) (ycor 2)))
(* v1 v2)                       ; value: 8
(get-slot (+ v1 v2) 'xcor)      ; value: 3
(get-slot (* v1 2) 'xcor)       ; value: 4
(get-slot (* 2 v1) 'ycor)       ; value: 6
(length v1)                     ; value: 3.605551275463989
(length -2)                     ; value: 2


;;; Tutorial exercise 5 and Lab exercise 6

; add a new method to generic function: print
(define-method print ((v <vector>))
  (print (cons (get-slot v 'xcor)
               (get-slot v 'ycor))))

; several tests
v1
; value: (2 . 3)


;;; Lab exercise 8

; several tests
(define-class <v> <object> x y)
; value: (defined-class: <v>)
; (defined generic function: x)
; (defined generic function: y)
(define v (make <v> (x 1) (y 2)))
(x v) ; value: 1
(y v) ; value: 2


;;; Lab exercise 9

;; define the complex class
(define-class <complex> <object> real imag)

; two instances
(define c1 (make <complex> (real 1) (imag 2)))
(define c2 (make <complex> (real 2) (imag 3)))

; basic operations
(define-method square ((n <number>)) (* n n))

(define-method * ((c1 <complex>) (c2 <complex>)) 
  (make <complex> 
        (real (- (* (real c1) (real c2)) 
                           (* (imag c1) (imag c2)))) 
        (imag (+ (* (real c1) (imag c2)) 
                 (* (imag c1) (real c2))))))

(define-method + ((c1 <complex>) (c2 <complex>)) 
  (make <complex> 
        (real (+ (real c1) 
                           (real c2))) 
        (imag (+ (imag c1) 
                 (imag c2)))))

(define-method - ((c1 <complex>) (c2 <complex>)) 
  (make <complex> 
        (real (- (real c1) 
                           (real c2))) 
        (imag (- (imag c1)
                 (imag c2)))))

(define-method length ((c <complex>)) 
  (sqrt (+ (square (real c)) 
           (square (imag c)))))

(define-method / ((c <complex>) (n <number>)) 
  (make <complex> 
        (real (/ (real c) n)) 
        (imag (/ (imag c) n))))

(define-method / ((c1 <complex>) (c2 <complex>)) 
  (/ (* (make <complex> 
              (real (real c2)) 
              (imag (- 0 (imag c2)))) 
        c1) 
     (+ (square (real c2)) 
        (square (imag c2)))))

; print module
(define-method print ((c <complex>)) 
  (print (cons (real c) (imag c))))

; several tests
(* c1 c2)   ; value: (-4 . 7)
(+ c1 c2)   ; value: (3 . 5)
(- c1 c2)   ; value: (-1 . -1)
(length c1) ; value: 2.23606797749979
(/ c1 c2)   ; value: (8/13 . 1/13)


;; define the segment class
(define-class <segment> <complex> xcor ycor)

;; define the dot class
(define-class <dot> <object> xcor ycor)

; length
(define-method length ((s <segment>)) 
  (length (make <complex> 
                (real (real s)) 
                (imag (imag s)))))

; several tests
(define s1 (make <segment> (real 1) (imag 2) (xcor 1) (ycor 1)))
(length s1) ; value: 2.23606797749979

; print module
(define-method print ((d <dot>)) 
  (print (cons (xcor d) (ycor d))))
(define-method print ((s <segment>)) 
  (print (cons 'complex (cons (real s) (imag s))))
  (print (cons 'dot (cons (xcor s) (ycor s)))))

; basic operation
(define-method - ((d1 <dot>) (d2 <dot>))
  (make <complex> 
        (real (- (xcor d1) 
                           (xcor d2)))
        (imag (- (ycor d1) 
                 (ycor d2)))))

(define-method cross-product ((c1 <complex>) (c2 <complex>))
  (- (* (real c1) (imag c2)) 
     (* (imag c1) (real c2))))

(define-method seperate-side? ((s <segment>) (d1 <dot>) (d2 <dot>))
  (define s1 (make <dot> (xcor (xcor s)) (ycor (ycor s))))
  (define s2 (make <dot> (xcor (+ (xcor s) (real s)))
                   (ycor (+ (ycor s) (imag s)))))
  (define result (* (cross-product
                     (- d1 s1)
                     (- s2 s1))
                    (cross-product
                     (- d2 s1)
                     (- s2 s1))))
  (cond ((< result 0) #t)
        ((= result 0) #t)
        (else #f)))

(define-method seg-cross? ((s1 <segment>) (s2 <segment>))
  (define d11 (make <dot> 
                    (xcor (xcor s1)) (ycor (ycor s1))))
  (define d12 (make <dot> 
                    (xcor (+ (xcor s1) (real s1)))
                    (ycor (+ (ycor s1) (imag s1)))))
  (define d21 (make <dot> 
                    (xcor (xcor s2)) (ycor (ycor s2))))
  (define d22 (make <dot> 
                    (xcor (+ (xcor s2) (real s2)))
                    (ycor (+ (ycor s2) (imag s2)))))
  (define result1 (seperate-side? s1 d21 d22))
  (define result2 (seperate-side? s2 d11 d12))
  (cond (result1
         (cond (result2 #t)
               (else #f)))
        (else #f)))

; several tests
(define s (make <segment> (real 1) (imag 2) (xcor 1) (ycor 1)))
(define s1 (make <segment> (real 1) (imag 0) (xcor 0) (ycor 0)))
(define s2 (make <segment> (real 1) (imag -1) (xcor 1) (ycor 2)))
(define s3 (make <segment> (real 0) (imag 1) (xcor 2) (ycor 0)))
(length s)         ; value: 2.23606797749979
(seg-cross? s s2)  ; value: #t
(seg-cross? s s1)  ; value: #f
(seg-cross? s2 s3) ; value: #t
