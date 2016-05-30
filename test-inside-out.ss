(use-modules (srfi srfi-64) ; testing framework, guile-2.0.11
	     (ice-9 receive)
	     )

(load "inside-out.ss")

(test-begin "inside-out")

(test-begin "complex-numbers")

;; z = 2 + 3*i
;; z^16 = f(4) = F(F(F(F(2,3)))) = -815616479 - 13651680 * i

(define aa0 -815616479)
(define bb0 -13651680)

(let ((f (make-f 2 3)))
  (receive (a b) (f 4)
	   (test-eqv aa0 a)
	   (test-eqv bb0 b)))

(let ((Fn (make-Fn 4)))
  (receive (a b) (Fn 2 3)
	   (test-eqv aa0 a)
	   (test-eqv bb0 b)))

(let ((ff (make-ff 15 2 3)))
  (receive (a b) (ff 2 3)
	   (test-eqv aa0 a)
	   (test-eqv bb0 b)))

(let ((FFn (make-FFn 2 3)))
  (receive (a b) ((FFn 15) 2 3)
	   (test-eqv aa0 a)
	   (test-eqv bb0 b)))

(test-end "complex-numbers")

;; -------------------------------------------

(test-begin "pauli")

;; Properties of Pauli matricies.
(test-assert (mat= (mat* s1 s2) (sca* 0+1i s3)))
(test-assert (mat= (mat* s2 s3) (sca* 0+1i s1)))
(test-assert (mat= (mat* s3 s1) (sca* 0+1i s2)))
(test-assert (mat= (mat* s2 s1) (sca* 0-1i s3)))
(test-assert (mat= (mat* s3 s2) (sca* 0-1i s1)))
(test-assert (mat= (mat* s1 s3) (sca* 0-1i s2)))
(test-assert (mat= (mat* s1 s1) I))
(test-assert (mat= (mat* s2 s2) I))
(test-assert (mat= (mat* s3 s3) I))
(test-assert (mat= (mat* I I) I))

;; if we create a pauli combination
;;
;; q = w*I + x*s1 + y*s2 + z*s3
;;
;; and then solve for w,x,y,z, we should get back 
;; what we started with.
(let ((q (pauli-combination 1 2 3 4)))
  (test-assert (mat= '(1 2 3 4)
		     (pauli-solve q))))

;; Suppose q is a pauli combination. I can raise the
;; matrix q to the power of 16 and then solve for w,x,y,z.
;; This should give known result.

(define ww0 3826362843136)
(define xx0 1414131548160)
(define yy0 2121197322240)
(define zz0 2828263096320)

(let* ((q (pauli-combination 1 2 3 4))
       (q16 (fold mat* I (make-list 16 q))) ; q^16 by matrix multiplication.
       (z16 (pauli-solve q16))) ; q,x,y,z coefficients for p16.
  (test-assert (mat= z16
		     (list ww0 xx0 yy0 zz0))))

;; Test successive squaring by mutual recursion 
;; and by function application.

(let ((g (make-g 1 2 3 4)))
  (receive (w x y z) (g 4)
	   (test-eqv ww0 w)
	   (test-eqv xx0 x)
	   (test-eqv yy0 y)
	   (test-eqv zz0 z)))

(let ((Gn (make-Gn 4)))
  (receive (w x y z) (Gn 1 2 3 4)
	   (test-eqv ww0 w)
	   (test-eqv xx0 x)
	   (test-eqv yy0 y)
	   (test-eqv zz0 z)))

;; Test using pauli vector x*s1 + y*s2 + z*s3.
;; To do.

(test-end "pauli")

;; ------------------------------------------

(test-begin "fibonacci")

;; Test function application method.
(receive (a b) (Fibn 10)
	 (test-eqv 55 a)
	 (test-eqv 34 b))

;; Test mutual recursion method.
(receive (a b) (fib 10)
	 (test-eqv 55 a)
	 (test-eqv 34 b))

(test-end "fibonacci")

;; -------------------------------------------

(test-begin "higher-fibonacci")

(define h7 81)
(define h8 149)
(define h9 274)

;; Function application method.
(receive (a b c) (Hn 9)
	 (test-eqv h9 a)
	 (test-eqv h8 b)
	 (test-eqv h7 c))

;; Mutual recursion method.
(receive (a b c) (h 9)
	 (test-eqv h9 a)
	 (test-eqv h8 b)
	 (test-eqv h7 c))

(test-end "higher-fibonacci")

(test-end "inside-out")
