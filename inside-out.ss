(use-modules (srfi srfi-1)   ; fold, every
	     (ice-9 receive) ; receive multiple values.
	     (ice-9 match)   ; pattern matcher.
	     )

;; ---------------------------------------------------

;; 1. PRELIMINARY.

;; Square of u.
(define (sq u) (* u u))

;; We need routines for 2x2 matrix arithmetic. 
;; 2x2 matrices are represented as row-form lists:
;;
;; [ a b
;;   c d ] = (a b c d)
;; 
;; We use pattern matching (ice-9 match) to destructure
;; these lists.

;; 2x2 matrix addition.
;;
;; (a b c d) + (a1 b1 c1 d1) = (a+a1 b+b1 c+c1 d+d1)
;;
(define (mat+ A A1)
  (match A
	 ((a b c d)
	  (match A1
		 ((a1 b1 c1 d1)
		  (list (+ a a1)
			(+ b b1)
			(+ c c1)
			(+ d d1)))))))

;; 2x2 matrix multiplication.
;;
;; (a b c d) * (a1 b1 c1 d1)
;;    = (a*a1 + b*c1
;;       a*b1 + b*d1
;;       a1*c + d*c1
;;       c*b1 + d*d1)
(define (mat* A A1)
  (match A
	 ((a b c d)
	  (match A1
		 ((a1 b1 c1 d1)
		  (list (+ (* a a1) (* b c1))
			(+ (* a b1) (* b d1))
			(+ (* a1 c) (* d c1))
			(+ (* c b1) (* d d1))))))))

;; 2x2 matrix multiplied by scalar.
;;
;; k * (a b c d) = (k*a k*b k*c k*d)
(define (sca* k A)
  (match A
	 ((a b c d)
	  (list (* k a) 
		(* k b) 
		(* k c)
		(* k d)))))

;; Guile doesn't support exact complex numbers.
;; But it seems that = compares complex numbers 
;; correctly (at least for our purpose here.)
;; Two matrices are equal if their elements
;; (which could be complex) are pairwise =.
(define (mat= A B) (every (lambda (u) u) (map = A B)))

;; Takes many functions F G H ... and composes them.
;; These functions may return multiple values. The
;; arg list of functions is reduced using fold and
;; with initial value being the procedure 'value'.
;; The procedure 'value' is the multiple-return 
;; version  of the identity function.
(define (many-compose . funcs) (fold compose values funcs))

;; --------------------------------------------------------

;; 2. EXAMPLE: COMPLEX NUMBERS.

;; We want to iterate 
;;
;; r(z) = (a + i*b)^2.
;; 
;; Every application of r squares the previous
;; application. In terms of the base case a0 + i*b0
;; this leads to the function f:
;;
;; f(n) = (a0 + b0*i) ^ (2^n)
;;
;; where n is the number of iterations. Write it like this
;;
;; f(n) = (a0 + b0*i)^(2^n) = a(n) + b(n)*i
;;
;; and solve for a(n) and b(n). After some algebra, we get 
;; these recursion relations:
;;
;; a(n) = a(n-1)^2 - b(n-1)^2
;; b(n) = 2*a(n-1)*b(n-1)
;;
;; with initial conditions
;;
;; a(0) = a0
;; b(0) = b0.

;; We will use lowercase letters ('f') to represent
;; recursive functions. make-f creates a function f
;; that evaluates (a0 + i*b0)^(2^n) using mutual recursion.
(define (make-f a0 b0)
  (lambda (m)
    (letrec ((a (lambda (n) 
		  (if (zero? n)
		      a0
		      (- (sq (a (- n 1)))
			 (sq (b (- n 1)))))))
	     (b (lambda (n)
		  (if (zero? n)
		      b0
		      (* 2
			 (a (- n 1))
			 (b (- n 1)))))))
      (values (a m)
	      (b m)))))

;; We will use capital letters (F, G, FF, ...) for
;; funcions that do the same thing by repeated application.
;;
;; If a(n-1),b(n-1) is the current state then the
;; next iteration is
;;
;; F(a(n-1),b(n-1) = a(n), b(n)
;; 
;; Notice that F takes as many arguments as it returns.
;; This makes it possible to translate the mutually 
;; recursive f into a function F that does the same 
;; thing by repeated application instead of recursion.
(define (F a b)
  (values (- (sq a) (sq b))
	  (* 2 a b)))

;; And now a multiple-application Fn is constructed from F
;; that does the same computation as the recursive f.
;;
;; Call it like this: ((Fn n) a0 b0).
(define (make-Fn n) (apply many-compose (make-list n F)))

;; If we just want powers n, and not powers 2^n,
;; we can do it by expressing complex multiplication
;; in terms of mutual recursion.
;;
;; (a0 + i*b0)*(a1 + i*b1) = (a1*a0 - b1*b0) + (a1*b0 + b1*a0)*i
;;
;; Repeated multiplication by a1 + i*b1 is expressed as 
;; a closure make-ff that repeatedly multiplies its arguments 
;; by a1 b1 m times. 
(define (make-ff m a1 b1)
  (lambda (a0 b0)
    (letrec ((a (lambda (n)
		   (if (zero? n)
		       a0
		       (- (* a1 (a (- n 1)))
			  (* b1 (b (- n 1)))))))
	     (b (lambda (n)
		  (if (zero? n)
		      b0
		      (+ (* a1 (b (- n 1)))
			 (* b1 (a (- n 1))))))))
      (values (a m)
	      (b m)))))

;; From this it follows what FF should be.
;; Or we could have started with FF and deduced
;; what the mutual recursion version should be.
(define (make-FF a1 b1)
  (lambda (a0 b0) 
    (values (- (* a1 a0)
	       (* b1 b0))
	    (+ (* a1 b0)
	       (* b1 a0)))))

;; A function that repeatedly multiplies some
;; complex number by another. For example
;;
;; (define FF (make-FFn 2 3))
;; ((FF 15) 2 3)
;; -815616479
;; -13651680
(define (make-FFn a1 b1) 
  (lambda (n)
    (apply many-compose (make-list n (make-FF a1 b1)))))

;; -----------------------------------------------------

;; 3. EXAMPLE: PAULI MATRICIES.

;; Any 2x2 matrix A can be expressed as a linear combination
;; of Pauli matrices and I:
;;
;; A = w*I + x*s1 + y*s2 + z*s3.

;; Pauli matricies.
(define I '(1 0 0 1))
(define s1 '(0 1 1 0))
(define s2 '(0 0-1i 0+1i 0))
(define s3 '(1 0 0 -1))

;; They have the following properties.
;;
;; s1*s1 = s2*s2 = s3*s3 = I
;; 
;; Counter-clockwise:
;; s1*s2 = is3
;; s2*s3 = is1
;; s3*s1 = is2
;;
;; Clockwise:
;; s2*s1 = -is3
;; s1*s3 = -is2
;; s3*s2 = -is1

;; Given coefficients w, x, y, z, this calculates A.
;; Change the name to pauli-combination.
(define (pauli-combination w x y z)
  (fold mat+ 
	'(0 0 0 0) 
	(list (sca* w I)
	      (sca* x s1)
	      (sca* y s2)
	      (sca* z s3))))

;; Given any 2x2 matrix with real or complex elements
;;
;; A = (a b c d)
;;
;; the coefficients of the pauli combination for A 
;; can be determined:
;;
;; w =  (1/2) * (a + d)
;; x =  (1/2) * (c + b)
;; y = (-i/2) * (c - b)
;; z =  (1/2) * (a - d)
(define (pauli-solve A)
  (match A
	 ((a b c d)
	  (list (* 1/2 (+ a d))
		(* 1/2 (+ c b))
		(* 1/2 0-1i (- c b))
		(* 1/2 (- a d))))))

;; If q and q' are two pauli combinatrions,
;; Multiplication works like this:
;;
;; q * q' = A*I + B*s1 + C*s2 + D*s3
;;
;; A = w*w' + x*x' + y*y' + z*z'
;; B = w'*x + w*x' + i*(y'*z - y*z')
;; C = w'*y + w*y' + i*(z'*x - z*x')  
;; D = w'*z + w*z' + i*(x'*y - x*y')
;;
;; If q = q' we have
;;
;; A = w^2 + x^2 + y^2 + z^2
;; B = 2*w*x
;; C = 2*w*y
;; D = 2*w*z

;; Which gives us a way to calculate 
;;
;; (w*I + x*s1 + y*s2 + z*s3)^(2^n)
;;
;; in a manner analogous to how we did it
;; for complex numbers: first by mutual 
;; recursion, then by repeated function
;; application.

;; 4-way mutual recursion. g(m) computes
;;
;; g(m) = (q)^(2^m).
;;
;; where q = w0*I + x0*s1 + y0*s2 + z0*s3.
(define (make-g w0 x0 y0 z0)
  (lambda (m)
    (letrec ((w (lambda (n)
		  (if (zero? n)
		      w0
		      (+ (sq (w (- n 1)))
			 (sq (x (- n 1)))
			 (sq (y (- n 1)))
			 (sq (z (- n 1)))))))
	     (x (lambda (n)
		  (if (zero? n)
		      x0
		      (* 2
			 (w (- n 1))
			 (x (- n 1))))))
	     (y (lambda (n)
		  (if (zero? n)
		      y0
		      (* 2 
			 (w (- n 1))
			 (y (- n 1))))))
	     (z (lambda (n)
		  (if (zero? n)
		      z0
		      (* 2
			 (w (- n 1))
			 (z (- n 1)))))))
      (values (w m) (x m) (y m) (z m)))))
 
;; Function-application version.
(define (G w x y z)
  (values (+ (sq w) (sq x) (sq y) (sq z))
	  (* 2 w x)
	  (* 2 w y)
	  (* 2 w z)))

;; Creates a function that applies G n times
;; to some w x y z.
(define (make-Gn n)
  (apply many-compose (make-list n G)))

;; Can we leave it as an exercise to implement
;; general multiplication and powers of Pauli 
;; combinations (q)^n analogous to how we did it
;; for complex numbers, using closures?

;; ------------------------------------------------

;; 3. EXAMPLE: FIBONACCI SEQUENCE.

;; Phrasing the Fibonacci algorithm as a  multiple return 
;; value function gives a spectacular way of calculating
;; say, the 500th Fibonacci number.
;;
;; a is the current Fibonacci number and b is the previous.
;; So one iteration of the Fibonacci algorithm gives:
;;
;; a,b => a+b,a
;;
;; Which leads immediately to the following function.
(define (Fib a b) (values (+ a b) a))

;; The reason why we have n-2 applications is that the first 
;; two Fibonacci numbers 1 and 1 are already given. So to get
;; the 10th fibonacci number, we need only 8 applications 
;; of Fib.
(define (Fibn n) ((apply many-compose (make-list (- n 2) Fib)) 1 1))

;; The Fib function gives us the right idea for writing
;; a mutually recursive way of computing Fibonacci numbers: 
;; something a bit unusual.  
(define (fib m) 
  (letrec ((a (lambda (n)
		(if (= n 1)
		    1
		    (+ (a (- n 1))
		       (b (- n 1))))))
	   (b (lambda (n)
		(if (= n 1)
		    1
		    (a (- n 1))))))
    (values (a (- m 1))
	    (b (- m 1)))))

;; -------------------------------------------------------------

;; 4. EXAMPLE: HIGHER ORDER FIBONACCI SEQUENCE.

;; Consider the following combinatorics problem: how many
;; heads/tails sequences of length n are there such that 
;; they do not contain three consecutive heads. This can
;; be solved by recursion:
;;
;; h(1) = 2
;; h(2) = 4
;; h(3) = 7
;;
;; h(n) = h(n-3) + h(n-2) + h(n-1).

;; by analogy with the Fibonacci example above, we have
;; the iteration function
;;
;; a,b,c => a+b+c,a,b
;;
;; giving:
(define (H a b c) (values (+ a b c) a b))

(define (Hn n) ((apply many-compose (make-list (- n 3) H)) 7 4 2))

;; The above are correct, they give the sequence
;; h(4) = 13 
;; h(5) = 24 
;; h(6) = 44 
;; h(7) = 81 
;; h(8) = 149
;; h(9) = 274 

;; The structure of H gives us the right idea for a mutually
;; recursive version, analogous to what was done above with
;; Fibonacci numbers.
(define (h m) 
  (letrec ((a (lambda (n)
		(if (= n 1)
		    7
		    (+ (a (- n 1))
		       (b (- n 1))
		       (c (- n 1))))))
	   (b (lambda (n)
		(if (= n 1)
		    4
		    (a (- n 1)))))
	   (c (lambda (n)
		(if (= n 1)
		    2
		    (b (- n 1))))))
    (values (a (- m 2))
	    (b (- m 2))
	    (c (- m 2)))))

;; ---------------------------------------------------

;; 5. CLASSIC MUTUAL RECURSION.

;; The classic example of mutual recursion. Can this be
;; translated into function application by turning it
;; inside out?
(define (my-even? m)
  (letrec ((e? (lambda (n)
		 (if (zero? n)
		     #t
		     (o? (- n 1)))))
	   (o? (lambda (n)
		 (if (zero? n)
		     #f
		     (e? (- n 1))))))
    (e? m)))











