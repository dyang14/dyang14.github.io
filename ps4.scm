;;; Andrew Begel
;;; Cs294-3
;;; Garland and Heckbert's Surface Modeling Algorithm
;;; 11/4/97


;;; written in Scheme (slightly non portable, due to a borrowed heap implementation)
;;; graphics package courtesy of CS61a and 6.001 and SICP.
;;; STM files written out to Scheme compatible files by Josh MacDonald.



(define the-heap #f)

(define triangle-predicate   ;; predicate for the heap
  (lambda (x y)
    (< (delaunay-error x) (delaunay-error y))))

(define the-mesh #f)

(define maximum-error 10)
(define draw? #f) ;; whether to draw or not.

;-------------------------------

;;; (go 'ashby), (go 'crater-lake), (go 'ntc), (go 'ozark)

(define (go name)
  (load-data name)
  (set! the-heap (make-heap triangle-predicate))
  (set! the-mesh (cons 'mesh '()))
  (let ((tri1 (make-delaunay-triangle
	       (list #t #f #f)
	       (list (make-point 0 0)
		     (make-point (- data-width 1) (- data-height 1))
		     (make-point 0 (- data-height 1)))
	       0 '() 0))
	(tri2 (make-delaunay-triangle
	       (list #f #f #t)
	       (list (make-point 0 0)
		     (make-point (- data-width 1) 0)
		     (make-point (- data-width 1) (- data-height 1)))
	       0 '() 0)))
    (set-car! (delaunay-neighbors tri1) tri2)
    (set-car! (cddr (delaunay-neighbors tri2)) tri1)
    (add-delaunay-triangle! tri1 the-mesh)
    (add-delaunay-triangle! tri2 the-mesh)
    (scan-triangle tri1)
    (scan-triangle tri2)
    (if draw? (draw-mesh the-mesh))
    (greedy-insert the-mesh)))


(define (greedy-insert mesh)
  (if (not (goal-met? mesh))
	(let ((triangle (heap-extract-max! the-heap)))
	  (newline)
	  (display "max error: ")
	  (display (delaunay-error triangle))
	  (newline)
	  (insert (delaunay-candpos triangle)
		  triangle mesh)
	  (if draw? (draw-mesh mesh))
	  (greedy-insert mesh))))


(define (scan-triangle triangle)
  (let ((plane (delaunay-plane triangle))
	(best (car (delaunay-corners triangle)))
	(maxerr 0))
    (for-each (lambda (point)
		(let ((err (abs (- (h point) (interpolate-to-plane point plane)))))
		  (if (> err maxerr)
		      (begin
			(set! maxerr err)
			(set! best point)))))
	      (let ((point-in-triangle? (make-point-in-triangle? triangle)))
		(point-list point-in-triangle? (triangle-bounding-box triangle))))
    (set-delaunay-heapptr!
     triangle
     (heap-change the-heap
		  (delaunay-heapptr triangle)
		  maxerr
		  triangle))
    (set-delaunay-candpos! triangle best)))


(define (insert point triangle mesh)
  (set-used?! (height-entry-at (point-h point) (point-v point)) #t)
  (mesh-insert point triangle mesh))
		     
(define (error point mesh)
  (abs (- (H point mesh)
	  (locate-and-interpolate point mesh))))

(define (locate-and-interpolate point mesh)
  (define (helper point mesh)
    (if (null? mesh)
	#f
	(if (point-in-delaunay-triangle? point (car mesh))
	    (compute-plane point (delaunay-plane (car mesh)))
	    (helper point (cdr mesh)))))
  (helper point (cdr mesh)))

(define (goal-met? mesh)
  (if (> (heap-length the-heap) 0)
      (< (delaunay-error (heap-peek-max the-heap)) maximum-error)
      #f))
  

;-----------------


;; plane abstraction
;; H = ax + by + c
(define (make-plane a b c)
  (list a b c (lambda (x y) (+ (* a x) (* b y) c))))

(define coeff-a car)
(define coeff-b cadr)
(define coeff-c caddr)

(define (compute-plane point plane)
  ((cadddr plane) (point-h point) (point-v point)))

(define interpolate-to-plane compute-plane)

(define (make-plane-from-corners point1 point2 point3)
  (let ((x1 (point-h point1))
	(x2 (point-h point2))
	(x3 (point-h point3))
	(y1 (point-v point1))
	(y2 (point-v point2))
	(y3 (point-v point3))
	(h1 (h point1))
	(h2 (h point2))
	(h3 (h point3)))
    (let ((topa (- (* (- y2 y1) (- h2 h3))
		   (* (- y2 y3) (- h1 h2))))
	  (bota (- (* (- y2 y1) (- x2 x3))
		   (* (- y2 y3) (- x1 x2))))
	  (topb (- (* (- x2 x1) (- h3 h1))
		   (* (- x3 x1) (- h2 h1))))
	  (botb (- (* (- y3 y1) (- x2 x1))
		   (* (- y2 y1) (- x3 x1)))))
      (let ((a (/ topa bota))
	    (b (/ topb botb)))
	(let ((c (- h1 (* a x1) (* b y1))))
	  (make-plane a b c))))))
  

;----------

;; line abstraction

(define (make-line point1 point2)
  (cons point1 point2))

(define line-left car)
(define line-right cdr)

(define (point-above-line? point line)
  (let ((left (line-left line))
	(right (line-right line)))
    (let ((matrix (list (list (point-h left) (point-v left) 1)
			(list (point-h right) (point-v right) 1)
			(list (point-h point) (point-v point) 1))))
      (> (determinant matrix) 0))))

;----------------

;;rectangle abstraction

(define (make-rect topleft botright)
  (cons topleft botright))

(define rect-topleft car)

(define rect-botright cdr)

(define (rect-left rect)
  (point-h (rect-topleft rect)))

(define (rect-top rect)
  (point-v (rect-topleft rect)))

(define (rect-right rect)
  (point-h (rect-botright rect)))

(define (rect-bottom rect)
  (point-v (rect-botright rect)))

(define (point-list pred rect)   ;;; gives back a list of points inside the rect.
  (define (col-iter x y xmax result)
    (if (<= x xmax)
	(col-iter (+ x 1) y xmax
		  (let ((point (make-point x y)))
		    (if (pred point)
			(cons (make-point x y) result)
			result)))
	result))
  (define (row-iter y xmin xmax ymax result)
    (if (<= y ymax)
	(row-iter (+ y 1) xmin xmax ymax
		  (append (col-iter xmin y xmax '())
			  result))
	result))
  (row-iter (rect-top rect)
	    (rect-left rect) (rect-right rect)
	    (rect-bottom rect)
	    '()))

;------------------

(define (load-data name)
  (cond ((eq? name 'crater-lake)
	 (load "crater.stm.scm"))
	((eq? name 'ozark)
	 (load "ozark.stm.scm"))
	((eq? name 'ntc)
	 (load "ntc.stm.scm"))
	((eq? name 'ashby)
	 (load "ashby.stm.scm"))
	((eq? name 'new)
	 (load "new.stm.scm"))
	((eq? name 'small)
	 (load "small.stm.scm")))
  (make-height-field))

(define (make-height-field)
  (define (row-iter x y row)
    (if (< x data-width)
	(begin
	  (vector-set! row x
		       (make-height-entry (vector-ref row x) #f))
	  (row-iter (+ x 1) y row))))
  (define (col-iter y)
    (if (< y data-height)
	(begin 
	  (row-iter 0 y (vector-ref data y))
	  (col-iter (+ y 1)))))
  (col-iter 0))

(define (height-entry-at x y)
  (vector-ref (vector-ref data y) x))

(define (make-height-entry height used?)
  (cons height used?))

(define height car)
(define used? cdr)
(define set-height! set-car!)
(define set-used?! set-cdr!)

(define (h point)
  (height (height-entry-at (point-h point) (point-v point))))

;--------------------

;; point abstraction

(define make-point cons)
(define point-h car)
(define point-v cdr)

;------------------
	

;;delaunay triangle abstraction

(define id 0)

(define (make-delaunay-triangle neighbors corners candpos heapptr error)
  (let ((result
	 (list neighbors
	       corners
	       (make-plane-from-corners (car corners)
					(cadr corners)
					(caddr corners))
	       candpos
	       heapptr
	       error
	       id
	       #f)))
    (set! id (+ id 1))
    result))

(define (delaunay-neighbors tri)
  (car tri))

(define (replace-delaunay-neighbor! tri tri-to-remove tri-to-add)
  (if tri
      (let ((cons-cell (find-tri-pos tri-to-remove (car tri))))
	(if cons-cell
	    (set-car! cons-cell tri-to-add)))))

(define (find-tri-pos tri list)
  (if (null? list)
      #f
      (if (car list)
	  (if (= (delaunay-id tri) (delaunay-id (car list)))
	      list
	      (find-tri-pos tri (cdr list)))
	  (find-tri-pos tri (cdr list)))))

(define (delaunay-corners tri)
  (cadr tri))

(define (delaunay-plane tri)
  (caddr tri))

(define (delaunay-candpos tri)
  (cadddr tri))

(define (set-delaunay-candpos! tri val)
  (set-car! (cdddr tri) val))

(define (delaunay-heapptr tri)
  (cadr (cdddr tri)))

(define (set-delaunay-heapptr! tri val)
  (set-car! (cdr (cdddr tri)) val))

(define (Delaunay-error tri)
  (caddr (cdddr tri)))

(define (set-delaunay-error! tri val)
  (set-car! (cddr (cdddr tri)) val))

(define (delaunay-id tri)
  (cadddr (cdddr tri)))

(define (delaunay-searched? tri)
  (cadr (cdddr (cdddr tri))))

(define (set-delaunay-searched?! tri val)
  (set-car! (cdr (cdddr (cdddr tri))) val))



(define (add-delaunay-triangle! tri mesh)
  (set-cdr! mesh (cons tri (cdr mesh))))

(define (remove-delaunay-triangle! tri mesh)
  (set-cdr! mesh (remove-tri tri (cdr mesh))))

;;; remove a triangle from a list of triangles.
(define (remove-tri tri from-list)
  (define (helper from-list result)
    (if (null? from-list)
	result
	(helper (cdr from-list)
		(if (car from-list)
		    (if (= (delaunay-id tri) (delaunay-id (car from-list)))
			result
			(cons (car from-list) result))
		    (cons (car from-list) result)))))
  (helper from-list '()))


(define (triangle-bounding-box tri)
  (let ((points (delaunay-corners tri)))
    (let ((xpoints (map point-h points))
	  (ypoints (map point-v points)))
      (let ((xmin (accumulate min 999999 xpoints))
	    (xmax (accumulate max -1 xpoints))
	    (ymin (accumulate min 999999 ypoints))
	    (ymax (accumulate max -1 ypoints)))
	(make-rect (make-point xmin ymin)
		   (make-point xmax ymax))))))


;;; rotate the points and neighbors of the triangle, ccw.
(define (rotate-ccw-delaunay-triangle! tri)
  (set-car! tri (rotate-list! (car tri)))
  (set-car! (cdr tri) (rotate-list! (cadr tri))))

(define (rotate-list! list)
  (let ((cell (list-tail list (- (length list) 2))))
    (let ((thing (cdr cell)))
      (set-cdr! cell '())
      (cons (car thing) list))))

(define (point-in-delaunay-triangle? point triangle)
  (let ((corners (delaunay-corners triangle)))
    (let ((line1 (make-line (car corners) (cadr corners)))
	  (line2 (make-line (cadr corners) (caddr corners)))
	  (line3 (make-line (caddr corners) (car corners))))
      (and (point-above-line? point line1)
	   (point-above-line? point line2)
	   (point-above-line? point line3)))))


(define (make-point-in-triangle? triangle)  ;;optimized to only compute lines once.
  (let ((corners (delaunay-corners triangle)))
    (let ((line1 (make-line (car corners) (cadr corners)))
	  (line2 (make-line (cadr corners) (caddr corners)))
	  (line3 (make-line (caddr corners) (car corners))))
      (lambda (point)
	(and (point-above-line? point line1)
	     (point-above-line? point line2)
	     (point-above-line? point line3))))))


(define (mesh-insert point tri mesh)
  (let* ((corners (delaunay-corners tri))
	 (neighbors (delaunay-neighbors tri))
	 (corner-pairs (list (list (car corners) (cadr corners)) 
			     (cdr corners)
			     (list (caddr corners) (car corners)))))
    (let ((tri1 (make-delaunay-triangle            ;;; split the triangles into 3
		 (list #f (car neighbors) #f)  
		 (cons point (car corner-pairs))
		 0 '() 0))
	  (tri2 (make-delaunay-triangle
		 (list #f (cadr neighbors) #f)   
		 (cons point (cadr corner-pairs))
		 0 '() 0))
	  (tri3 (make-delaunay-triangle
		 (list #f (caddr neighbors) #f)  
		 (cons point (caddr corner-pairs))
		 0 '() 0)))
      (set-car! (delaunay-neighbors tri1) tri3)      ;;; ooh, my functional heart breaks!
      (set-car! (cddr (delaunay-neighbors tri1)) tri2)
      (set-car! (delaunay-neighbors tri2) tri1)
      (set-car! (cddr (delaunay-neighbors tri2)) tri3)
      (set-car! (delaunay-neighbors tri3) tri2)
      (set-car! (cddr (delaunay-neighbors tri3)) tri1)
      (replace-delaunay-neighbor! (car neighbors) tri tri1)  ;; fix up the neighbors
      (replace-delaunay-neighbor! (cadr neighbors) tri tri2)
      (replace-delaunay-neighbor! (caddr neighbors) tri tri3)
      (remove-delaunay-triangle! tri mesh)                  ;; get rid of old triangle
      (add-delaunay-triangle! tri1 mesh)                    ;; add new triangles
      (add-delaunay-triangle! tri2 mesh)
      (add-delaunay-triangle! tri3 mesh)
      (for-each scan-triangle (list tri1 tri2 tri3))        ;; scan them and add them to the heap
      (for-each (lambda (tri) (search point tri mesh))      ;; now check for encroaching points
		(list tri1 tri2 tri3)))))

(define (search point triangle mesh)
  (if triangle  ;make sure triangle isn't bogus
      (let ((tri-neighbor (cadr (delaunay-neighbors triangle)))) ;;neighbor opposite point
	(if tri-neighbor    ;; if there is a neighbor
	    (if (and (in-circle? point tri-neighbor mesh)  ;; if he encroaches?
		     (not (delaunay-searched? triangle)))
		(begin
		  (set-delaunay-searched?! triangle #t)
		  (let ((his-neighbors (remove-tri
					triangle
					(delaunay-neighbors tri-neighbor))))
		    (flip point triangle tri-neighbor mesh)   ;;; flip the edge
		    (for-each (lambda (tri) (search point tri mesh))  ;;; recursve
			      his-neighbors)
		    (set-delaunay-searched?! triangle #f))))))))
      
(define (in-circle? point triangle mesh)
  (let ((corners (delaunay-corners triangle)))
    (let ((corner1 (car corners))
	  (corner2 (cadr corners))
	  (corner3 (caddr corners)))
      (define (square corner)
	(+ (* (point-h corner) (point-h corner))
	   (* (point-v corner) (point-v corner))))
      (let ((matrix (list (list (point-h corner1) (point-v corner1) (square corner1) 1)
			  (list (point-h corner2) (point-v corner2) (square corner2) 1)
			  (list (point-h corner3) (point-v corner3) (square corner3) 1)
			  (list (point-h point) (point-v point) (square point) 1))))
	(> (determinant matrix) 0)))))


(define (determinant m)  ;;; from slib
  (define (butnth n lst)
    (if (zero? n) (cdr lst) (cons (car lst) (butnth (+ -1 n) (cdr lst)))))
  (define (minor m i j)
    (map (lambda (x) (butnth j x)) (butnth i m)))
  (define (cofactor m i j)
    (* (if (odd? (+ i j)) -1 1) (determinant (minor m i j))))
  (define n (length m))
  (if (eqv? 1 n) (caar m)
      (do ((j (+ -1 n) (+ -1 j))
	   (ans 0 (+ ans (* (list-ref (car m) j) (cofactor m 0 j)))))
	  ((negative? j) ans))))


;;; flip the edge between two triangles
(define (flip point tri1 tri2 mesh)   
  (if (member (car (delaunay-corners tri2))  ;;; rotate neighbor until his first point is opposite
	      (delaunay-corners tri1))       ;;; their shared edge.
      (rotate-ccw-delaunay-triangle! tri2))   
  (if (member (car (delaunay-corners tri2))
	      (delaunay-corners tri1))
      (rotate-ccw-delaunay-triangle! tri2))
  (let ((tri1-corners (delaunay-corners tri1))
	(tri2-corners (delaunay-corners tri2))
	(tri1-neighbors (delaunay-neighbors tri1))
	(tri2-neighbors (delaunay-neighbors tri2)))
    (let ((new-tri1 (make-delaunay-triangle          ;;; make the new triangles
		     (list (car tri1-neighbors)
			   (caddr tri2-neighbors)
			   #f)
		     (list point
			   (cadr tri1-corners)
			   (car tri2-corners))
		     0 '() 0))
	  (new-tri2 (make-delaunay-triangle
		     (list #f
			   (car tri2-neighbors)
			   (caddr tri1-neighbors))
		     (list point
			   (car tri2-corners)
			   (caddr tri1-corners))
		     0 '() 0)))
      (set-car! (cddr (delaunay-neighbors new-tri1)) new-tri2)  ;; fix up the structure
      (set-car! (delaunay-neighbors new-tri2) new-tri1)
      (replace-delaunay-neighbor! (car tri1-neighbors) tri1 new-tri1)  ;; fix up the neighbors
      (replace-delaunay-neighbor! (caddr tri1-neighbors) tri1 new-tri2)
      (replace-delaunay-neighbor! (car tri2-neighbors) tri2 new-tri2)
      (replace-delaunay-neighbor! (caddr tri2-neighbors) tri2 new-tri1)
      (remove-delaunay-triangle! tri1 mesh)       ;;; get rid of the old triangles from the mesh
      (remove-delaunay-triangle! tri2 mesh)
      (heap-delete the-heap (delaunay-heapptr tri1))  ;;; and the heap.
      (heap-delete the-heap (delaunay-heapptr tri2))
      (add-delaunay-triangle! new-tri1 mesh)        ;;; add the new ones to the mesh
      (add-delaunay-triangle! new-tri2 mesh)
      (scan-triangle new-tri1)                      ;;; and the heap.
      (scan-triangle new-tri2))))


;-----------------------------

;;;; "priorque.scm" priority queues for Scheme.
;;; Copyright (C) 1992, 1993, 1994, 1995, 1997 Aubrey Jaffer.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;; Algorithm from:
;;; Introduction to Algorithms by T. Cormen, C. Leiserson, R. Rivest.
;;; 1989 MIT Press.

;; Record type.
(define heap:rtd (make-record-type "heap" '(array size heap<?)))

;; Constructor.
(define heap:make-heap
  (let ((cstr (record-constructor heap:rtd)))
    (lambda (pred<?)
      (cstr (make-vector 4) 0 pred<?))))

;; Reference an element.
(define heap:ref
  (let ((ra (record-accessor heap:rtd 'array)))
    (lambda (a i)
      (vector-ref (ra a) (+ -1 i)))))

;; Set an element.
(define heap:set!
  (let ((ra (record-accessor heap:rtd 'array)))
    (lambda (a i v)
      (let ((cell (cons i v)))
      (vector-set! (ra a) (+ -1 i) cell)
      cell))))

;; Exchange two elements.
(define heap:exchange
  (let ((aa (record-accessor heap:rtd 'array)))
    (lambda (a i j)
      (set! i (+ -1 i))
      (set! j (+ -1 j))
      (let* ((ra (aa a))
	     (tmp (vector-ref ra i))
	     (other (vector-ref ra j)))
	(set-car! other (+ i 1))
	(set-car! tmp (+ j 1))
	(vector-set! ra i other)
	(vector-set! ra j tmp)))))


;; Get length.
(define heap:length (record-accessor heap:rtd 'size))

(define heap:heap<? (record-accessor heap:rtd 'heap<?))

(define heap:set-size!
  (let ((aa (record-accessor heap:rtd 'array))
	(am (record-modifier heap:rtd 'array))
	(sm (record-modifier heap:rtd 'size)))
    (lambda (a s)
      (let ((ra (aa a)))
	(if (> s (vector-length ra))
	    (let ((nra (make-vector (+ s (quotient s 2)))))
	      (do ((i (+ -1 (vector-length ra)) (+ -1 i)))
		  ((negative? i) (am a nra))
		(vector-set! nra i (cons (+ i 1) (cdr (vector-ref ra i)))))))
	(sm a s)))))

(define (heap:parent i) (quotient i 2))
(define (heap:left i) (* 2 i))
(define (heap:right i) (+ 1 (* 2 i)))

(define (heap:heapify a i)
  (let* ((l (heap:left i))
	 (r (heap:right i))
	 (largest (if (and (<= l (heap:length a))
			   ((heap:heap<? a) (cdr (heap:ref a i)) (cdr (heap:ref a l))))
		      l
		      i)))
    (cond ((and (<= r (heap:length a))
		((heap:heap<? a) (cdr (heap:ref a largest)) (cdr (heap:ref a r))))
	   (set! largest r)))
    (cond ((not (= largest i))
	   (heap:exchange a i largest)
	   (heap:heapify a largest)))))

(define (heap:insert! a key)
  (define i (+ 1 (heap:length a)))
  (heap:set-size! a i)
  (do ()
      ((not (and (> i 1)
		 ((heap:heap<? a) (cdr (heap:ref a (heap:parent i))) key))))
    (heap:set! a i (cdr (heap:ref a (heap:parent i))))
    (set! i (heap:parent i)))
  (heap:set! a i key))

(define (heap:extract-max! a)
  (if (< (heap:length a) 1)
      (slib:error "heap underflow" a))
  (let ((max (cdr (heap:ref a 1))))
    (heap:set! a 1 (cdr (heap:ref a (heap:length a))))
    (heap:set-size! a (+ -1 (heap:length a)))
    (heap:heapify a 1)
    max))

(define (heap-delete heap heap-cons)
  (if (not (null? heap-cons))
      (begin
	(set-delaunay-error! (cdr heap-cons) 999999)
	(heap:heapify heap (car heap-cons))
	(heap-extract-max! heap)
	'())))

(define (heap-change heap heap-cons key triangle)
  (if (not (null? heap-cons))
      (if (>= key 0)
	  (begin
	    (set-delaunay-error! (cdr heap-cons) key)
	    (heap:heapify heap (car heap-cons))
	    heap-cons)
	  (begin
	    (set-delaunay-error! (cdr heap-cons) 999999)
	    (heap:heapify heap (car heap-cons))
	    (heap-extract-max! heap)
	    '()))
      (if (>= key 0)
	  (begin
	    (set-delaunay-error! triangle key)
	    (heap-insert! heap triangle))
	  heap-cons)))

  
(define (heap-peek-max heap)
  (if (< (heap:length heap) 1)
      (slib:error "heap underflow" heap))
  (cdr (heap:ref heap 1)))

;;
;; Externals.
;;
(define make-heap heap:make-heap)
(define heap-insert! heap:insert!)
(define heap-extract-max! heap:extract-max!)
(define heap-length heap:length)

(define (heap:test)
  (require 'debug)
  (let ((heap #f))
    (set! heap (make-heap char>?))
    (heap-insert! heap #\A)
    (heap-insert! heap #\Z)
    (heap-insert! heap #\G)
    (heap-insert! heap #\B)
    (heap-insert! heap #\G)
    (heap-insert! heap #\Q)
    (heap-insert! heap #\S)
    (heap-insert! heap #\R)
    (do ((i 7 (+ -1 i)))
	((negative? i))
      (write (heap-extract-max! heap)) (newline))))



;----------------------
;; berkeley.scm and scm version of scheme required to make this graphics go.
;; Code for CS61A project 2 -- picture language

(define make-vect cons)
(Define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
	     (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
	     (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect scalefactor vect)
  (make-vect (* scalefactor (xcor-vect vect))
	     (* scalefactor (ycor-vect vect))))

(define make-frame list)
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) 200) 100)
	 (- (* (ycor-vect v1) 200) 100))
  (pendown)
  (setxy (- (* (xcor-vect v2) 200) 100)
	 (- (* (ycor-vect v2) 200) 100)))

(define full-frame (make-frame (make-vect -0.5 -0.5)
			       (make-vect 1.5 0)
			       (make-vect 0 1.5)))



(define (make-normalized-vect-from-point point)
  (make-vect (/ (point-h point) data-width)
	     (/ (point-v point) data-height)))

(define (make-triangle-painter triangle)
  (let ((corners (delaunay-corners triangle)))
    (let ((corner1 (car corners))
	  (corner2 (cadr corners))
	  (corner3 (caddr corners)))
      (segments->painter
       (list
	(make-segment (make-normalized-vect-from-point corner1)
		      (make-normalized-vect-from-point corner2))
	(make-segment (make-normalized-vect-from-point corner2)
		      (make-normalized-vect-from-point corner3))
	(make-segment (make-normalized-vect-from-point corner3)
		      (make-normalized-vect-from-point corner1)))))))

(define (draw-triangle triangle)
  ((make-triangle-painter triangle) full-frame))

(define (draw-mesh mesh)
  (define (helper mesh)
    (if (null? mesh)
	#f
	(begin
	  (draw-triangle (car mesh))
	  (helper (cdr mesh)))))
  (cs)
  (ht)
  (helper (cdr mesh)))
      
;-------------------------

;;helpers

(define (accumulate combiner initial stuff)
  (if (null? stuff)
      initial
      (combiner (car stuff) (accumulate combiner initial (cdr stuff)))))


