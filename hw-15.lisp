;;Add-list, add-genlist, add-list-struct, and add-genlist-struct
;;are all helper functions so that #'identity and (make-ident-cont)
;;don't need to be passed as parameters
(defun add-list (l)
  (add-l l #'identity))

(defun add-l (l k)
  (cond ((null l) (funcall k 0))
	((integerp (car l)) 
	 (add-l (cdr l) 
		(lambda (x) (funcall k (+ (car l) x)))))
	(t (add-l (cdr l) k))))

(defun add-genlist (l)
  (add-genl l #'identity))

(defun add-genl (l k)
  (cond ((null l) (funcall k 0))
	((integerp (car l))
	 (add-genl (cdr l)
		      (lambda (x) (funcall k (+ (car l) x)))))
	((listp (car l)) 
	 (add-genl (cdr l) 
		      (lambda (x) 
			(funcall k 
				 (add-genl (car l) 
					      (lambda (y) (+ x y)))))))
	(t (add-genl (cdr l) k))))

(defstruct ident-cont)
(defstruct plus-cont x pk)
(defstruct genl1-cont carl pk)
(defun apply-cont (k x)
  (etypecase k
	     (ident-cont x)
	     (plus-cont (apply-cont (plus-cont-pk k) 
				    (+ x (plus-cont-x k))))
	     (genl1-cont (add-genls (genl1-cont-carl k)
				    (make-plus-cont :x x
						    :pk (genl1-cont-pk k))))))
			 
(defun add-list-struct (l)
  (add-ls l (make-ident-cont)))

(defun add-ls (l k)
  (cond ((null l) (apply-cont k 0))
	((integerp (car l))
	 (add-ls (cdr l) 
		      (make-plus-cont :x (car l)
				      :pk k)))
	(t (add-ls (cdr l) k))))

(defun add-genlist-struct (l)
  (add-genls l (make-ident-cont)))

(defun add-genls (l k)
  (cond ((null l) (apply-cont k 0))
	((integerp (car l)) 
	 (add-genls (cdr l) 
		    (make-plus-cont :x (car l) :pk k)))
	((listp (car l))
	 (add-genls (cdr l) (make-genl1-cont :carl (car l) :pk k)))
	(t (add-genls (cdr l) k))))