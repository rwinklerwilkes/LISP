(setq *print-circle* t)
(defstruct define-form var val)
(defstruct const-expr val)
(defstruct var-expr name)
(defstruct if-expr test if-part else-part)
(defstruct let-expr vars vals body)
(defstruct letrec-expr vars vals body)
(defstruct proc-call-expr proc args)
(defstruct assign-expr var val)
(defstruct lambda-expr formals body)
(defstruct call-cc-expr func)
(defstruct display-expr arg)
(defstruct newline-expr)
(defstruct begin-expr expr-list)
(defstruct prim-proc name arity func)
(defstruct closure formals body env)
(defstruct begin exprs)
(defstruct cont k)
(defstruct if-cont if-part else-part env k)
(defstruct display-cont k)
(defstruct assign-cont assoc k)
(defstruct let-cont vars body env k)
(defstruct letrec-cont vars body env k)
(defstruct map-eval-cont-1 cdr env k)
(defstruct map-eval-cont-2 y k)
(defstruct call-cc-cont k)
(defstruct proc-call-cont-1 args env k)
(defstruct proc-call-cont-2 x k)
(defstruct list-cont cdr env k)
(defstruct ident-cont)

(defun parse-form (form)
  (typecase form
	    (cons (case (car form)
			(define 
			  (make-define-form 
			   :var (parse-expr (cadr form))
			   :val (parse-expr (caddr form))))
			(otherwise
			 (parse-expr form))))
	    (otherwise (parse-expr form))))

(defun parse-expr (expr)
  (etypecase expr
	     (number (make-const-expr :val expr))
	     (symbol (case expr
			   (true (make-const-expr :val 'true))
			   (false (make-const-expr :val 'false))
			   (otherwise (make-var-expr :name expr))))
	     (cons (case (car expr)
			 (if (make-if-expr 
			      :test (parse-expr (cadr expr))
			      :if-part (parse-expr (caddr expr))
			      :else-part (parse-expr (cadddr expr))))
			 (newline (make-newline-expr))
			 (display (make-display-expr :arg (parse-expr (cadr expr))))
			 (begin (make-begin-expr :expr-list (mapcar #'parse-expr (cdr expr))))
			 (set! (make-assign-expr
				:var (parse-expr (cadr expr))
				:val (parse-expr (caddr expr))))
			 (let (make-let-expr
			       :vars (mapcar #'car (cadr expr))
			       :vals (mapcar (lambda (x) (parse-expr (cadr x))) (cadr expr))
			       :body (parse-expr (caddr expr))))
			(letrec (make-letrec-expr
			       :vars (mapcar #'car (cadr expr))
			       :vals (mapcar (lambda (x) (parse-expr (cadr x))) (cadr expr))
			       :body (parse-expr (caddr expr))))
			 (lambda (make-lambda-expr
				  :formals (cadr expr)
				  :body (parse-expr (caddr expr))))
			 (call/cc (make-call-cc-expr :func (parse-expr (cadr expr))))
			 (otherwise
			  (make-proc-call-expr 
			   :proc (parse-expr (car expr))
			   :args (mapcar #'parse-expr (cdr expr))))))))

(defun eval-form (form env)
  (typecase form
	    (define-form 
	      (let* ((var (var-expr-name (define-form-var form)))
		     (val (eval-expr-cps (define-form-val form) env (make-ident-cont)))
		     (loc (assoc var env)))
		(if (null loc)
		    (progn 
		      (let ((cdr-of-env (cdr env)))
			(setf (cdr env) (cons (list var val) cdr-of-env))))
		  (setf (cadr loc) val))))
	    (otherwise 
	     (let ((val (eval-expr-cps form env (make-ident-cont))))
	       (typecase val 
			 (closure (format *standard-output* "*closure*~%"))
			 (cont (format *standard-output* "*continuation*~%"))
			 (null (format *standard-output* "()~%"))
			 (otherwise (format *standard-output* "~a~%" val)))))))

(defun apply-cont (k x)
  (etypecase k
		(ident-cont x)
	     (if-cont (if (eq x 'true)
			  (eval-expr-cps (if-cont-if-part k) (if-cont-env k) (if-cont-k k))
			(eval-expr-cps (if-cont-else-part k) (if-cont-env k) (if-cont-k k))))
		(display-cont (apply-cont (display-cont-k k) (progn (format *standard-output* "~a " x) x)))
		(assign-cont (setf (cadr (assign-cont-assoc k)) x) (apply-cont (assign-cont-k k) x))
		(let-cont (let ((bindings (mapcar #'list (let-cont-vars k) x)))
			(eval-expr-cps (let-cont-body k) (append bindings (let-cont-env k)) (let-cont-k k))))
		(letrec-cont (let* ((bindings (mapcar #'list (letrec-cont-vars k) x))
							(bpenv (append bindings (letrec-cont-env k))))
			(backpatch bpenv bpenv (length (letrec-cont-vars k)))
			(eval-expr-cps (letrec-cont-body k) bpenv (letrec-cont-k k))))
		(map-eval-cont-1 (map-eval-expr-cps (map-eval-cont-1-cdr k) (map-eval-cont-1-env k) (make-map-eval-cont-2 :y x :k (map-eval-cont-1-k k))))
		(map-eval-cont-2 (apply-cont (map-eval-cont-2-k k) (cons (map-eval-cont-2-y k) x)))
		(call-cc-cont (apply-proc-cps x (list (make-cont :k (call-cc-cont-k k))) (call-cc-cont-k k)))
		(proc-call-cont-1 (map-eval-expr-cps (proc-call-cont-1-args k) (proc-call-cont-1-env k) (make-proc-call-cont-2 :x x :k (proc-call-cont-1-k k))))
		(proc-call-cont-2 (apply-proc-cps (proc-call-cont-2-x k) x (proc-call-cont-2-k k)))
		(list-cont (eval-expr-list-cps (list-cont-cdr k) (list-cont-env k) (list-cont-k k)))))
		
(defun eval-expr-cps (expr env k)
  (etypecase expr
	     (const-expr (apply-cont k (const-expr-val expr)))
	     (var-expr 
	      (apply-cont k 
		       (let*
			   ((var (var-expr-name expr))
			    (loc (assoc var env)))
			 (if (null loc)
			     (error (format nil "Undefined variable: ~a" var))
			   (cadr loc)))))
	     (if-expr (eval-expr-cps (if-expr-test expr) env 
				     (make-if-cont :if-part (if-expr-if-part expr)
						   :else-part (if-expr-else-part expr)
						   :env env
						   :k k)))
	     (newline-expr (progn (format *standard-output* "~%") (apply-cont k 0)))
		 (display-expr (eval-expr-cps (display-expr-arg expr) 
					  env
						(make-display-cont :k k)))
	     (begin-expr (eval-expr-list-cps (begin-expr-expr-list expr) env k))
		 (assign-expr
	      (let* ((var (var-expr-name (assign-expr-var expr)))
		     (loc (assoc var env)))
		(if (null loc)
		    (error (format nil "Undefined variable: ~a" var))
		  (eval-expr-cps (assign-expr-val expr) 
			     env 
			     (make-assign-cont :assoc loc :k k)))))
		(let-expr
	      (let ((vars (let-expr-vars expr)))
			(map-eval-expr-cps (let-expr-vals expr) 
				   env
				   (make-let-cont :vars vars :body (let-expr-body expr) :env env :k k))))
		(letrec-expr
	      (let* ((vars (letrec-expr-vars expr)))
		     (map-eval-expr-cps (letrec-expr-vals expr)
				env
				(make-letrec-cont :vars vars :body (letrec-expr-body expr) :env env :k k))))
	     (lambda-expr
	      (apply-cont k (make-closure :formals (lambda-expr-formals expr) 
				       :body (lambda-expr-body expr) 
				       :env env)))
		(call-cc-expr 
	      (eval-expr-cps (call-cc-expr-func expr) env (make-call-cc-cont :k k)))
	     (proc-call-expr
	      (eval-expr-cps (proc-call-expr-proc expr) 
			 env
			 (make-proc-call-cont-1 :env env :args (proc-call-expr-args expr) :k k)))))

(defun backpatch (env val n)
  (if (not (zerop n))
      (let ((cadar-env (cadar env)))
	(typecase cadar-env
		  (closure
		   (setf (closure-env cadar-env) val)))
	(backpatch (cdr env) val (- n 1)))))
			 
(defun eval-expr-list-cps (lst env k)
  (cond ((null lst) (apply-cont k nil))
	((null (cdr lst)) (eval-expr-cps (car lst) env k))
	(t (eval-expr-cps (car lst) env (make-list-cont :cdr (cdr lst) :env env :k k)))))

;;This will have to be treated like fibonacci
(defun map-eval-expr-cps (lst env k)
  (if (null lst)
      (apply-cont k nil)
    (eval-expr-cps (car lst) 
		   env 
		   (make-map-eval-cont-1 :cdr (cdr lst) :env env :k k))))

(defun apply-proc-cps (proc args k)
  (etypecase proc
	     (prim-proc (apply-prim-proc-cps proc args k))
	     (cont (apply-cont-cc proc args))
	     (closure (apply-closure-cps proc args k))))

(defun apply-cont-cc (cont args)
	(apply-cont (cont-k cont) (car args)))
		 
(defun apply-prim-proc-cps (proc args k)
  (let ((arity (prim-proc-arity proc)))
    (if (or (eq arity 'v) (= arity (length args)))
	(apply-cont k (apply (prim-proc-func proc) args))
      (error (format nil 
		     "Wrong number of arguments for primitive procedure ~a" 
		     (prim-proc-name proc))))))

(defun apply-closure-cps (proc args k)
  (let ((formals (closure-formals proc))
	(body (closure-body proc)))
    (if (= (length formals) (length args))
	(let* ((bindings (mapcar #'list formals args))
	       (env (closure-env proc)))
	  (eval-expr-cps body (append bindings env) k))
      (error (format nil
		     "Wrong number of arguments for procedure (lambda ~a ~a)"
		     formals
		     body)))))

(setf initial-env 
      (list
       (list nil nil)
       (list '+ (make-prim-proc :name '+ :arity 2 :func #'+))
       (list '- (make-prim-proc :name '- :arity 2 :func #'-))
       (list '* (make-prim-proc :name '* :arity 2 :func #'*))
       (list '/ (make-prim-proc :name '/ :arity 2 :func #'/))
       (list 'remainder (make-prim-proc :name 'remainder :arity 2 :func #'mod))
       (list 'quotient (make-prim-proc :name 'quotient :arity 2 :func (lambda (x y) (truncate (/ x y)))))
       (list 'list (make-prim-proc :name 'list :arity 'v :func #'list))
       (list 'cons (make-prim-proc :name 'cons :arity 2 :func #'cons))
       (list 'car (make-prim-proc :name 'car :arity 1 :func #'car))
       (list 'cdr (make-prim-proc :name 'cdr :arity 1 :func #'cdr))
       (list 'null? (make-prim-proc :name 'null? :arity 'v :func (lambda (x) (if (null x) 'true 'false))))
       (list '= (make-prim-proc :name '= :arity 2 :func (lambda (x y) (if (= x y) 'true 'false))))
       (list '<= (make-prim-proc :name '= :arity 2 :func (lambda (x y) (if (<= x y) 'true 'false))))
       (list '>= (make-prim-proc :name '= :arity 2 :func (lambda (x y) (if (>= x y) 'true 'false))))
       (list '< (make-prim-proc :name '= :arity 2 :func (lambda (x y) (if (< x y) 'true 'false))))
       (list '> (make-prim-proc :name '= :arity 2 :func (lambda (x y) (if (> x y) 'true 'false))))
       (list 'not (make-prim-proc :name 'not :arity 1 :func (lambda (x) (if (eq x 'false) 'true 'false))))))

(defun repl (&optional (env initial-env))
  (format *standard-output* "> ")
  (handler-case (let ((form (read *standard-input* nil)))
		  (if (not (null form))
		      (progn 
			(eval-form (parse-form form) env)			
			(repl env))))
		(error (condition)
			 (format *standard-output* "~a~%" condition)
			 (repl env))))

(repl)