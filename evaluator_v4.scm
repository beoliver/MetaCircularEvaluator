;;; ENVIRONMENT LOOKUP  ------------------------------------------------------------
;;; the 'environment' is a list of associative lists (frames)

(define (lookup-in-frames k env)
  ;; to allow for variables to store references to #f
  ;; we return the (var . val) pair if found else just #f
  (if (null? env)
      #f
      (or (assoc k (car env))
	  (lookup-in-frames k (cdr env)))))

;;; UPDATING ENVIRONMENT  ----------------------------------------------------------

(define (define-var! var val frames)
  ;; add (var . val) pair to head of frames, 
  ;; or update existing (var . existing-val) in head of frames
  (let ((pair (assoc var (car frames))))
    (if pair 
	(set-cdr! pair val)
        (set-car! frames (cons (cons var val) (car frames))))))

(define (set-var! var val frames)
  ;; traverses ALL frames
  ;; updates existing (var . existing-val) or "error" if no match found
  (if (null? frames)
      (display "set-var! :: unknown variable\n")
      (let ((pair (assoc var (car frames))))
	(if pair
	    (set-cdr! pair val)
	    (set-var! var val (cdr frames))))))

;;; INTERNAL REPRESENTATION OF LAMBDAS ---------------------------------------------
;;; all lambda expressions are converted into internal representations of the form
;;; ('procedure <parameters> <body> <closure specific environment>)
;;; we define 3 simple getters to make this clearer
;;; the conversion to internal representation happens during 'special-form-lambda'

(define (proc-parameters p) (cadr p))
(define (proc-body p) (caddr p))
(define (proc-environment p) (cadddr p))

;;; EVALUATION  --------------------------------------------------------------------

(define (meta-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((symbol? exp) (let ((pair (lookup-in-frames exp env)))
			 (if pair ;; a valid pair (KEY . VAL)
			     (cdr pair) ;; return VAL
			     (begin
			       (display "unknown variable: ")
			       (display exp)
			       (newline)))))
	((special-form? exp) (eval-special-form exp env))
	((application? exp)
	 (meta-apply (meta-eval (car exp) env)
		 (map (lambda (x) (meta-eval x env)) (cdr exp))))
	(else (for-each display '("meta-eval :: no match for expression - " expr "\n")))))

(define (meta-apply proc args)
  ;; both proc and args have allready been evaluated
  (cond ((primitive-procedure? proc) (apply proc args))
	((compound-procedure? proc)
	 (let* ((new-frame (map cons (proc-parameters proc) args))
		(extended-env (cons new-frame (proc-environment proc))))
	   (eval-sequence (proc-body proc) extended-env)))
	(else (for-each display `("meta-apply :: no match for expression - " ,proc " args:" ,args "\n")))))

(define (eval-sequence exps env) 
  ;; only last value is returned
  (if (null? (cdr exps))
      (meta-eval (car exps) env)
      (begin (meta-eval (car exps) env)
  	     (eval-sequence (cdr exps) env))))

;;; EVALUATION PREDICATES ----------------------------------------------------------

(define (self-evaluating? exp)
  (or (number? exp) (boolean? exp) (string? exp) (char? exp)))

(define (special-form? exp)
  (and (list? exp) (assoc (car exp) special-forms)))

(define (application? exp) (pair? exp))

(define (compound-procedure? exp)
  (and (list? exp) (eq? 'procedure (car exp))))

(define (primitive-procedure? exp) (procedure? exp))

(define (empty-return-value? exp)
  (not (or (self-evaluating? exp) (symbol? exp) (list? exp) (pair? exp) (procedure? exp))))

;;; SPECIAL FORMS ------------------------------------------------------------------

(define (eval-special-form exp env)
  ;; we use an association table to dispatch calls.
  ;; as 'eval-special-form' is called from 'meta-eval'
  ;; we know that 'exp' is indeed a special form
  ((cdr (assoc (car exp) special-forms)) exp env))

(define (special-form-lambda exp env)
  ;; create an internal representation of the lambda
  ;; we do this so that we can keep track of the the scope
  ;; of the closure
  (let ((parameters (cadr exp))
	(body (cddr exp)))
    (list 'procedure parameters body env)))

(define (special-form-define exp env)
  (define-var! (cadr exp) (meta-eval (caddr exp) env) env))

(define (special-form-set! exp env)
  (set-var! (cadr exp) (meta-eval (caddr exp) env) env))

(define (special-form-if exp env)
  (let ((evaluated-predicate (meta-eval (cadr exp) env))
	(then-branch (caddr exp))
	(else-branch (cadddr exp))) ;; will fail on (if #f 1)
    (if evaluated-predicate
	(meta-eval then-branch env)
	(meta-eval else-branch env))))

(define (special-form-let exp env)
  ;; a let expression is just a closure, thus we can re-write it
  ;; as a lambda expression
  (let* ((let-bindings (cadr exp))
	 (parameters (map car let-bindings))
	 (args (map cadr let-bindings))
	 (body (cddr exp))
	 (lambda-form (cons 'lambda (cons parameters body)))
	 (new-exp (cons lambda-form args)))
    (meta-eval new-exp env)))

(define special-forms ;; kept SPEARATE from frames
  `((lambda . ,special-form-lambda)
    (define . ,special-form-define)
    (exit . ,(lambda (a b) 'user-exit-request))
    (set! . ,special-form-set!)    
    (if  . ,special-form-if)
    (let . ,special-form-let)))

;; PRIMITIVES ---------------------------------------------------------------------

(define primitives
  ;; loaded as the first frame of the environment
  `((+ . ,+) (- . ,-) (* . ,*) (/ . ,/) (nil . nil) (id . ,(lambda (x) x))))

;; REPL ---------------------------------------------------------------------------

(define (repl)
  (display "\n?> ")
  (let ((obj (meta-eval (read) env)))
    (if (eq? obj 'user-exit-request)
	'bye
	(begin
	  (cond ((compound-procedure? obj) (display (list '<proc> (proc-parameters obj))))
		;; the compound-procedure test is needed as we don't want to display the
		;; environment of the procedure (that has a pointer to the procedure itself
		;; which results in an infinite loop of printing
		((empty-return-value? obj) (display ""))
		(else (display obj)))
	  (repl)))))

;; --------------------------------------------------------------------------------

(define env (list '() primitives))

;; --------------------------------------------------------------------------------

(repl)
