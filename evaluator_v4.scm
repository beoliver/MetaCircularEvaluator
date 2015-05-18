;; ENVIRONMENT LOOKUP  ------------------------------------------------------------

(define (lookup-in-frames k env)
  (if (null? env) 
      #f
      (or (assoc k (car env))
	  (lookup-in-frames k (cdr env)))))

;; UPDATING ENVIRONMENT  ----------------------------------------------------------

(define (define-var! var val frames)
  (let ((pair (assoc var (car frames))))
    (if pair 
	(set-cdr! pair val)
        (set-car! frames (cons (cons var val) (car frames))))))

(define (set-var! var val frames)
  (if (null? frames)
      (begin
	(display "unknown variable")
	(newline))
      (let ((pair (assoc var (car frames))))
	(if pair
	    (set-cdr! pair val)
	    (set-var! var val (cdr frames))))))

;; EVALUATION  --------------------------------------------------------------------

(define (meta-eval exp env)
  ;; (display "meta eval")
  ;; (newline)
  ;; (display exp)
  ;; (newline)
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
	(else (display 'meta-eval-error))))

(define (meta-apply proc args)
  (cond ((primitive-procedure? proc) (apply proc args))
	((compound-procedure? proc)
	 (let* ((new-frame (map cons (procedure-parameters proc) args))
		(extended-env (cons new-frame (procedure-environment proc))))
	   (eval-sequence (procedure-body proc) extended-env)))
	(else (display 'meta-apply-error))))

(define (eval-sequence exps env) ;; only last value is returned
  (if (null? (cdr exps))
      (meta-eval (car exps) env)
      (begin (meta-eval (car exps) env)
  	     (eval-sequence (cdr exps) env))))

;; EVALUATION PREDICATES ----------------------------------------------------------

(define (self-evaluating? exp)
  (or (number? exp) (boolean? exp) (string? exp) (char? exp)))

(define (special-form? exp)
  (and (list? exp) (assoc (car exp) special-forms)))

(define (application? exp) (pair? exp))

(define (compound-procedure? exp)
  (and (list? exp) (eq? 'procedure (car exp))))

(define (primitive-procedure? exp) (procedure? exp))

(define (empty-return-value? exp)
  (not (or (self-evaluating? exp)
	   (symbol? exp)
	   (list? exp)
	   (pair? exp)
	   (procedure? exp))))

;; SPECIAL FORMS ------------------------------------------------------------------

(define (eval-special-form exp env)
  ((cdr (assoc (car exp) special-forms)) exp env))

(define (special-form-lambda exp env)
  (make-procedure exp env))

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

;; INTERNAL REPRESENTATION OF LAMBDAS ---------------------------------------------

(define (make-procedure exp env)
  (let ((parameters (cadr exp))
	(body (cddr exp)))
    (list 'procedure parameters body env)))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; PRIMITIVES ---------------------------------------------------------------------

(define primitives
  `((+ . ,+) (- . ,-) (* . ,*) (/ . ,/) (nil . nil)))

;; REPL ---------------------------------------------------------------------------

(define (repl)
  (displayer input-prompt)
  (let ((input (read)))
    (let ((output (meta-eval input env)))
      (displayer output-prompt)
      (if (eq? output 'user-exit-request)
	  "ciao..."
	  (begin
	    (user-print output)
	    (repl))))))

(define (displayer string)
  (newline) (display string) (newline))

(define input-prompt "> META-CIRCULAR INPUT... (exit) to exit")
(define output-prompt "> META-CIRCULAR VALUE:")

(define (user-print object)
  (cond ((compound-procedure? object)
	 (display (list 'compound-procedure
			(procedure-parameters object)
			(procedure-body object)
			'<procedure-env>)))
	((empty-return-value? object) (display ""))
	(else (display object))))

;; --------------------------------------------------------------------------------

(define env (list '() primitives))

;; --------------------------------------------------------------------------------

(meta-eval '(define kons 
	      (lambda (a b) 
		(lambda (p) (p a b)))) 
	   env)

(meta-eval '(define kar 
	      (lambda (p) 
		(p (lambda (a b) a)))) 
	   env)

(meta-eval '(define kdr 
	      (lambda (p) 
		(p (lambda (a b) b)))) 
	   env)

(meta-eval '(define xs (kons 1 (kons 2 (kons 3 (kons 4 nil))))) env)


(repl)
