;;; ENVIRONMENT LOOKUP  ------------------------------------------------------------
;;; the 'environment' is a list of associative lists (frames)

(define (lookup-in-frames k env)
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
  ;; updates existing (var . existing-val) or "error"
  (if (null? frames)
      (begin
	(display "unknown variable")
	(newline))
      (let ((pair (assoc var (car frames))))
	(if pair
	    (set-cdr! pair val)
	    (set-var! var val (cdr frames))))))

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
	(else (display 'meta-eval-error))))

(define (meta-apply proc args)
  ;; both proc and args have allready been evaluated
  (cond ((primitive-procedure? proc) (apply proc args))
	((compound-procedure? proc)
	 (let* ((new-frame (map cons (procedure-parameters proc) args))
		(extended-env (cons new-frame (procedure-environment proc))))
	   (eval-sequence (procedure-body proc) extended-env)))
	(else (display 'meta-apply-error))))

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
  ;; we use an association table to dispatch calls
  ;; as this is called from meta-eval, we know that
  ;; 'exp' is a special form
  ((cdr (assoc (car exp) special-forms)) exp env))

(define (special-form-lambda exp env)
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

(repl)
