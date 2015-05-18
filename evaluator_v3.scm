(define (lookup-in-frames k env)
  (if (null? env) #f
      (or (assoc k (car env))
	  (lookup-in-frames k (cdr env)))))

(define (define-variable! key val frames) ; UNDEFINED return value
  ;; only changes the local frame (ie the car of frames)
  (let ((pair (assoc key (car frames))))
    (if pair 
	(set-cdr! pair val)
        (set-car! frames (cons (cons key val) (car frames))))))

(define primitives
  `((+ . ,+) (- . ,-) (* . ,*) (/ . ,/)))

(define (meta-eval expr env)
  (cond ((number? expr) expr)
	((boolean? expr) expr)
	((string? expr) expr)
	((char? expr) expr)
	((symbol? expr) (cdr (lookup-in-frames expr env)))
	((lambda? expr) (make-procedure expr env))
	((define? expr) (define-variable! (cadr expr) (meta-eval (caddr expr) env) env))
	((application? expr) 
	 (meta-apply (meta-eval (car expr) env)
		 (map (lambda (x) (meta-eval x env)) (cdr expr))))
	(else "error")))

(define (meta-apply proc args)
  (cond ((primitive-procedure? proc) (apply proc args))
	((compound-procedure? proc)
	 (let* ((new-frame (map cons (procedure-parameters proc) args))
		(extended-env (cons new-frame (procedure-environment proc))))
	   (eval-sequence (procedure-body proc) extended-env)))
	(else "error")))

(define (eval-sequence exps env) ;; only last value is returned
  (if (null? (cdr exps))
      (meta-eval (car exps) env)
      (begin (meta-eval (car exps) env)
  	     (eval-sequence (cdr exps) env))))

(define (make-procedure exp env)
  (let ((parameters (cadr exp))
	(body (cddr exp)))
    (list 'procedure parameters body env)))

(define (application? exp) (pair? exp))

(define (compound-procedure? exp)
  (and (list? exp) (eq? 'procedure (car exp))))

(define (primitive-procedure? exp)
  (procedure? exp))
  ;; (and (list? exp) (eq? 'primitive (car exp))))

(define (lambda? exp)
  (and (list? exp) (eq? 'lambda (car exp))))

(define (define? exp)
  (and (list? exp) (eq? 'define (car exp))))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))


(define env (list '() primitives))


(define (repl)
  (displayer input-prompt)
  (let ((input (read)))
    (let ((output (meta-eval input env)))
      (displayer output-prompt)
      (user-print output)))
  (repl))

(define (displayer string)
  (newline) (display string) (newline))

(define input-prompt ";;; MC-Eval input:")
(define output-prompt ";;; MC-Eval value:")

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
