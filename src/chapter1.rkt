; -*- mode: racket -*-

; this is a translation of the Scheme presented in chapter 1 of LiSP to Racket
; and contains amended code implementing exercises.
; This code should be run in a racket/base REPL

(require "./util.rkt")
(require rnrs/mutable-pairs-6)

(define the-false-value (cons "false" "boolean"))

(define (evaluate e env)
  (if (atom? e) 
      (cond ((symbol? e) (lookup e env))
            ((or (number? e)(string? e)(char? e)(boolean? e)(vector? e))
             e )
            (else (error "Cannot evaluate" e)) )
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (not (eq? (evaluate (cadr e) env) the-false-value))
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env) ))
        ((begin)  (eprogn (cdr e) env))
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env) )) ) ) )

(define (evaluate-trace e env)
  (if (atom? e) 
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (error "Cannot evaluate" e)))
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (not (eq? (evaluate (cadr e) env) the-false-value))
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env) ))
        ((begin)  (eprogn (cdr e) env))
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else (let ([fn-name (car e)]
                    [evs (cdr e)])
                (printf "calling ~a with ~a\n" fn-name evs)
                (invoke (evaluate (car e) env)
                        (evlis (cdr e) env) ))))))

(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env) )
          (evaluate (car exps) env) )
      '() ) )

; this is the answer copied, since the author has a better implementation than me 😬
(define (evlis exps env)
  (define (evlis exps)
    (if (pair? (cdr exps))
        (cons (evaluate (car exps) env)
              (evlis (cdr exps)) )
        (list (evaluate (car exps) env)) ) )
  (if (pair? exps)
      (evlis exps)
      '()))

(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)) )
      (error "No such binding" id) ) )

(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value)
                 value )
          (update! id (cdr env) value) )
      (error "No such binding" id) ) ) 

(define env.init '())

(define (extend env variables vals)
  (cond ((pair? variables)
         (if (pair? vals)
             (cons (cons (car variables) (car vals))
                   (extend env (cdr variables) (cdr vals)) )
             (error "Too less vals") ) )
        ((null? variables)
             (if (null? vals)
                 env 
                 (error "Too much vals") ) )
        ((symbol? variables) (cons (cons variables vals) env)) ) ) 

(define (invoke fn args)
  (if (procedure? fn) 
      (fn args)
      (error "Not a function" fn) ) ) 

(define (make-function variables body env)
  (lambda (vals)
     (eprogn body (extend env variables vals)) ) )

(define env.global env.init)

(define-syntax definitial 
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (cons (cons 'name 'void) env.global))
            'name ) )
    ((definitial name value)
     (begin (set! env.global (cons (cons 'name value) env.global))
            'name ) ) ) )

(define-syntax defprimitive 
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name 
        (lambda (vals) 
          (if (= arity (length vals))
              (apply value vals)       ; The real \texttt{apply} of Scheme
              (error "Incorrect arity"
                     (list 'name vals) ) ) ) ) ) ) )

(define-syntax defpredicate 
  (syntax-rules ()
    ((defpredicate name value arity)
     (defprimitive name
       (lambda vals (or (apply value vals) the-false-value))
       arity ) ) ) )

(definitial t #t)
(definitial f the-false-value)
(definitial nil '())

(definitial x)
(definitial y)
(definitial z)
(definitial a)
(definitial b)
(definitial c)
(definitial k)
(definitial foo)
(definitial bar)
(definitial hux)
(definitial fib)
(definitial fact)
(definitial visit)
(definitial length)
(definitial primes)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defpredicate pair? pair? 1)
(defpredicate symbol? symbol? 1)
(defprimitive eq? eq? 2)           ; cf. exercice \ref{exer-predicate}
(defpredicate eq? eq? 2)           ; cf. exercice \ref{exer-predicate}
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defpredicate = = 2)
(defpredicate > > 2)
(defpredicate < < 2)               ; cf. exercice \ref{exer-predicate}\endlisp
(defprimitive * * 2)
(defpredicate <= <= 2)
(defpredicate >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)

(defprimitive call/cc 
  (lambda (f) 
    (call/cc (lambda (g) 
               (invoke 
                f (list (lambda (vals)
                          (if (= (length vals) 1)
                              (g (car vals))
                              (error "Incorrect arity" g) ) ))) )) )
  1 )   

(definitial apply 
  (lambda (vals)
    (if (>= (length vals) 2)
        (let ((f (car vals))
              (args (let flat ((args (cdr vals)))
                      (if (null? (cdr args))
                          (car args)
                          (cons (car args) (flat (cdr args))) ) )) )
        (invoke f args) )
        (error "Incorrect arity" 'apply) ) ) )

(definitial list 
  (lambda (vals) vals) )

(define (chapter1-scheme tracing)
  (define (toplevel)
    (let ((input (read)))
      (unless (eq? input "exit")
        (display (evaluate input env.global))
        (toplevel))))
  (define (toplevel-with-tracing)
    (let ((input (read)))
      (unless (eq? input "exit")
        (display (evaluate-trace input env.global))
        (toplevel))))
  (if tracing
      (toplevel-with-tracing)
      (toplevel)))

;;;  Tests provided by author, commented out for now

;; (define (scheme1)
;;   (interpreter
;;    "Scheme? "
;;    "Scheme= "
;;    #t
;;    (lambda (read print error)
;;      (set! error error)
;;      (lambda ()
;;        (print (evaluate (read) env.global)) ) ) ) )

;; (define (test-scheme1 file)
;;   (suite-test
;;    file
;;    "Scheme? "
;;    "Scheme= "
;;    #t
;;    (lambda (read check error)
;;      (set! error error)
;;      (lambda ()
;;        (check (evaluate (read) env.global)) ) )
;;    (lambda (expected obtained)
;;      (or (equal? expected obtained)
;;          (and (eq? obtained the-false-value) (eq? expected #f)) ) ) ) )
