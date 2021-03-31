#lang racket

; Basic Hash Table Examples
(define ht (let ((my-hash-table (make-immutable-hash '((a . 10) (b . 5) (c . 2)))))
  (hash-set my-hash-table 'd -2)))

(hash-ref ht 'c)

(hash-remove ht 'a)

; make-empty-env creates an environment given an ID of the enclosing environment.
; Returns a pair containing the following:
; 1st element: empty hash table
; 2nd environment: ID of enclosing environment
(define (make-empty-env enclosing)
  (cons (make-immutable-hash) enclosing))

; built-in functions in our interpreter
(define built-in-names '(define lambda set! + - / * remainder modulo and or))

(define (get-global-env)
  (let ((global-frame (make-immutable-hash (map (lambda (x) (cons x 'BUILT-IN-FUNCTION)) built-in-names))))
    (cons global-frame 0)))

; An environment map is a mapping from environment names (IDs) to environments

(define (get-global-env-map)
  (make-immutable-hash (list (cons 1 (get-global-env)))))

; returns value of key inside an environment
(define (env-lookup env-map env-id key)
  (if (zero? env-id)
      (error (string-append "Wasn't able to find key "
                            (symbol->string key)
                            " in any environment."))
      (let* ((env (hash-ref env-map env-id))
             (value (hash-ref (car env) key 'NOT-FOUND)))
        (if (equal? value 'NOT-FOUND)
            (env-lookup env-map (cdr env) key)
            value))))

(define (env-add-key-value env-map env-id key value)
  (let* ((env (hash-ref env-map env-id))
         (frame (car env))
         (new-frame (hash-set frame key value))
         (new-env (cons new-frame (cdr env))))
    (hash-set env-map env-id new-env)))

(define (return-env env-id env-map)
  (car (hash-ref env-map env-id)))

; Delete the environment with the associated env-id
(define (env-remove env-map env-id)
  (hash-remove env-map env-id))

; Example of adding a new environment to the env-map
(hash-set (get-global-env-map) 2 (make-empty-env 1))

; Usage Example
; (my-eval '(let ((x 10)) (+ x x)) 1 (get-global-env-map))
(define (my-eval expr env-id env-map)
  (cond ((number? expr) (cons expr env-map))
        ((string? expr) (cons expr env-map))
        ((boolean? expr) (cons expr env-map))
        ((null? expr) (cons expr env-map))
        ((not (list? expr)) (when (hash-has-key? (car (hash-ref env-map env-id)) expr) ; evaluates symbol if defined in env
            (cons (hash-ref (car (hash-ref env-map env-id)) expr) env-map))
            )
        ((eq? (env-lookup env-map env-id (first expr)) 'BUILT-IN-FUNCTION)
            (sym-lookup expr env-id env-map)
            )
        ))

; checks function name
(define (sym-lookup expr env-id env-map)
  (cond ((eq? (first expr) 'define) (my-define expr env-id env-map))
        ((eq? (first expr) '+) (+ (car (my-eval (second expr) env-id env-map)) (car (my-eval (third expr) env-id env-map))))
        ((eq? (first expr) '-) (- (car (my-eval (second expr) env-id env-map)) (car (my-eval (third expr) env-id env-map))))
        ((eq? (first expr) '*) (* (car (my-eval (second expr) env-id env-map)) (car (my-eval (third expr) env-id env-map))))
        ((eq? (first expr) '/) (/ (car (my-eval (second expr) env-id env-map)) (car (my-eval (third expr) env-id env-map))))
        ((eq? (first expr) 'remainder) (remainder (car (my-eval (second expr) env-id env-map)) (car (my-eval (third expr) env-id env-map))))
        ((eq? (first expr) 'modulo) (modulo (car (my-eval (second expr) env-id env-map)) (car (my-eval (third expr) env-id env-map))))
        ((eq? (first expr) 'and) (and (car (my-eval (second expr) env-id env-map)) (car (my-eval (third expr) env-id env-map))))
        ((eq? (first expr) 'or) (or (car (my-eval (second expr) env-id env-map)) (car (my-eval (third expr) env-id env-map))))))

; define - place expression in environment
(define (my-define expr env-id env-map)
  (env-add-key-value env-map env-id (second expr) (third expr)))
  