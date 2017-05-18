#lang racket/base
(require racket/match
         racket/list)

;;;
;;; Uniquify
;;;
(define (uniquify p)
  (match p
    [`(program ,exp) `(program ,(uniquify-exp (Key-Value-Empty) exp))]
    ))

(define (uniquify-exp alst exp)
  (define (recur e) (uniquify-exp alst e))
  (match exp
    [(? symbol?) (Key-Value-Lookup alst exp)]
    [(? integer?) exp]
    [`(read) `(read)]
    [`(let ([,name ,val]) ,body)
     (define new-alst (Key-Value-Node name (gensym name) alst))
     `(let ([,(Key-Value-Node-Value new-alst) ,(recur val)]) ,(uniquify-exp new-alst body))]
    [`(- ,(app recur v)) `(- ,v)]
    [`(+ ,(app recur v1) ,(app recur v2)) `(+ ,v1 ,v2)]
    ))


;;;
;;; Flatten
;;;
(define (flatten-R1 exp)
  (match exp
    [(? symbol?) (values exp `() `())]
    [(? integer?) (values exp `() `())]

    [`(read)
     (define new-var (gensym `tmp))
     (define new-assign `(assign ,new-var (read)))
     (values new-var (list new-assign) (list new-var))]
    
    [`(- ,v)
     (define-values (flat-exp flat-assigns flat-all-vars) (flatten-R1 v))
     (define new-var (gensym `tmp))
     (define new-assign `(assign ,new-var (- ,flat-exp)))
     (values new-var (append flat-assigns (list new-assign)) (append flat-all-vars (list new-var)))]

    [`(+ ,v1 ,v2)
     (define-values (LHS-exp LHS-assigns LHS-vars) (flatten-R1 v1))
     (define-values (RHS-exp RHS-assigns RHS-vars) (flatten-R1 v2))
     (define new-var (gensym `tmp))
     (define new-assign `(assign ,new-var (+ ,LHS-exp ,RHS-exp)))
     (values new-var (append LHS-assigns RHS-assigns (list new-assign)) (append LHS-vars RHS-vars (list new-var)))]  

    [`(let ([,x ,v]) ,body)
     (define-values (flat-exp flat-assigns flat-all-vars) (flatten-R1 v))
     (define new-assign `(assign ,x (,flat-exp)))
     (define-values (body-flat-exp body-flat-assigns body-flat-all-vars) (flatten-R1 body))
     (values body-flat-exp (append flat-assigns (list new-assign) body-flat-assigns) (append flat-all-vars body-flat-all-vars (list x)))] 
    
    [`(program ,body)
     (define-values (flat-exp flat-assigns flat-all-vars) (flatten-R1 body))
     (define assigns flat-assigns)
     (list* `program flat-all-vars (append assigns `((return ,flat-exp))))] 
    ))

;;;
;;; Select Instructions
;;;
(define (select-instructions exp)
  (match exp
    [`(program ,vars ,assigns ,return)
     `(program ,vars)]
    ))

;;;
;;; R1 Interpreter
;;;
(define (interp-R1 p)
  (match p
    [`(program ,exp) (interp-R1-exp (Key-Value-Empty) exp)]
    ))

(define (interp-R1-exp env exp)
  (define (recur e) (interp-R1-exp env e))
  (match exp
    [(? symbol?) (Key-Value-Lookup env exp)]
    [`(let ([,name ,(app recur value)]) ,body)
     (define new-env (Key-Value-Node name value env))
     (interp-R1-exp new-env body)]
    [(? fixnum?) exp]
    [`(read)
     (let ([r (read)])
       (cond [(fixnum? r) r]
             [else (error `interp-R1-exp "expected an integer" r)]))]
    [`(- ,(app recur v))
     (- 0 v)]
    [`(+ ,(app recur v1) ,(app recur v2))
     (+ v1 v2)]
    ))

;;;
;;; Helper Functions & Misc.
;;;
;; Key-Value Data Structure
(struct Key-Value-Empty () #:transparent)
(struct Key-Value-Node (Key Value Rest) #:transparent)

;; Lookup function for Key-Value Data Structure
;; Returns Value of PassedKey found in PassedMap
;; Errors if PassedKey not found in PassedMap
(define (Key-Value-Lookup PassedMap PassedKey)
  (match PassedMap
    [(Key-Value-Empty) (error `Key-Value-Lookup "Key: ~e was not found" PassedKey)]
    [(Key-Value-Node Key Value Rest)
     (if (eq? PassedKey Key) Value
         (Key-Value-Lookup PassedKey Rest))]
    ))

;;;
;;; Test Modules
;;;

;; Program to test
(define program
  `(program
    (let ([x (+ (- (read)) (- 5))]) (+ x 2))
    )
  )

;; Test uniquify
(module+ test
  (newline)
  (print "Testing uniquify...")
  (newline)
  (uniquify program))

;; Test flatten-R1
(module+ test
  (newline)
  (print "Testing flatten-R1...")
  (newline)
  (flatten-R1 program))

;; Test select instructions
