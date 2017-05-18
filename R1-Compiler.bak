#lang racket/base
(require racket/match
         racket/list)

;;;
;;; Flatten
;;;


;; HAS EXTRA PARENS
(define (flatten-R1 exp)
  (match exp
    [(? symbol?) (values exp `() `())]
    [(? integer?) (values exp `() `())]

    [`(- ,v)
     (define-values (flat-exp flat-assigns flat-vars) (flatten v))
     (define new-var (gensym `temp))
     (define new-assign `(- ,flat-exp))
     (values flat-exp 2 3)]
    
    
    ))

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
;;; R1 Interpreter
;;;

;; Make sure programs START with program keyword
(define (interp-R1 p)
  (match p
    [`(program ,exp) (interp-R1-exp (Key-Value-Empty) exp)]
    ))

;; Interpret an R1 expression
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
    (- 10)
    )
  )

;; Test uniquify
(module+ test
  (newline)
  (print "Testing uniquify...")
  (newline)
  (uniquify program))

;; Test flatten
(module+ test
  (newline)
  (print "Testing flatten...")
  (newline)
  (flatten-R1 program Key-Value-Empty))