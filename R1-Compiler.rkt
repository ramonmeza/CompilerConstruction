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
;; Problem with let: produces unnecessary assign statement...
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

    ;; Slightly incorrect...
    ;; Does recursion then sets variable to returned variable
    [`(let ([,x ,v]) ,body)
     (define-values (flat-exp flat-assigns flat-all-vars) (flatten-R1 v))
     (define new-assign `(assign ,x ,flat-exp))
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
(define (select-instructions p)
  (define (select-instruction prog inst)
    (define exp (car prog))
    (match exp
      ;; Read
      [`(assign ,LHS (read))
       (define asm (list `(callq read_int) `(movq (reg rax) (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]

      ;; Integer
      [`(assign ,LHS ,(? integer? v))
       (define asm (list `(movq (int ,v) (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]

      ;; Symbols
      [`(assign ,LHS ,(? symbol? v))
       (define asm (list `(movq (var ,v) (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]

      ;; Addition
      [(or `(assign ,LHS (+ ,(? integer? v) ,LHS))
           `(assign ,LHS (+ ,LHS ,(? integer? v))))
       (define asm (list `(addq (int ,v) (var ,LHS))))
       (select-instruction (cdr prog) (append inst asm))]
      
      [(or `(assign ,LHS (+ ,(? symbol? v) ,LHS))
           `(assign ,LHS (+ ,LHS ,(? symbol? v))))
       (define asm (list `(addq (var ,v) (var ,LHS))))
       (select-instruction (cdr prog) (append inst asm))]
      
      [`(assign ,LHS (+ ,(? integer? v1) ,(? integer? v2)))
       (define asm (list `(movq (int ,v1) (var ,LHS)) `(addq (int ,v2) (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]

      [`(assign ,LHS (+ ,(? symbol? v1) ,(? symbol? v2)))
       (define asm (list `(movq (var ,v1) (var ,LHS)) `(addq (var ,v2) (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]

      [`(assign ,LHS (+ ,(? integer? v1) ,(? symbol? v2)))
       (define asm (list `(movq (int ,v1) (var ,LHS)) `(addq (var ,v2) (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]

      [`(assign ,LHS (+ ,(? symbol? v1) ,(? integer? v2)))
       (define asm (list `(movq (var ,v1) (var ,LHS)) `(addq (int ,v2) (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]  
         
      ;; Negation
      [`(assign ,LHS (- ,LHS))
       (define asm (list `(negq (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]
      
      [`(assign ,LHS (- ,(? integer? v)))
       (define asm (list `(movq (int ,v) (var ,LHS)) `(negq (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]
      
      [`(assign ,LHS (- ,(? symbol? v)))
       (define asm (list `(movq (var ,v) (var ,LHS)) `(negq (var ,LHS))))
       (select-instruction  (cdr prog) (append inst asm))]

      ;; Match return
      [`(return ,v)
       (define asm (list `(movq (var ,v) (reg rax))))
       (append inst asm)]
      ))
  (define vars (cadr p))
  (define assigns (select-instruction (cddr p) `()))
  (list* `program vars assigns))

;;;
;;; Assign Homes
;;;
(define (assign-homes p)
  (match-define `(program ,vars ,asm ...) p)

  ;; Put variables into a key-value map with value being their offset on the stack
  (define (create-homes lst offset)
    (cond
      [(empty? lst) (Key-Value-Empty)]
      [else (Key-Value-Node (car lst) offset (create-homes (cdr lst) (- offset 8)))])
    )

  ;; Save the key-value map for later use
  (define homes (create-homes vars -8))

  ;; Replace all instances of vars
  (define (fix-homes lst asm)
    (cond
      [(empty? lst) asm]
      [else
       (match (car lst)
         [`(,inst (var ,v1) (reg ,v2))
          (define new-inst (list `(,inst (deref rbp ,(Key-Value-Lookup homes v1)) (reg ,v2))))
          (fix-homes (cdr lst) (append asm new-inst))]
         
         [`(,inst (var ,v1) (var ,v2))
          (define new-inst (list `(,inst (deref rbp ,(Key-Value-Lookup homes v1)) (deref rbp ,(Key-Value-Lookup homes v2)))))
          (fix-homes (cdr lst) (append asm new-inst))]
         
         [`(,inst (int ,v1) (var ,v2))
          (define new-inst (list `(,inst (int ,v1) (deref rbp ,(Key-Value-Lookup homes v2)))))
          (fix-homes (cdr lst) (append asm new-inst))]
         
         [`(,inst (var ,v1) (int ,v2))
          (define new-inst (list `(,inst (deref rbp ,(Key-Value-Lookup homes v1)) (int ,v2))))
          (fix-homes (cdr lst) (append asm new-inst))]
         
         [`(,inst (var ,v))
          (define new-inst (list `(,inst (deref rbp ,(Key-Value-Lookup homes v)))))
          (fix-homes (cdr lst) (append asm new-inst))]

         [`(callq ,v)
          (define new-inst (list `(callq ,v)))
          (fix-homes (cdr lst) (append asm new-inst))]
         )]
      ))

  (list* `program (* (length vars) 8) (fix-homes asm `())))

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
         (Key-Value-Lookup Rest PassedKey))]
         ;(Key-Value-Lookup PassedKey Rest))]
    ))

;;;
;;; Test Modules
;;;

;; Program to test
(define program
  `(program
    (let ([x (+ (- 10) 11)]) (+ x 41))
    )
  )

(module+ test
  (newline)
  (print "Given program...")
  (newline)
  program)

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
  (flatten-R1 (uniquify program)))

;; Test select-instructions
(module+ test
  (newline)
  (print "Testing select-instructions...")
  (newline)
  (select-instructions (flatten-R1 (uniquify program))))

;; Test assign-homes
(module+ test
  (newline)
  (print "Testing assign-homes...")
  (newline)
  (assign-homes (select-instructions (flatten-R1 (uniquify program)))))