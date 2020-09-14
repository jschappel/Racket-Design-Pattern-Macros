#lang racket
(require (for-syntax racket/syntax))
(provide builder)
;; helper functions for the builder macro
(begin-for-syntax
  ;; creates the name for the builder struct
  (define (create-builder-name name string)
    (with-syntax ([new-name (format-id name (string-append "~a-" string) name)])
      #'new-name))
  ;; creates the equivlent builder struct for the given macro syntax
  (define (builder-creator name fields)
    (with-syntax ([b-name (create-builder-name name "builder-s")]
                  [s-name name]
                  [b-fields (for/list [(field  (syntax->list fields))]
                              (syntax-case field ()
                                [(n : _) #`[n #:mutable]]
                                [n #`[n #:mutable]]
                                [else #'(error "invalid field syntax for struct")]))]
                  [s-fields (for/list [(field  (syntax->list fields))]
                              (syntax-case field ()
                                [(n : _) #`n]
                                [n #`n]
                                [else #'(error "invalid field syntax for struct")]))])
      #`(struct b-name b-fields)))
  ;; creates the equivlent struct for the given macro syntax
  (define (struct-creator name fields)
    (with-syntax ([s-name name]
                  [s-fields (for/list [(field  (syntax->list fields))]
                              (syntax-case field ()
                                [(n : _) #`n]
                                [n #`n]
                                [else #'(error "invalid field syntax for struct")]))])
      #'(struct s-name s-fields #:transparent)))
  ;; creates the function names for each of the structs fields
  (define (make-func-names fields)
    (for/list ([field (syntax->list fields)])
      (syntax-case field ()
        [(n : _) (with-syntax ([func-name (format-id #'n "add-~a" #'n)])
                   #'func-name)]
        [n (with-syntax ([func-name (format-id #`n "add-~a" #`n)])
             #'func-name)]
        [else #'(error "invalid field syntax for struct")]))))
    
;; builder :: syntax ->syntax
;; Purpose: creates the control function for the passing in struct
(define-syntax (builder stx)
  (syntax-case stx ()
    [(_ builder-name (struct-name (args ...)))
     (with-syntax ([builer-struct (builder-creator #'struct-name #'(args ...))]
                   [struct-struct (struct-creator #'struct-name #'(args ...))]
                   [c-name (create-builder-name #'struct-name "control")]
                   [b-name (create-builder-name #'struct-name "builder")]
                   [temp-name (create-builder-name #'struct-name "builder-s")]
                   [create-name (format-id #'builder-name "~a.to-~a" #'builder-name #'struct-name)]
                   [f-names (make-func-names #'(args ...))])
       #`(begin
           struct-struct
           ;; if the cournter is not made then create it
           (define (counter-macro)
             (let ([n 0])
               (set! n (add1 n))
               n))
           (define (b-name)
             builer-struct
             (define temp (temp-name #,@(for/list [(field  (syntax->list #'(args ...)))]
                                          (syntax-case field ()
                                            [(_ : val) #'val]
                                            [_ #' 'NONE]
                                            [else "Invalid syntax for field"]))))
             ;; create the builder funcs
             #,@(for/list [(field  (syntax->list #'(args ...)))]
                  (syntax-case field ()
                    [(n : _) (with-syntax ([func-name (format-id #'n "add-~a" #'n)]
                                           [set-id (format-id #'n "set-~a-~a!" #'temp-name #'n)])
                               #`(define (func-name val) (set-id temp val)))]
                    [n (with-syntax* ([func-name (format-id #'n "add-~a" #'n)]
                                      [set-id (format-id #'n "set-~a-~a!" #'temp-name #'n)])
                         #`(define (func-name val) (set-id temp val)))]
                    [else #'(error "invalid field syntax for struct")]))
                  
             ;; convert builder to struct
             (define (builder->struct)
               ;(with-syntax ([reset-struct (format-id #'temp-name "set~a" #'temp-name)])
               (define temp-struct (struct-name #,@(for/list [(field  (syntax->list #'(args ...)))]
                                                     (syntax-case field ()
                                                       [(n : _) (with-syntax ([id (format-id #'n "~a-~a" #'temp-name #'n)])
                                                                  #'(let ([tmp (id temp)])
                                                                      (if (eq? 'NONE tmp)
                                                                          (error (format "Field ~a has not been set" #'n))
                                                                          tmp)))]
                                                       [n (with-syntax ([id (format-id #'n "~a-~a" #'temp-name #'n)])
                                                            #'(let ([tmp (id temp)])
                                                                (if (eq? 'NONE tmp)
                                                                    (error (format "Field ~a has not been set" #'n))
                                                                    tmp)))]
                                                       [else "Invalid syntax for field"]))))
               (set! temp (temp-name #,@(for/list [(field  (syntax->list #'(args ...)))]
                                          (syntax-case field ()
                                            [(_ : val) #'val]
                                            [_ #' 'NONE]
                                            [else "Invalid syntax for field"]))))
               temp-struct)

             ;;create control function
             (define (c-name msg)
               (cond
                 #,@(for/list [(name (syntax->list #'f-names))]
                      (with-syntax ([n name])
                        #`[(eq? 'n msg) n]))
                 [(eq? 'gen-struct msg) (builder->struct)]
                 [else (error (format "invalid message. Given: ~a" msg))]))

             c-name)

           (define builder-name (b-name))


           ;; add the setters
           #,@(for/list [(field  (syntax->list #'(args ...)))]
                (syntax-case field ()
                  [(field-name : _) (with-syntax ([fn-name (format-id #'builder-name "~a.add-~a" #'builder-name #'field-name)]
                                                  [cont-name (format-id #'builder-name "add-~a" #'field-name)])
                                      #`(define (fn-name val)
                                          ((builder-name 'cont-name) val)))]
                  [field-name (with-syntax ([fn-name (format-id #'builder-name "~a.add-~a" #'builder-name #'field-name)]
                                            [cont-name (format-id #'builder-name "add-~a" #'field-name)])
                                #`(define (fn-name val)
                                    ((builder-name 'cont-name) val)))]
                  [else "Invalid syntax for field"]))
           (define (create-name)
             (builder-name 'gen-struct))))]
    [else (error "Invalid syntax for builder macro")]))


(module+ test
  (require rackunit)

  ;; --- Pre defined builder ---
  (builder pb2 (point2 ([x : 10]
                        [y : (list 1 2 3)]
                        z)))

  
  (pb2.add-z 5)
  (pb2.add-y (list 4 5 6))
  (define p2 (pb2.to-point2))

  (pb2.add-z 50) ;; pd is reset
  (define p-defualt2 (pb2 'gen-struct))

  (check-equal? (point2-x p2) 10 "10 is the default value that was set")
  (check-equal? (point2-y p2) (list 4 5 6) "'(4 5 6) is the new list that was set")
  (check-equal? (point2-z p2) 5 "5 is the value that the pb set for z")
  (check-equal? (point2-x p-defualt2) 10 "10 is the default value that was set")
  (check-equal? (point2-y p-defualt2) (list 1 2 3) "'(1 2 3) is the default list that was set")
  (check-equal? (point2-z p-defualt2) 50 "50 is the value that the pb set for z"))