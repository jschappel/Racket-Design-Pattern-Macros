# Design Pattern Macros 
This library contains a set of racket macros that allow you to implement Object Oriented Design Patterns in Racket. The following patterns are supplied:
1. Builder
1. Specialized Builder
2. Adapter
3. Factory


## Builder
The builder macro has the following structure

```
(builder <builder-name> (<struct-name (<values>)>))
- Where values is either a identifier or [identifier value]
- The macro creates fields for each of the struct's fields that take the form
    <builder-name>.add-<field-name>
- The macro also generates a build function that has the form 
    <builer-name>.to-<struct-name>
```
```scheme
(require Macros)

(builder point-builder (point ([x : 10]
                               [y : 20]
                               z)))

(point-builder.set-z 10) ;; after this line point-builder is reset to its default implementation
(define pt (point-builder.to-point 10) ;; creates a point: (point 10 20 10)
```

## Specialize Builder
```scheme
(require Macros)

(keyword-builder point (x y [z 10]))
(make-point 10 20)          ;; makes a point: (point 10 20 10)
(make-point 10 20 #:z 100)  ;; makes a point: (point 10 20 100)
```


## Factory
```scheme
(require Macros)

;; define functions for the factory cases
(define (josh data) 'josh)
(define (marco data) 'marco)
(define (sena data) 'sena)
(define (sach data) 'sach)
(define (other data) 'other)

;; create the factory using the factory macro
(factory fsa
           [(number? 1 symbol?) <- other]
           [(_ 2 3) <- josh]
           [(1 _ 1) <- marco]
           [(_ _ _) <- sach]
           [(_ _) <- sena])

;; call the factory
(fsa-factory '((1 2 3))) ;; returns 'josh
```



## Adapter
```scheme
(require Macros)
;; define functions to map each case on...
(define (fsa-special-rule-to-string rules)
    (foldl (lambda (v accum) (string-append accum (if (number? v)
                                                      (number->string v)
                                                      (symbol->string v))))
           "SPECIAL "
           rules))
  
(define (fsa-rule-to-string rules)
    (foldl (lambda (v accum) (string-append accum (symbol->string v)))
        ""
        rules))

(define (pda-rule-to-string rules)
    (foldl (lambda (v accum) (string-append accum (symbol->string v)))
            ""
            (flatten rules)))

(define (tm-rule-to-string rules)
    (foldl (lambda (v accum) (string-append accum (symbol->string v)))
            ""
            (flatten rules)))


;; create the adapter
(adapter graph
           [(_ number? _) <- fsa-special-rule-to-string]
           [(_ _ _) <- fsa-rule-to-string]
           [((_ _ _) (_ _)) <- pda-rule-to-string]
           [((_ _) (_ _)) <- tm-rule-to-string])


;; call the adapter
(graph-adapter '((A a B) (B a B))) ;; returns: '("AaB" "BaB") 
```
