# Design Pattern Macros 
This library contains a set of racket macros that allow you to implement Object Oriented Design Patterns in Racket. The following patterns are supplied:
1. Builder
1. Specialized Builder
2. Adaptor
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
Coming soon

## Adaptor
Coming soon