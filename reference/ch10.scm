(define atom? (compose not list?))

(define build
  (lambda (x y)
    (cons x (cons y '()))))

(define first car)

(define second cadr)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (cond
     ((null? (car entry)) (entry-f name))
     ((eq? name (car (car entry)))
      (car (cadr entry)))
     (else (lookup-in-entry name
                            (cons
                             (cdr (car entry))
                             (cons
                              (cdr (cadr entry)) '()))
                            entry-f)))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name (car table)
                            (lambda (name)
                              (lookup-in-table name (cdr table) table-f)))))))


(define expression-to-action
  (lambda (exp)
    (cond
     ((atom? exp) (atom-to-action exp))
     (else (list-to-action exp)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

(define list-to-action
  (lambda (l)
    (cond
     ((atom? (car l))
      (cond
       ((eq? (car l) 'quote) *quote)
       ((eq? (car l) 'lambda) *lambda)
       ((eq? (car l) 'cond) *cond)
       (else *application)))
     (else *application))))

(define value
  (lambda (exp)
    (meaning exp '())))

(define meaning
 (lambda (e table)
   ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (second e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (print name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
     ((or
       (else? (question-of (car lines)))
       (meaning (question-of (car lines)) table))
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (e)
    (and (atom? e) (eq? e 'else))))

(define question-of first)
(define answer-of second)

(define cond*
  (lambda (e table)
    (evcond (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else (cons (meaning (car args) table)
                 (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (e)
    (eq? 'primitive (first e))))

(define non-primitive?
  (lambda (e)
    (eq? 'non-primitive (first e))))

(define apply
  (lambda (fn args)
    (cond
     ((primitive? fn)
      (apply-primitive (second fn) args))
     ((non-primitive? fn)
      (apply-closure (second fn) args)))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (+ (first vals) 1))
     ((eq? name 'sub1)
      (- (first vals) 1))
     ((eq? name 'number?)
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((null? x) #f)
     ((atom? x) #t)
     ((eq? (car x) 'primitive) #t)
     ((eq? (car x) 'non-primitive) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (cons
              (build (formals-of closure) vals)
              (table-of closure)))))

