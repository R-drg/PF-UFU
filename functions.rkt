#lang racket

#| Soma |#

(define (soma lista)
    (define (soma-aux lista ac)
      (if (empty? lista)
         ac
         (soma-aux (cdr lista) (+ (car lista) ac))))
(soma-aux lista 0))

#| Exemplo:
> (soma '(1 2 3 4))
10
|#

#| Substitui v1 |#

(define (substitui1 a b lista)
  (define (sub-aux lista ac)
    (if (empty? lista)
        (reverse ac)
        (sub-aux (cdr lista)
                 (if (equal? a (car lista))
                     (cons b ac)
                 (cons (car lista) ac)))))
(sub-aux lista empty))

#| Exemplo:
> (substitui 1 2 '(1 2 3 4))
'(2 2 3 4)
|#

#| Substitui numero v2 |#
(define (substitui2 a b lista)
    (cond
      [(empty? lista) empty]
      [(=(car lista) a) (cons b ( substitui2 a b (cdr lista)))]
      [else (cons (car lista) (substitui2 a b (cdr lista)))]))

#| Exemplo:
> (substitui 1 2 '(1 2 3 4))
'(2 2 3 4)
|#