#lang racket
(require rackunit "othello.rkt")

(check-equal? (get-pos 5 2) 42)
(check-equal? (get-pos 7 7) 63)
(check-equal? (get-pos 1 1) 9)

(check-equal? (get-numcolumna 63) 7)
(check-equal? (get-numcolumna 61) 5)
(check-equal? (get-numcolumna 3) 3)

(check-equal? (get-elem partida1 27) 'blanc)
(check-equal? (get-elem partida1 0) 'libre)
(check-equal? (get-elem partida1 28) 'negra)

(check-equal? (get-numfila 0) 0)
(check-equal? (get-numfila 8) 1)

(check-equal? (get-diagonalID partida1 17) '(17 26 35 44 53 62 8))

(check-equal? (get-diagonalDI partida1 17) '(17 10 3 24))

(check-equal? (cambiar-ficha partida1 0 'negra) (list
                  'negra 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre 
                  'libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre  
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre))



(check-equal? (get-pos-ficha-igual partida1 'dcha 27 (get-diagonalID partida1 27)) 1)

(check-equal? (get-cambios  (list
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
                  'libre 'libre 'negra 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre 
                  'libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre  
                  'libre 'libre 'libre 'libre 'libre 'negra 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre) 18 (get-diagonalID
                                                                               (list
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
                  'libre 'libre 'negra 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre 
                  'libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre  
                  'libre 'libre 'libre 'libre 'libre 'negra 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre) 18)) '(27 36))

(check-equal? (cambiar-tablero (list
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
                  'libre 'libre 'negra 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre 
                  'libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre  
                  'libre 'libre 'libre 'libre 'libre 'negra 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre) 18 '(27 36))
              (list
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
                  'libre 'libre 'negra 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'negra 'negra 'libre 'libre 'libre 
                  'libre 'libre 'libre 'negra 'negra 'libre 'libre 'libre  
                  'libre 'libre 'libre 'libre 'libre 'negra 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre))

(check-equal? (get-oponente 'blanc) 'negra)

(check-equal? (casillas-posibles? partida1 'blanc) #t)

(check-equal? (get-num-fichas partida1 'blanc) 2)

(check-equal? (get-ganador partida1) "")

(check-equal? (get-ganador (list
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
                  'libre 'libre 'negra 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'negra 'negra 'libre 'libre 'libre 
                  'libre 'libre 'libre 'negra 'negra 'libre 'libre 'libre  
                  'libre 'libre 'libre 'libre 'libre 'negra 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre)) " negras")


(check-equal?(heuristica partida1) 0)