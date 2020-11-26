#lang racket
(define partida1 (list
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre 
                  'libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre  
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                  'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre)) ;posiciones 0-63


;---------------------------------------------------------------------------------------------------------------------
(define (get-pos fila columna) ;devuelve posicion dentro de la lista inicial
  (+ (* fila 8) columna))

;---------------------------------------------------------------------------------------------------------------------

(define (get-numcolumna pos)(modulo pos 8)) ;devuelve el numero de columna en el que se encuentra pos
;---------------------------------------------------------------------------------------------------------------------


(define (get-elem lista pos) (list-ref lista pos)); devuelve el elemento de una lista dada una posicion

;---------------------------------------------------------------------------------------------------------------------


(define (get-numfila pos) (floor (/  pos 8))) ;devuelve el numero de fila en el que se encuentra pos

;---------------------------------------------------------------------------------------------------------------------


(define (get-diagonalID partida pos) ;devuelve la diagonal de izq a dcha
  (define fila (get-numfila pos))
  (define columna (get-numcolumna pos))
  (append ;une las listas
   ;suma filas y columnas
   (for/list  ;coge las posiciones de la diagonal de arriba a abajo (incluida la pos introducida)
       ([i (in-range  fila 8)] ;desde la fila pasada hasta la ultima
        [j (in-range columna 8)]) ;desde la columna pasada hasta la ultima
     (get-pos i j)

     )
   ;resta filas y columnas
   (for/list ;coge las posiciones de la diagonal de abajo a arriba 
       ([i (in-range  (- fila 1) -1 -1)] ;desde la fila-1 (ignora la fila pasada) hasta la primera (para atras)
        [j (in-range (- columna 1) -1 -1)]) ;desde la columna-1 (ignora la fila pasada) hasta la primera (para atras)
     (get-pos i j)

     ))
  )

;---------------------------------------------------------------------------------------------------------------------

(define (get-diagonalDI partida pos) ;devuelve la diagonal de dcha a izq
  (define fila (get-numfila pos))
  (define columna (get-numcolumna pos))
  (append ;une las listas
   ;suma columnas y resta filas
   (for/list ;coge las posiciones de la diagonal de abajo a arriba (incluida la pos introducida)
       ([i (in-range  fila -1 -1)] ;desde la fila pasada hasta la primera (para atras)
              [j (in-range columna 8)]) ;desde la columna pasada hasta la ultima
     (get-pos i j)

     )
   ;suma filas y restar columnas
   (for/list ;coge las posiciones de la diagonal de arriba a abajo
       ([i (in-range  (+ fila 1) 8)] ;desde la fila+1 (ignora la fila pasada) hasta la ultima
        [j (in-range (- columna 1) -1 -1)]) ;desde la columna-1 (ignora la fila pasada) hasta la primera (para atras)
     (get-pos i j)

     ))
  )

;---------------------------------------------------------------------------------------------------------------------


(define (cambiar-ficha partida pos color) ;cambiar el color de la ficha en pos
  (list-set partida pos color)
  )

;---------------------------------------------------------------------------------------------------------------------

(define (print-tablero partida) ;imprime el tablero por pantalla
  (for ([i (in-range 0 64)])
    (if (= (get-numcolumna i) 7)
        (printf "~a\n" (get-elem partida i))
        (printf "~a " (get-elem partida i)))
    )
  (printf "\n")
  )

;---------------------------------------------------------------------------------------------------------------------

(define (get-pos-ficha-igual partida direccion pos lista) ;devuelve la ficha igual a pos (mismo color) mas cercana por la direccion introducida. Si entre est ficha y la encontrada hay alguna libre devuelve #f
  (cond
    [(equal? direccion 'izq) ;desde pos hacia la izq
     (for/first ([i (in-range (- (index-of lista pos) 1) -1 -1)] ;recorre la lista hacia la izq
                 #:break (equal? (get-elem partida (get-elem lista i)) 'libre) ;para si encuentra una casilla libre
                 #:when (equal? (get-elem partida (get-elem lista i)) (get-elem partida pos))) ;comprueba si es del mismo color
       i
       )
     ]
    [(equal? direccion 'dcha) ;desde pos hacia la dcha
     (for/first ([i (in-range (+ (index-of lista pos) 1) (length lista))] ;recorre la lista hacia la dcha
                 #:break (equal? (get-elem partida (get-elem lista i)) 'libre) ;para si encuentra una casilla libre
                 #:when (equal? (get-elem partida (get-elem lista i)) (get-elem partida pos))) ;comprueba si es del mismo color
       i
       )
     ]
    )
  )

;---------------------------------------------------------------------------------------------------------------------

(define (get-cambios partida pos list) ;devuelve una lista con las posiciones a cambiar de una lista (si no hay nada que cambiar la lista es vacia)
  (define lista (sort list <)) ;ordena la lista de menor a mayor
  (append
   (if (not (equal? (get-pos-ficha-igual partida 'izq pos lista) #f))
       (for/list ([i (in-range (- (index-of lista pos) 1) (get-pos-ficha-igual partida 'izq pos lista) -1)]) ;recorre la lista desde pos-1 hasta la primera ficha igual por la izq
         (get-elem lista i)
         )
       '()
       )
   (if (not (equal? (get-pos-ficha-igual partida 'dcha pos lista) #f))
       (for/list ([i (in-range (+ (index-of lista pos) 1) (get-pos-ficha-igual partida 'dcha pos lista))]) ;recorre la lista desde pos+1 hasta la primera ficha igual por la dcha
         (get-elem lista i)
         )
       '()
       )
   )
  )

;---------------------------------------------------------------------------------------------------------------------
(define (get-cambios-tablero partida pos) ;devuelve una lista con los cambios a realizar en el tablero al colocar una ficha
  (define fila 
    (lambda (partida pos) ;devuelve las posiciones de una fila a partir de una posicion
      (define primer-pos-fila (* (get-numfila pos) 8)) ;coge la primera posicion de la fila
      (for/list ([i 8])
        ;coge los elementos de la fila (empezando por el primer elemento y sumando de 1 en 1)
        (+ primer-pos-fila i)
        )  
      ))
  (define columna (lambda (partida pos) ;devuelve las posiciones de una columna a partir de una posicion
                    ;coge la columna correspondiente a la posicion (= primera posicion de la columna)
                    (for/list ([i (in-range (get-numcolumna pos) 64 8)]) ;coge los elementos de la columna (empezando en el primer elemento y yendo de 8 en 8)
                      i)
                    ))
  (define diagonalID (get-diagonalID partida pos))
  (define diagonalDI (get-diagonalDI partida pos))
  (append
   (get-cambios partida pos (fila partida pos))
   (get-cambios partida pos (columna partida pos))
   (get-cambios partida pos diagonalID)
   (get-cambios partida pos diagonalDI)
   )
  )


;---------------------------------------------------------------------------------------------------------------------


(define (cambiar-tablero partida pos cambios) ;cambia el tablero con las fichas correspondientes cambiadas de color, es recursivo
  (define color (get-elem partida pos))
  (if (not(empty? cambios))
      (cambiar-tablero (cambiar-ficha partida (car cambios) color) pos (cdr cambios))
      partida))

;---------------------------------------------------------------------------------------------------------------------


(define (get-num-fichas partida jugador) ;obtiene el numero de fichas de un jugador en un tablero (jugador = 'blanc/'negra)
  (for/sum ([i 64]
        #:when (equal? (get-elem partida i) jugador))
    1)
  )
        
;------------------------------------------------------------------------------------------
(define (get-ganador partidafinal) ;obtiene el ganador
  (cond [(> (get-num-fichas partidafinal 'negra) (get-num-fichas partidafinal 'blanc)) " negras"]
        [(< (get-num-fichas partidafinal 'negra) (get-num-fichas partidafinal 'blanc)) "blancas"]
        [else ""]))

;------------------------------------------------------------------------------------------
(define (casillas-posibles? partida jugador) ;devuelve si hay casillas posibles
  (if (empty? (get-movimientos-posibles partida jugador))
      #f
      #t
      )
   )

;------------------------------------------------------------------------------------------
(define (get-movimientos-posibles partida jugador) ;devuelve una lista con los movimientos posibles del jugador
  (for/list ([i 64] ;recorre todas las casillas
        #:when (and (equal? (get-elem partida i) 'libre) (not (empty? (get-cambios-tablero (cambiar-ficha partida i jugador) i))))) ;comprueba que este libre y tenga cambios
    i
    )
  )

;------------------------------------------------------------------------------------------
(define (get-oponente jugador) ;obtiene el oponente
  (if (equal? jugador 'blanc)
      'negra
      'blanc
      )
  )

;------------------------------------------------------------------------------------------
(define (heuristica partida) ;dvuelve el valor de la heuristica (diferencia entre l numero de fichas de un jugador y su oponente), siempre sera blanco, orque la maquina es blanco
  (- (get-num-fichas partida 'blanc) (get-num-fichas partida (get-oponente 'blanc)))
  )

;------------------------------------------------------------------------------------------
 ;saca el valor mas alto de una lista
(define (sacar-max lista num)
  (cond
    [(empty? lista)
     num]
    [else
     
  (if(> (car lista) num)
     (sacar-max (cdr lista) (car lista))
     (sacar-max (cdr lista) num))]))

;------------------------------------------------------------------------------------------
;saca todos los posibles othellos que se generan cambiando la fucha correspondiente de posibles-movimientos de un jugador
(define (hijos partida posibles-movimientos jugador)
  (for/list ([i (length posibles-movimientos)])
    (cambiar-tablero (cambiar-ficha partida (get-elem posibles-movimientos i) jugador) (get-elem posibles-movimientos i) (get-cambios-tablero (cambiar-ficha partida (get-elem posibles-movimientos i) jugador) (get-elem posibles-movimientos i)))
))
;------------------------------------------------------------------------------------------
(define (modo-jugar-maquina estrategia partida jugador alpha beta prof) ;aplicacion de los algoritmos minimax y poda alfa-beta (recursiva)
  (cond
    [(= prof 0) ;profundidad = 0
     (cons (heuristica partida) null)]
    [else ;resto profundidades
     (define posibles-jugadas (hijos partida (get-movimientos-posibles partida jugador) jugador))
     (cond
       [(empty? posibles-jugadas) ;si no tiene movimientos
        (define posibles-jugadas-oponente (hijos partida (get-movimientos-posibles partida (get-oponente jugador)) (get-oponente jugador)))
        (cond
          [(empty? posibles-jugadas-oponente) ;si el oponente no tiene movimientos
           (cons -1 -1)
           ]
          [else ;si el oponente tiene movimientos
           (if (equal? estrategia "poda")
               (cons (- (get-elem (modo-jugar-maquina "poda" partida (get-oponente jugador) (- beta) (- alpha) (- prof 1)) 0)) -1)
               (cons (- (get-elem (modo-jugar-maquina "minimax" partida (get-oponente jugador) null null (- prof 1)) 0)) -1))
           ]
          )
        ]
       [else ;si tiene movimientos
        (cond
          [(equal? estrategia "poda")
              
           (define lista-evaluaciones (for/list [(i (length posibles-jugadas)) #:break (or (> alpha beta) (= alpha beta))]
                                        (- (get-elem (modo-jugar-maquina "poda" (get-elem posibles-jugadas i) (get-oponente jugador) (- beta) (- alpha) (- prof 1)) 0))))                  
           (define maximo (sacar-max lista-evaluaciones alpha)) ;val > alpha = actualiza                      
           (define casilla (get-elem (get-movimientos-posibles partida jugador) (index-of lista-evaluaciones maximo)))
           (cons maximo casilla)
           ]
                
          [else    
           (define lista-evaluaciones (for/list [(i (length posibles-jugadas))]
                                        (- (get-elem (modo-jugar-maquina "minimax" (get-elem posibles-jugadas i) (get-oponente jugador) null null (- prof 1)) 0))))                  
           (define maximo (sacar-max lista-evaluaciones -9999999999))        
           (define casilla (get-elem (get-movimientos-posibles partida jugador) (index-of lista-evaluaciones maximo)))
           (cons maximo casilla)]
          )])]))
        

;------------------------------------------------------------------------------------------
  
(provide get-pos
         get-numfila
         get-numcolumna
         get-diagonalDI
         get-diagonalID
         get-elem
         partida1
         cambiar-tablero
         get-cambios
         get-oponente
         get-cambios-tablero
         cambiar-ficha
         get-ganador
         heuristica
         get-num-fichas
         casillas-posibles?
         get-movimientos-posibles
         get-pos-ficha-igual
         print-tablero
         modo-jugar-maquina
         )