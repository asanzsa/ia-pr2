;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname PracticaLab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;------------------------------------------
;
; Práctica 2 de Inteligencia Artificial
;
; Autores:    Álvaro Sanz Sanz
;             Mario Alejandro Abajo Gamarra
;             
;------------------------------------------

; Descripción del problema: 
; ------------------------
; Sobre la base del mismo mapa de carreteras usado en el primer trabajo, modificar el algoritmo de búsqueda
; para construir un programa Scheme que implemente las búsquedas en profundidad o en profundidad iterativa 
; (a elegir por el usuario) para realizar una búsqueda de rutas entre dos ciudades,elegidas por el usuario, 
; de modo análogo a lo hecho para el primer trabajo.

; Recursos: 
; ------------------------
; Se proporcionan dos mapas (figuras 1 y 2) con el esquema de rutas, así como enlaces a paginarios web con 
; la información sobre distancias entre cada dos ciudades y sobre las coordenadas geográficas de estas.

; Se pide: 
; --------
; Hacer una implementación del algoritmo Profundidad y Profundidad Iterativa para este problema.

; Ejemplo de ejecución del programa:
; ---------------------------------
; i.e.- (encuentra_camino "Katherine" "Darwin")
; i.e.- (encuentra_camino "Perth" "Sydney")


; DATOS
; =====

; La siguiente tabla, expresa las conexiones entre ciudades y la distancia en kilometros entre ellas.
; Viene a representar el mapa de carreteras que usaremos posteriormente en el algoritmo.
; Estos datos han sido sacados de http://distancecalculator.globefeed.com/
(define mapa_carreteras (make-immutable-hash '(
("Darwin" (("Katherine" 311.72)))
("Katherine" (("Darwin" 311.72) ("Tennant Creek" 703.63) ("Halls Creek" 735.36)))
("Halls Creek" (("Katherine" 735.36) ("Port Hedland" 1830.25)))
("Port Hedland" (("Halls Creek" 1830.25) ("Perth" 1517.99)))
("Perth" (("Port Hedland" 1517.99) ("Norseman" 643.76)))
("Norseman" (("Perth" 643.76) ("Eucla" 772.59)))
("Eucla" (("Norseman" 772.59) ("Port Augusta" 968.86)))
("Port Augusta" (("Alice Springs" 1207.19) ("Adelaide" 323.52) ("Eucla" 968.86)))
("Alice Springs" (("Tennant Creek" 519.22) ("Port Augusta" 1207.19)))
("Tennant Creek" (("Katherine" 703.63) ("Mount Isa" 566.02) ("Alice Springs" 519.22)))
("Mount Isa" (("Tennant Creek" 566.02) ("Toowoomba" 1694.23)))
("Toowoomba" (("Mount Isa" 1694.23) ("Brisbane" 119.23) ("Goondiwindi" 225.87)))
("Goondiwindi" (("Toowoomba" 225.87) ("Narrandera" 775.93)))
("Narrandera" (("Goondiwindi" 775.93) ("Sydney" 504.62) ("Canberra" 278.46) ("Melbourne" 424.53) ("Adelaide" 834.43)))
("Melbourne" (("Adelaide" 751.33) ("Narrandera" 424.53) ("Canberra" 536.3) ("Sydney" 820.61)))
("Adelaide" (("Port Augusta" 323.52) ("Narrandera" 834.43) ("Melbourne" 751.33)))
("Canberra" (("Sydney" 284.04) ("Narrandera" 278.46) ("Melbourne" 536.3)))
("Sydney" (("Newcastle" 117.15) ("Canberra" 284.04) ("Melbourne" 820.61) ("Narrandera" 504.62)))
("Newcastle" (("Brisbane" 706.59) ("Sydney" 117.15)))
("Brisbane" (("Bundaberg" 347.38) ("Newcastle" 706.59) ("Toowoomba" 119.23)))
("Bundaberg" (("Rockhampton" 285.98) ("Brisbane" 347.38)))
("Rockhampton" (("Mackay" 324.36) ("Bundaberg" 285.98)))
("Mackay" (("Townsville" 376.77) ("Rockhampton" 324.36) ))
("Townsville" (("Cairns" 323.71) ("Mackay" 376.77)))
("Cairns" (("Townsville" 323.71)))
("Burnie" (("Launceston" 129.77)))
("Launceston" (("Burnie" 129.77) ("Hobart"  188.21)))
("Hobart" (("Launceston"  188.21)))
)))


; FUNCIONES
; =========

; FUNCIÓN PRINCIPAL
; -----------------
; Esta función es la llamada principal del programa, y su cometido es encontrar
; el camino entre dos ciudades pasadas como parámetro. Para ello hace las validación
; previas pertinentes.
;
; PARAMETROS:
; -----------
; Origen : Ciudad inicial.
; Destino : Ciudad final/meta.
;
; i.e.- (encuentra_camino "Katherine" "Darwin")
(define (encuentra_camino Origen Destino) 
  (if (false? (esPosibleCamino Origen Destino))
       (printf "(No es posible llegar desde ~a a ~a por carretera)" Origen Destino)
       (encuentra_camino_tras_comprobacion_profundidad Origen Destino)
  )
)


; Función que comprueba la viabilidad del camino, devolviendo true o false, si no
; hay conexión entre las ciudades.
;
; PARAMETROS:
; Origen : Ciudad inicial.
; Destino : Ciudad final/meta.
;
; i.e.- (esPosibleCamino "Melbourne" "Burnie" )
(define (esPosibleCamino Origen Destino )
  (if (or (and( equal? Origen "Burnie") (or (equal? Destino "Burnie") (equal? Destino "Launceston") (equal? Destino "Hobart")))
          (and( equal? Origen "Hobart") (or (equal? Destino "Burnie") (equal? Destino "Launceston") (equal? Destino "Hobart")))
          (and( equal? Origen "Launceston") (or (equal? Destino "Burnie") (equal? Destino "Launceston") (equal? Destino "Hobart")))
          (and (and (not(equal? Origen "Burnie")) (not(equal? Origen "Launceston")) (not(equal? Origen "Hobart")))
               (and (not(equal? Destino "Burnie")) (not(equal? Destino "Launceston")) (not(equal? Destino "Hobart"))))
      ) #t #f
  )
)


; Encuentra el camino entre dos ciudades Origen Destino tras las comprobaciones previas. 
; Haciendo uso del algoritmo de busqueda en profundidad. En esta función se genera el primer nodo
; de la lista de ABIERTOS.
;
; PARAMETROS:
; Origen : Ciudad inicial.
; Destino : Ciudad final/meta.
;
; i.e.- (encuentra_camino_tras_comprobacion "Katherine" "Darwin")
(define (encuentra_camino_tras_comprobacion_profundidad Origen Destino) 
  (let ((Abiertos (list Origen)) ) ; Nodo Origen.
    (busqueda_profundidad Origen Destino Abiertos null)
  )
)


; Este es el cuerpo del Algoritmo de busqueda primero en profundidad. Que realiza la busqueda
; del camino entre dos ciudades inicales de manera recursiva explorando todos los nodos de una rama
; hasta llegar al final.
;
;   ACTUAL = Escoger el último nodo de la lista ABIERTOS.
;   CERRADOS = Añadir el nodo actual a cerrados.
;   SUCCESORES = explorar el nodo actual obtienendo los nuevos nodos sucesores que no están ni en ABIERTOS ni en Cerrados.
; Mientras la lista Abiertos no esté vacia. Hacer.
;   Evaluar el nodo ACTUAL:
;        Si nodo es meta: imprimir RUTA y terminar.
;        Sino: Llamamos de manera recursiva pasandole la nueva lista de Abiertos, la lista de Cerrados y el nodo Actual.
;
; PARAMETROS:
; Origen : Ciudad inicial.
; Destino : Ciudad final/meta.
; Abiertos : Lista de nodos todavía por explorados.
; Cerrados : Lista de nodos ya explorados.
(define (busqueda_profundidad Origen Destino Abiertos Cerrados)
  ( let (
         (Actual (seleccionar_nodo Abiertos))
         )
     ( let (
               (Cerrados_aux (append (list Actual) Cerrados))
               (Succesores (obtener_nuevos_nodos_sucesores (explorar_nodo_limpio Actual) Cerrados Abiertos '()))
            )
        ( cond 
           ((empty? Abiertos) 
             (printf "~a~n" '("no hay camino")))     ; Todos los nodos visitado sin resultado.
           
           ((equal? Actual Destino) 
             ;(printf "Camino: ~a~n~nCerrados: ~a~n~nAbiertos: ~a~n~n" (obtener_camino (car Cerrados_aux) Cerrados_aux '()) (reverse Cerrados_aux) (reverse Abiertos))) ; Encontramos la meta, e imprimimos el camino.
            (printf "Camino: ~a~n" (obtener_camino (car Cerrados_aux) Cerrados_aux '()))) ; Encontramos la meta, e imprimimos el camino.

           (else (busqueda_profundidad
                  Actual
                  Destino 
                 (borrar_elemento (anyadir_nodos_a_lista Succesores Abiertos) Actual)
                 Cerrados_aux
            ))
         )
      )
   )
)


; Recorre la lista de cerrados, para obtener el camino que se ha recorrido, comprobando que nodos los sucesores del nodo Origen.
; Se llama de manera recursiva para formar el Camino recorrido el cual se devuelve como parametro.
;
; PARAMETROS:
; Origen : Ciudad inicial.
; Cerrados : Ciudad final/meta.
; Camino : Camino recorrido.
;
;i.e.- (obtener_camino "Melbourne" (reverse '("Brisbane" "Toowoomba" "Goondiwindi" "Narrandera" "Adelaide" "Port Augusta" "Eucla" "Norseman" "Perth" "Port Hedland" "Halls Creek" "Katherine" "Tennant Creek" "Darwin" "Alice Springs" "Melbourne")) '())
(define (obtener_camino Origen Cerrados Camino)
  ( let ((hijos_nodo (explorar_nodo_limpio Origen)))
   (cond 
     ((empty? Cerrados) Camino)
     ((es_predecesor (car Cerrados) hijos_nodo)
       (obtener_camino (car Cerrados) (cdr Cerrados) (append (list (car Cerrados)) Camino))
       )
     ((equal? Origen (car Cerrados))
       (obtener_camino (car Cerrados) (cdr Cerrados) (append (list (car Cerrados)) Camino))
       )
     (else (obtener_camino Origen (cdr Cerrados) Camino))
  ))
)


; Comprueba si un nodo es predecesor de otro dado.
;
; PARAMETROS:
; nodo : Nodo a comprobar.
; nodos_adyacentes : lista de nodos adyacentes a otro.
;
(define (es_predecesor nodo nodos_adyacentes)
  (member nodo nodos_adyacentes)
)


; Selecciona el siguiente nodo de la lista de abiertos. Que corresponde a último de la lista de Abiertos.
;
; PARAMETROS:
; Abiertos : Lista de nodos Abiertos.
;
(define (seleccionar_nodo Abiertos)
  (ultimo_elemento_lista Abiertos)
  )


; Obtiene el último elemento de una lista.
;
; PARAMETROS:
; l : Lista de nodos.
;
(define (ultimo_elemento_lista l)
  (cond ((null? l) null)
        ((null? (cdr l)) (car l))
        (else (ultimo_elemento_lista (cdr l)))))


; Añade una lista de nodos 'nodos' al final de la lista 'lista'.
;
; PARAMETROS:
; nodos : lista de nodos.
; lista : lista completa de nodos.
;
; i.e.- (anyadir_nodos_a_lista '(a o) '(c d) )
(define (anyadir_nodos_a_lista nodos lista)
  (if(null? nodos)
     lista
     (if (member (car nodos) lista)
         (anyadir_nodos_a_lista 
          (cdr nodos)
          lista)
         (anyadir_nodos_a_lista 
          (cdr nodos)
          (append lista (list (car  nodos))))
     )
  )
)


; Obtiene una nueva lista de nodos partiendo del parametro nodos y de aquellos elementos que no esten 
; ni en Cerrados ni en Abiertos. Devolviendo como resultado la lista n.
;
; PARAMETROS:
; nodos : lista de nuevos nodos.
; Cerrados : lista de nodos Cerrados.
; Abiertos : lista de nodos Abiertos.
; n : lista combinada de nodos.
;
; i.e.- (obtener_nuevos_nodos_sucesores '(a o) '(c d) '(a b) '())
(define (obtener_nuevos_nodos_sucesores nodos Cerrados Abiertos n)
     (if(null? nodos)
        (reverse n)
        (if (or (member (car nodos) Cerrados) (member (car nodos) Abiertos))
            (obtener_nuevos_nodos_sucesores 
             (cdr nodos)
              Cerrados
              Abiertos
              n)
            (obtener_nuevos_nodos_sucesores 
             (cdr nodos)
             Cerrados
             Abiertos
             (append (list (car nodos)) n))
        )
     )
)


; Función que expande/explora los hijos de un nodo padre (conexiones de una Ciudad).
;
; PARAMETROS:
; nodo : nodo/ciudad a explorar.
;
; i.e.- (explorar_nodo "Darwin")
(define (explorar_nodo nodo)
  (hash-ref mapa_carreteras nodo)
)


; Función que expande/explora los hijos de un nodo padre limpiando el resultado de las distancias entre 
; carreteras de la tabla de la que se sacan (conexiones de una Ciudad).
;
; PARAMETROS:
; nodo : nodo/ciudad a explorar.
;
; i.e.- (explorar_nodo_limpio "Darwin")
(define (explorar_nodo_limpio nodo)
  (clean_node_list (hash-ref mapa_carreteras nodo) '())
)


; Limpia de las distancias de una lista formada por grupos de pares "(nodo distancia)".
;
; PARAMETROS:
; nodo : nodo/ciudad a explorar.
; lclean : lista auxiliar de nodos limpios.
;
; i.e.- (clean_node_list "Katherine")
(define (clean_node_list nodos lclean)
  (cond
    ((not (empty? nodos)) (clean_node_list (cdr nodos) (append lclean (list (car (car nodos))))))
    (else lclean)
   )
)


; Borra un elemento pasado como parametro, de una lista de nodos.
;
; PARAMETROS:
; tl2 : lista de nodos.
; a : elemento a borrar.
;
; i.e.- (borrar_elemento '(("b" 1 1) ("c" 3 2) ("d" 4 3)) '("b" 1 1))
(define (borrar_elemento tl2 a)
      (cond ((null? tl2) (quote ()))
            ((equal? a (car tl2)) (cdr tl2))
            (else (cons (car tl2) (borrar_elemento (cdr tl2) a))))
)

;================================================================================================
;================================================================================================


; TEST
; ----
; i.e.- (encuentra_camino "Katherine" "Darwin")
; TEST 1:(Katherine - Darwin)
(printf "TEST 1: Ciudades corta distancia.\n")
(define test1_a (encuentra_camino "Katherine" "Darwin") )
(define test1_b (encuentra_camino "Darwin" "Katherine") )

; i.e.- (encuentra_camino "Darwin" "Eucla")
; TEST 2:(Darwin - Katherine - Tennant Creek - Alice Springs - Port Augusta - Eucla)
(printf "TEST 2: Ciudades media distancia.\n")
(define test2_a (encuentra_camino "Darwin" "Eucla") )
(define test2_b (encuentra_camino "Eucla" "Darwin") )

; i.e.- (encuentra_camino "Katherine" "Darwin")
; TEST 3:Burnie - Launceston - Hobart)
(printf "TEST 3: Ciudades corta distancia.\n")
(define test3_a (encuentra_camino "Burnie" "Hobart") )
(define test3_b (encuentra_camino "Hobart" "Burnie") )

; i.e.- (encuentra_camino "Darwin" "Eucla")
; TEST 4:(Perth - Norseman - Eucla - Port Augusta - Adelaide - Narrandera - Goondiwindi - Toowoomba - Brisbane - Bundaberg - Rockhampton - Mackay - Townsville - Cairns)
(printf "TEST 4: Ciudades larga distancia.\n")
(define test4_a (encuentra_camino "Perth" "Cairns") )
(define test4_b (encuentra_camino "Cairns" "Perth") )

; i.e.- (encuentra_camino "Port Hedland" "Canberra")
; TEST 5:(Port Hedland - Perth - Norseman - Eucla - Port Augusta - Adelaide - Narrandera - Canberra)
(printf "TEST 5: Ciudades larga distancia.\n")
(define test5_a (encuentra_camino "Port Hedland" "Canberra") )
(define test5_b (encuentra_camino "Canberra" "Port Hedland") )

; i.e.- (encuentra_camino "Norseman" "Burnie")
; TEST 6:(No es posible llegar desde Norseman a Burnie por carretera)
(printf "TEST 6:  Ciudades sin conexión.\n")
(define test6_a (encuentra_camino "Norseman" "Burnie") )
(define test6_b (encuentra_camino "Burnie" "Norseman") )