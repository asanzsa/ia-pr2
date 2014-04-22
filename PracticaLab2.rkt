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

; La siguiente tabla, expresa las posición terrestre de cada ciudad en grados decimales.
; Esta información nos valdrá para calcular la distancia aérea entre ciudades.
; Estos datos han sido sacados de http://www.latlong.net/
(define lat_long (make-immutable-hash '(
("Darwin" (-12.462827 130.841777))
("Katherine" (-14.464967 132.264256))
("Halls Creek" (-18.224055 127.668204))
("Port Hedland" (-20.311627 118.575258))
("Perth" (-31.953004 115.857469))
("Norseman" (-32.198568 121.781268))
("Eucla" (-31.677126 128.889304))
("Port Augusta" (-32.492440 137.762818))
("Alice Springs" (-23.700210 133.880611))
("Tennant Creek" (-19.645850 134.191246))
("Mount Isa" (-20.724705 139.497462))
("Toowoomba" (-27.564330 151.953987))
("Goondiwindi" (-28.547206 150.307452))
("Narrandera" (-34.747901 146.550364))
("Melbourne" (-37.814107 144.963280))
("Adelaide" (-34.928621 138.599959))
("Canberra" (-35.282000 149.128684))
("Sydney" (-33.867487 151.206990))
("Newcastle" (-32.926689 151.7789205))
("Brisbane" (-27.471011 153.023449))
("Bundaberg" (-24.864963 152.348653))
("Rockhampton" (-23.377915 150.510103))
("Mackay" (-21.141210 149.185625))
("Townsville" (-19.257622 146.817879))
("Cairns" (-16.920334 145.770860))
("Burnie" (-41.052465 145.906851))
("Launceston" (-41.426181 147.112468))
("Hobart" (-42.881903 147.323815))
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
; Haciendo uso del algoritmo de busqueda A*. En esta función se genera el primer nodo
; de la lista de ABIERTOS.
;
; PARAMETROS:
; Origen : Ciudad inicial.
; Destino : Ciudad final/meta.
;
; i.e.- (encuentra_camino_tras_comprobacion "Katherine" "Darwin")
(define (encuentra_camino_tras_comprobacion_profundidad Origen Destino) 
  (let ((Abiertos (list Origen)) ; Nodo Origen.
   )
  (busqueda_profundidad_2 Origen Destino Abiertos null null)
   )
)



(define (busqueda_profundidad_2 Origen Destino Abiertos Cerrados Camino)
  ( let (
         (Actual (seleccionar_nodo Abiertos))
         )
     ( let (
               (Cerrados_aux (append (list Actual) Cerrados))
               (Succesores (obtener_nuevos_nodos_sucesores (explorar_nodo Actual) Cerrados Abiertos '()))
               (Camino_aux (append (list Actual) Camino))
            )
        ( cond 
           ((empty? Abiertos) (printf "~a~n" '("no hay camino")))     ; Todos los nodos visitado sin resultado.
           ((equal? Actual Destino) (printf "Camino: ~a~n~nCerrados: ~a~n~nAbiertos: ~a~n~n" (reverse Camino_aux) (reverse Cerrados) (reverse Abiertos))) ; Encontramos la meta, e imprimimos el camino.
           ((and (empty? Succesores)  (comprobar_si_es_nodo_cambio Abiertos (explorar_nodo_limpio Origen)))
             (busqueda_profundidad_2 
                                 (car (cdr Camino))
                                 Destino 
                                 Abiertos
                                 Cerrados_aux
                                 (borrar_elemento Camino Origen)
                                 ))        ; Sin nodos validos, volvemos atras.
           
           ((empty? Succesores) (busqueda_profundidad_2 
                                 (car Camino)
                                 Destino 
                                 (borrar_elemento Abiertos Actual)
                                 Cerrados_aux
                                 Camino
                                 ))        ; Sin succesores validos, volvemos atras.
           
           (else (busqueda_profundidad_2 
                  Actual
                  Destino 
                 (borrar_elemento (anyadir_nodos_a_lista Succesores Abiertos) Actual)
                 Cerrados_aux
                 Camino_aux
            ))
         )
      )
   )
)




(define (comprobar_si_es_nodo_cambio Abiertos Lista)
   (member (ultimo_elemento_lista Abiertos) Lista)
   )
; Selecciona el siguiente nodo de la lista de abiertos.
(define (seleccionar_nodo Abiertos)
  (ultimo_elemento_lista Abiertos)
  )


; Obtiene el último elemento de una lista.
(define (ultimo_elemento_lista l)
  (cond ((null? l) null)
        ((null? (cdr l)) (car l))
        (else (ultimo_elemento_lista (cdr l)))))


; elimina el elemento obj de la lista list1.
(define (rmobject list1 obj)
  (cond ((null? list1)
         '())
        ((equal? obj (car list1))
         (rmobject (cdr list1) obj))
        (else
         (cons (car list1)
               (rmobject (cdr list1) obj)))))



; Genera la lista de nodos sucesores de una ciudad padre.
;
; PARAMETROS:
; nodos : lista de ciudades a generar nodo.
; n : lista de nodos generados.
; Cerrados : lista de nodos Cerrados.
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

(define (obtener_nuevos_nodos_sucesores nodos Cerrados Abiertos n)
     (if(null? nodos)
        (reverse n)
        (if (or (member (car (car nodos)) Cerrados) (member (car (car nodos)) Abiertos))
            (obtener_nuevos_nodos_sucesores 
             (cdr nodos)
              Cerrados
              Abiertos
              n)
            (obtener_nuevos_nodos_sucesores 
             (cdr nodos)
             Cerrados
             Abiertos
             (append (list (car (car nodos))) n))
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

; i.e.- (explorar_nodo_limpio "Katherine")
(define (explorar_nodo_limpio nodo)
  (clean_node_list (hash-ref mapa_carreteras nodo) '())
)
(define (clean_node_list nodos lclean)
  (cond
    ((not (empty? nodos)) (clean_node_list (cdr nodos) (append lclean (list (car (car nodos))))))
    (else lclean)
   )
)


;================================================================================================
;================================================================================================

; Encuentra el camino entre dos ciudades Origen Destino tras las comprobaciones previas. 
; Haciendo uso del algoritmo de busqueda A*. En esta función se genera el primer nodo
; de la lista de ABIERTOS.
;
; PARAMETROS:
; Origen : Ciudad inicial.
; Destino : Ciudad final/meta.
;
; i.e.- (encuentra_camino_tras_comprobacion "Katherine" "Darwin")
(define (encuentra_camino_tras_comprobacion Origen Destino) 
  (let ((Abiertos (list (generar_nodo_inicial Origen Destino))) ; Nodo Raiz/Origen + h(0km + distanciaAerea).
   )
  (busqueda_A_estrella Origen Destino Abiertos)
   )
)

; Este es el cuerpo del Algoritmo de busqueda A*. Que realiza la busqueda
; del camino entre dos ciudades inicales de manera recursiva.
;
; Mientras la lista Abiertos no esté vacia. Hacer.
;   ACTUAL = Escoger el mejor nodo de la lista ABIERTOS. (El de menor distancia)
;   Evaluar el nodo ACTUAL:
;        Si nodo es meta: imprimir RUTA y terminar.
;        Sino: Obtener los nodos de ACTUAL, calculando el valor del nodo con la función Heuristica (SUCESORES).
;              Pasamos los nodos SUCESORES a la lista de ABIERTOS.
;              Eliminamos el ACTUAL de la lista de ABIERTOS.
;              Llamamos de manera recursiva pasandole los nuevos parametros.
;   Añadir los SUCESORES a la lista de ABIERTOS.
; Presentar FALLO y terminar.
;
; PARAMETROS:
; Origen : Estado inicial.
; Destino : Estado final/meta.
; Abiertos : Lista de nodos pendientes de evaluar.
;
; i.e- (busqueda_A_estrella "Darwin" "Eucla" null '("Darwin" 0))
(define (busqueda_A_estrella Origen Destino Abiertos)
  (let ((actual (busca_menor Abiertos)))
    (if (empty? Abiertos)
        '("no hay camino") ; "camino: "
        (if (equal? (nombre_nodo actual) Destino)
            (printf "~a~n" (list-ref actual 3))
            (busqueda_A_estrella 
                  actual
                  Destino 
                 (borrar_elemento (append (generar_nodos_sucesores (explorar_nodo actual) actual Destino '()) Abiertos) actual)
             )
        )
    )
  )
)

; Función que expande/explora los hijos de un nodo padre (conexiones de una Ciudad).
;
; PARAMETROS:
; nodo : nodo/ciudad a explorar.
;
; i.e.- (explorar_nodo "Darwin")
;(define (explorar_nodo nodo)
;  (hash-ref mapa_carreteras (nombre_nodo nodo))
;)


; Calcula la heurística para un nodo dado.
;
; i.e.- (f_heuristica_nodo "Darwin" '("Eucla" 110))
;(define (f_heuristica_nodo Destino nodo)
;  (list (car nodo) (+ (car (cdr nodo)) (distanciaAerea Destino (car nodo))) (car (cdr nodo)))
;)


; Función que dado el origen y la meta, genera el primer nodo o estado inicial.
;
; PARAMETROS:
; origen : nodo/ciudad inicial.
; meta : Destino/meta que se quiere alcanzar.
;
; i.e.- (generar_nodo_inicial "Darwin" "Katherine")
(define (generar_nodo_inicial origen meta)
  (list origen
        0
        (distanciaAerea origen meta)
        (list origen)
  )
)

; Genera la lista de nodos sucesores de una ciudad padre.
;
; PARAMETROS:
; nodos : lista de ciudades a generar nodo.
; padre : nodo padre de la lista de nodos (nodos).
; meta : nodo/ciudad destino/meta.
; n : lista de nodos generados.
(define (generar_nodos_sucesores nodos padre meta n)
  (if(null? nodos)
     n
     (generar_nodos_sucesores 
           (cdr nodos) 
           padre
           meta 
           (append (list (generar_nodo padre (car nodos) meta)) n))
  )
)

; Genera la información de un nodo (hijo) a partir del nodo padre y la meta.
;
; PARAMETROS:
; padre : lista de ciudades a generar nodo.
; hijo : lista de ciudades a generar nodo.
; meta : lista de ciudades a generar nodo.
;
; i.e.- (generar_nodo (list "Darwin" 100 32 (list "Darwin")) (list "Katherine" 110) "Tennant Creek")
(define (generar_nodo padre hijo meta)
  (list (nombre_nodo hijo)
         (+ (distancia_acumulada_nodo padre) (distancia_acumulada_nodo hijo))
         (distanciaAerea (nombre_nodo hijo) meta)
         (append (list-ref padre 3) '("-") (list (nombre_nodo hijo)))
  )
)

; Obtiene el nombre de un nodo.
;
; PARAMETROS:
; nodo : información del nodo.
;
; i.e.- (nombre_nodo '("b" 2))
(define (nombre_nodo nodo)
  (list-ref nodo 0)
)

; Calcula la distancia heurística a partir de la información del nodo.
; Suma la distancia acumulada y la distancia aerea al nodo meta.
;
; PARAMETROS:
; nodo : información del nodo.
;
; i.e.- (distancia_heuristica_nodo '(list "Darwin" 100 32 (list "Darwin"))
(define (distancia_heuristica_nodo nodo)
  (+ (list-ref nodo 1) (list-ref nodo 2))
)

; Obtiene la distancia acumulada de un nodo.
;
; PARAMETROS:
; nodo : información del nodo.
;
; i.e.- (distancia_heuristica_nodo '(list "Darwin" 100 32 (list "Darwin"))
(define (distancia_acumulada_nodo nodo)
  (list-ref nodo 1)
)

; Busca el menor elemento de una lista de nodos.
;
; PARAMETROS:
; tl : lista de nodos.
;
; i.e.- (busca_menor '(("b" 2) ("c" 3) ("d" 4)))
(define (busca_menor tl)
  (encuentra_menor tl (car tl))
)


; Encuentra el menor elemento de una lista de nodos.
;
; PARAMETROS:
; tl : lista de nodos.
; b : un elemento nodo.
;
; i.e.- (encuentra_menor '(("b" 2 3) ("c" 3 2) ("d" 4 1)) '("d" 4 1))
(define (encuentra_menor tl b)
  (cond ((null? tl) b)
        ((< (distancia_heuristica_nodo (car tl)) (distancia_heuristica_nodo b))
            (encuentra_menor (cdr tl) (car tl)))
        (else (encuentra_menor (cdr tl) b)))
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

; Calcula la distancia aerea entre 2 ciudades dadas.
;
; PARAMETROS:
; Cit1 : Ciudad 1.
; Cit2 : Ciudad 2.
;
; i.e.- (distanciaAerea "Darwin" "Katherine")
(define (distanciaAerea Cit1 Cit2)
   ( let ( 
          (lat1 (covertirARadianes (list-ref (hash-ref lat_long Cit1) 0)))
          (lat2 (covertirARadianes (list-ref (hash-ref lat_long Cit2) 0)))
          (lng1 (covertirARadianes (list-ref (hash-ref lat_long Cit1) 1)))
          (lng2 (covertirARadianes (list-ref (hash-ref lat_long Cit2) 1)))
          )
      (convertirAKilometros (leyEsfericaDelCoseno (formulaDeHaversine (delta lat1 lat2) (delta lng1 lng2) (abs lat1) (abs lat2))))
   )
)

; Calcula la diferencia entre dos posiciones terrestres expresadas en radianes.
;
; PARAMETROS:
; l1 : posición en radianes 1.
; l2 : posición en radianes 2.
;
; i.e.- (delta 200 100)
(define (delta l1 l2)
  (if (= (sgn l1) (sgn l2))
      (abs (- (abs l2) (abs l1)))
      (+ (abs l2) (abs l1))
  )
)
  
; Convierte un angulo expresado en grados decimales a radianes.
;
; PARAMETROS:
; d : posición en grados decimales.
; 
; i.e.- (covertirARadianes 10)
(define (covertirARadianes d)
  (/ (* pi d) 180)
)

; Formula de Haversine para calcular la distancia entre dos coordenadas de la tierra.
; a = sin^2 (Δlat / 2) + [cos (lat1) x cos (lat2) x sin^2 (Δlong / 2)]
;
; PARAMETROS:
; dlat : diferencial de latitudes (lat2 - lat1).
; dlong : diferencial de longitudes (long2 - long1).
; lat1 : Latitud de la posición 1.
; lat2 : Latitud de la posición 2.
;
; i.e.- (formulaDeHaversine 10 10 1 1)
(define (formulaDeHaversine dlat dlong lat1 lat2)
  (+ (expt (sin (/ dlat 2)) 2) (* (cos lat1) (cos lat2) (expt (sin (/ dlong 2)) 2)))
)
  
; Formula de la Ley esférica del coseno.
; c = 2 x arctan (√ a / √ (1-a))
;
; PARAMETROS:
; a : Salida de la formula de Haversine.
;
; i.e.- (leyEsfericaDelCoseno 10)
(define (leyEsfericaDelCoseno a)
  (* 2 (atan (/ (sqrt a) (sqrt (- 1 a)))))
)

; Convierte una distancia a Kilometros.
; d = R x c  ; R 6371 -> Radio de la tierra.
;
; PARAMETROS:
; a : Salida de la ley eserica del coseno.
;
; i.e.- (convertirAKilometros 1)
(define (convertirAKilometros d)
  (* 6371 d)
)


; TEST
; ----
; i.e.- (encuentra_camino "Katherine" "Darwin")
; TEST 1:(Katherine - Darwin)
;(printf "TEST 1: Ciudades corta distancia.\n")
;(define test1 (encuentra_camino "Katherine" "Darwin") )

; i.e.- (encuentra_camino "Darwin" "Eucla")
; TEST 2:(Darwin - Katherine - Tennant Creek - Alice Springs - Port Augusta - Eucla)
;(printf "TEST 2: Ciudades media distancia.\n")
;(define test2 (encuentra_camino "Darwin" "Eucla") )

; i.e.- (encuentra_camino "Katherine" "Darwin")
; TEST 3:Burnie - Launceston - Hobart)
;(printf "TEST 3: Ciudades corta distancia.\n")
;(define test3 (encuentra_camino "Burnie" "Hobart") )

; i.e.- (encuentra_camino "Darwin" "Eucla")
; TEST 4:(Perth - Norseman - Eucla - Port Augusta - Adelaide - Narrandera - Goondiwindi - Toowoomba - Brisbane - Bundaberg - Rockhampton - Mackay - Townsville - Cairns)
;(printf "TEST 4: Ciudades larga distancia.\n")
;(define test4 (encuentra_camino "Perth" "Cairns") )

; i.e.- (encuentra_camino "Port Hedland" "Canberra")
; TEST 5:(Port Hedland - Perth - Norseman - Eucla - Port Augusta - Adelaide - Narrandera - Canberra)
;(printf "TEST 5: Ciudades larga distancia.\n")
;(define test5 (encuentra_camino "Port Hedland" "Canberra") )

; i.e.- (encuentra_camino "Norseman" "Burnie")
; TEST 6:(No es posible llegar desde Norseman a Burnie por carretera)
;(printf "TEST 6:  Ciudades sin conexión.\n")
;(define test6 (encuentra_camino "Norseman" "Burnie") )