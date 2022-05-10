  
#lang racket
;--------------------------------------------------------INICIO DEL JUEGO----------------------------------------------------------------------
#|
Params: dimentions of the matrix, m lines, n columns
Funcion de inicio del juego. Recibe como parametros la cantidad de filas y de columnas que tendra la matriz a crear.
Se realizan las validaciones necesarias para verificar que tanto n como m esten entre 3 y 10, dimensiones minimas y maximas
que puede tener el tablero de tik tak toe. En caso de cumplir con una de las dimensiones permitidas, se procede a crear la matriz.
|#
(define (GenerateLogicMatrix n m)
    (cond ((and (>= m 3) (<= m 10) (>= n 3) (<= n 10)) (crearMatriz n m '()))
    (else (display "The values given were incorrect."))))
              
;-------------------------------------------------------CREACION DE LA MATRIZ------------------------------------------------------------------
#|
Funcion que se encarga de la creacion de la estructura de la matriz. De manera recursiva, por cada m especificado por el usuario al
inicializar el programa, se le agrega a la estructura una nueva fila de dimension n. Al tener la matriz creada, de las dimensiones correctas,
se inicia el ciclo de juego, siempre con el usuario colocando la primera ficha.
|#
(define (crearMatriz nContador mContador matriz)
  (if (> mContador 0)
      (crearMatriz nContador (- mContador 1) (append matriz (list(crearFila nContador '()))))
      matriz
  )
)
#|
Funcion de creacion de filas. Funciona como auxiliar de la funcion anterior, que crea la matriz. Es llamada por cada m de las dimensiones de la
matriz, y de manera recursiva agrega 0 (el valor que indica que el tablero esta vacio) por cada n de las dimensiones de la matriz. Esto quiere
decir que se agregan n casillas m veces dentro de la matriz.
|#
(define (crearFila nContador filaCrear)
  (if (> nContador 0)
      (crearFila (- nContador 1) (append filaCrear '(0)))
      filaCrear
   )  
)     
;-----------------------------------------------------TURNO DEL JUGADOR-----------------------------------------------------------------------
#|
Funcion que se encarga de ejecutar el movimiento seleccionado por el usuario. Recibe como parametros la matriz, la columna y la fila donde se
quiere colocar la ficha, ademas de las dimensiones maximas de la matriz de juego.


Primero se valida que las posiciones m n donde se quiere colocar la ficha no sobrepasen a las dimensiones maximas de la matriz. En caso de ser
asi, se llama a la funcion notificarError con un codigo de error 1 o 2, correspondiente a si es la posicion m o n la que causa el error.

Luego, se valida que la posicion m n donde se quiere colocar la ficha no este ocupada, y en caso de estarla se llama a la funcion
notificarError con el codigo de error 3.

En caso de que la posicion m n corresponda a una de las posibles dimensiones, se llama a la funcion placePlayerSymbol.
|#
(define (ejecutarMovimientoJugador matriz mPosicion nPosicion mMaximo nMaximo)
  (if (> mPosicion mMaximo)
     (notificarError 1 matriz mMaximo nMaximo)
      (if (> nPosicion nMaximo)
          (notificarError 2 matriz mMaximo nMaximo)          
          (if (= (encontrarFichaPorPosicion matriz mPosicion nPosicion '()) 0)
              (placePlayerSymbol mPosicion nPosicion 1 mMaximo matriz matriz '() 1)
              (notificarError 3 matriz mMaximo nMaximo)
          )
     )
  )
)
#|
Funcion que coloca la ficha del jugador. Recibe como parametros la posicion m, la posicion n, un contador que siempre inicia en 1, y que
facilita el recorrido por las columnas de la matriz, la cantidad total de columnas m, la matriz a la que se le quiere agregar la ficha, la
nueva matriz que se va formando de la antigua y el elemento que se quiere colocar, en este caso un 1 (que corresponde a la ficha definida
para el usuario).

Primero, se verifica si la posicion m (donde el usuario quiere colocar la ficha) corresponde al valor actual del contador. De ser asi, se sabe
que la llamada recursiva actual es la que corresponde a la posicion de columna correcta dentro de la matriz, por lo que solo faltaria colocar
la ficha en esa columna numero contador, en la posicion deseada nPosicion.
Para esto se llama recursivamente a la funcion placePlayerSymbol (para que siga reconstruyendo la nueva matriz) pero aplicandole un cdr a la
matriz y haciendole un append a la nueva matriz de la lista proveniente de la funcion auxiliar colocarFichaEnFila. Esta es la forma de ir
actualizando la matriz al colocar una ficha. Se va "recortando" la matriz antigua con cdr y se van agregando las columnas con la ficha en
su posicion n correspondiente.

En caso de que la posicion indicada mPosicion no sea igual a contador, quiere decir que la llamada recursiva actual no es la que se encuentra
sobre la columna deseada, por lo que se omite la modificacion de columna y se vuelve a llamar a si misma, aumentando el contador por 1,
haciendole un cdr a la matriz, y agregandole a la nueva matriz la columna actual proveniente de car matriz.

Es importante mencionar que la condicion de parada de esta llamada recursiva es cuando el contador sea mayor que el mTotal (la
cantidad de columnas de la matriz), momento en el cual se ha recorrido, reconstruido y actualizado toda la matriz nueva a partir de la matriz
original, y procede a llamarse a la funcion  actualizarMatriz.
|#
(define (placePlayerSymbol mPosicion nPosicion contador mTotal matrizRespaldo matriz matrizNueva elemento)
  (if (= mPosicion contador)
      (placePlayerSymbol mPosicion
                           nPosicion
                           (+ contador 1)
                           mTotal
                           matrizRespaldo
                           (cdr matriz)
                           (append matrizNueva (list(colocarFichaEnFila nPosicion 1 (length (car matriz)) (car matriz) '() elemento)))
                           elemento
      )
      (if (>= mPosicion contador)
          (placePlayerSymbol mPosicion
                               nPosicion
                               (+ contador 1)
                               mTotal
                               matrizRespaldo
                               (cdr matriz)
                               (append matrizNueva (list(car matriz)))
                               elemento
          )
          (if (<= contador mTotal)
              (placePlayerSymbol mPosicion
                                   nPosicion
                                   (+ contador 1)
                                   mTotal
                                   matrizRespaldo
                                   (cdr matriz)
                                   (append matrizNueva (list(car matriz)))
                                   elemento
              )
              (actualizarMatriz matrizRespaldo matrizNueva mPosicion nPosicion elemento)   
          )
      )
  )   
)

#|
Funcion auxiliar que se encarga de colocar las fichas en la columna especifica, encontrada a partir de la funcion anterior. Recibe como
parametros a la posicion dentro de la fila a donde se quiere colocar la ficha, un contador que facilite el recorrido dentro de la fila,
la dimension total de la fila, la fila original, la nueva fila que se actualiza al agregar el elemento, y el elemento que se quiere agregar.

Trabaja de forma similar a la funcion anterior, reconstruyendo la nueva fila a partir de la fila anterior de manera recursiva, hasta que el
contador es igual a la posicion n especifica, momento en el cual el valor se cambia por el elemento.

Una vez mas, la condicion de parada es cuando el contador sea mayor que la dimension maxima de la fila nTotal, momento en el cual se sabe
que la fila ya esta correctamente reconstruida, al haberla recorrido en su totalidad. Eh ese momento devuelve a la nueva fila ya actualizada
a la funcion placePlayerSymbol, para que esta siga reconstruyendo la matriz.
|#
(define (colocarFichaEnFila nPosicion contador nTotal fila nuevaFila elemento)
  (if (= nPosicion contador)
      (colocarFichaEnFila nPosicion
                          (+ contador 1)
                          nTotal
                          (cdr fila)
                          (append nuevaFila (list elemento))
                          elemento
      )
      (if (>= nPosicion contador)
          (colocarFichaEnFila nPosicion
                              (+ contador 1)
                              nTotal
                              (cdr fila)
                              (append nuevaFila (list(car fila)))
                              elemento
          )
          (if (<= contador nTotal)
              (colocarFichaEnFila nPosicion
                                  (+ contador 1)
                                  nTotal
                                  (cdr fila)
                                  (append nuevaFila (list(car fila)))
                                  elemento
              )
              nuevaFila
         )
      )
  )
)
#|
Funcion encargada de mostrar al usuario la matriz actualizada en consola, ademas de devolverla al cicloDeJuego para que este pueda seguir
su correcto funcionamiento una vez que el usuario coloca una ficha. Recibe como parametros unicamente a la matriz actualizada.
|#
(define (actualizarMatriz matriz matrizNueva mPosicion nPosicion elemento)
  (display "REVISANDO ")
  (display (append (list mPosicion) (list nPosicion)))
  (display (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 1 "Logica"))
  (display matriz)
  (display "\n")

  
  (if (= elemento 1)
      (if (equal? (revisarGaneHorizontal (encontrarFichasColocadas matriz '() 0 1 1 '())
                                         (encontrarFichasColocadas matriz '() 0 1 1 '())
                                         (length (car matriz))
                                         '()
                                         (encontrarFichasColocadas matriz '() 0 1 0 '())
                  )
                  (append (list mPosicion) (list nPosicion))
          )
          (printPrueba(append (list (append (list (append (list (car (revisarGaneHorizontal (encontrarFichasColocadas matriz '() 0 1 1 '())
                                                                                (encontrarFichasColocadas matriz '() 0 1 1 '())
                                                                                (length (car matriz))
                                                                                '()
                                                                                (encontrarFichasColocadas matriz '() 0 1 0 '())
                                                         )
                                                    )
                                              )
                                              (list 1)
                                      )
                                )
                                (list (append (list (car (revisarGaneHorizontal (encontrarFichasColocadas matriz '() 0 1 1 '())
                                                                                (encontrarFichasColocadas matriz '() 0 1 1 '())
                                                                                (length (car matriz))
                                                                                '()
                                                                                (encontrarFichasColocadas matriz '() 0 1 0 '())
                                                         )
                                                    )
                                              )
                                              (list (length (car matriz)))
                                      )
                               )
                        )
                  )                    
                  (list (append (list mPosicion) (list nPosicion)))
                  (list matrizNueva)
          ))
          (if (equal? (revisarGaneVertical (encontrarFichasColocadas matriz '() 0 1 1 '())
                                           (encontrarFichasColocadas matriz '() 0 1 1 '())
                                           (length matriz)
                                           '()
                                           (encontrarFichasColocadas matriz '() 0 1 0 '())
                      )
                      (list (append (list mPosicion) (list nPosicion)))
              )
              (printPrueba(append (list (append (list (append (list (cadr (revisarGaneVertical (encontrarFichasColocadas matriz '() 0 1 1 '())
                                                                                   (encontrarFichasColocadas matriz '() 0 1 1 '())
                                                                                   (length matriz)
                                                                                   '()
                                                                                   (encontrarFichasColocadas matriz '() 0 1 0 '())
                                                             )
                                                        )
                                                  )
                                                  (list 1)
                                          )
                                    )
                                    (list (append (list (cadr (revisarGaneVertical (encontrarFichasColocadas matriz '() 0 1 1 '())
                                                                                   (encontrarFichasColocadas matriz '() 0 1 1 '())
                                                                                   (length matriz)
                                                                                   '()
                                                                                   (encontrarFichasColocadas matriz '() 0 1 0 '())
                                                             )
                                                       )
                                                  )
                                                  (list (length matriz))
                                          )
                                    )
                            )
                      )
                      (list (append (list mPosicion) (list nPosicion)))
                      (list matrizNueva)
              ))
              (if (equal? (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 1 "Logica")
                          (append (list mPosicion) (list nPosicion))
                  )
                  (printPrueba(append (list (append (list (car (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 1 "Interfaz")))
                                                    (list (ultimoElementoLista (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 1 "Interfaz")))
                                )
                          )
                          (list (append (list mPosicion) (list nPosicion)))
                          (list matrizNueva)
                  ))

                  (printPrueba(append (list '())
                          (list (append (list mPosicion) (list nPosicion)))
                          (list matrizNueva)
                  ))
              )
          )
      )
  matrizNueva
  )
)
             
#|
Funcion encargada de la notificacion y correcion de errores en la colocacion de fichas. Recibe como parametros un codigo de error, la matriz,
y las dimensiones de la misma. En caso de que el usuario coloque un valor de m o n mayor a los permitidos, le sera notificado por medio de
esta funcion. El error de tipo 1 corresponde a una posicion m que excede las dimensiones de la matriz. El error de tipo 2 corresponde a una
posicion n que excede las dimensiones de la matriz. El error de tipo indica que esa posicion m n ya esta ocupada.
En caso de existir errores, se llama a la funcion ejecutarMovimientoJugador, con el fin de que el usuario corrija las posiciones seleccionadas.
|#
(define (notificarError tipo matriz mMaximo nMaximo)
  (if (= tipo 1)
      (display "Se ha colocado una posicion de columna no valida para las dimensiones de la matriz \n")
      (if (= tipo 2)
          (printf "Se ha colocado una posicion de fila no valida para las dimensiones de la matriz \n")
          (printf "Se ha colocado una posicion donde ya existe una ficha en la matriz \n")
      )
  )
  (ejecutarMovimientoJugador matriz (string->number (read-line)) (string->number (read-line)) mMaximo nMaximo)
)                                
;--------------------------------------------------------RECORRER LA MATRIZ--------------------------------------------------------------------
#|
Funcion que encuentra todas las posiciones (m n) donde hayan fichas de tipo 1 (jugador), 2 (maquina) o 0 (espacio vacio). Recibe como
parametros a la matriz, la columna actual, un contador para recorrer las columnas, un contador para recorrer las filas, el elemento que
se busca en la matriz y una lista de tuplas (m n) donde se van almacenando las posiciones donde se encuentre al elemento.

Contrario a la funcion de agregar fichas, esta trabaja sin necesidad de actualizar una nueva matriz, por lo que se puede simplemente ir
"recortandola" a lo largo de la ejecucion recursiva.

Primero, la condicion de parada ocurre si tanto la columna actual mActual como la matriz son nulas (ya no quedan elementos que analizar en
la matriz). En caso de ocurrir esto, se llama a la funcion auxiliar actualizarPosiciones para que notifique al usuario.

En caso de que solo la columna actual mActual sea nula, quiere decir que ya se analizaron todas las posiciones de dicha columna, por lo que se
procede a llamar a la funcion encontrarFichasColocadas con el cdr de la matriz, la nueva columna actual como el car de la matriz, aumentando
el contador de la columna donde se encuentra la funcion, reiniciando el contador de fila nContador a 1 (por cada nueva columna, la fila empieza
a contarse en 1) y conservando tanto el elemento buscado como las posiciones previas encontradas.

En caso de que la columna actual mActual no sea nula, quiere decir que quedan elementos por revisar en la misma. Se hace un condicional para
verificar si el primer elemento de la columna actual mActual es igual al elemento, y de ser asi, se hace una llamada recursiva recortando con
cdr la mActual, aumentando el contador de fila, y agregando a las posiciones la tupla (mContador nContador). Caso contrario, se realiza
lo anteriormente descrito, pero sin agregar nada a las posiciones.
|#
(define (encontrarFichasColocadas matriz mActual mContador nContador elemento posiciones)
  (if (null? mActual)
      (if (null? matriz)
          (actualizarPosiciones posiciones elemento)
          (encontrarFichasColocadas (cdr matriz)
                                    (car matriz)
                                    (+ mContador 1)
                                    1
                                    elemento
                                    posiciones
          )      
      )
      (if (equal? (car mActual) elemento)
          (encontrarFichasColocadas matriz
                                    (cdr mActual)
                                    mContador
                                    (+ nContador 1)
                                    elemento
                                    (append posiciones (list (list mContador nContador)))
          )
                                                                                      
          (encontrarFichasColocadas matriz
                                    (cdr mActual)
                                    mContador
                                    (+ nContador 1)
                                    elemento
                                    posiciones
          )
      )
  )
)
#|
Funcion que encuentra el valor de la ficha de una posicion especifica. Nuevamente, funciona de manera similar al procedure anterior. Recibe
como parametros a la matriz, la posicion m n deseada y la columna actual.

Primero, si la columna actual es nula, quiere decir que en la misma no estaba el elemento, por lo que se procede a la siguiente iteracion, esta
vez con el cdr de la matriz como la matriz, y el car de la matriz como la columna actual.

Si la columna actual no es nula, pero la mPosicion (la posicion que se busca) es mayor que 1, se debe seguir con la siguiente iteracion, porque
esto significa que la posicion mPosicion esta mas adelante en la matriz. Se hace el cdr de la matriz, se le resta 1 a mPosicion y se selecciona
la mActual como el car de la matriz (con el fin de ir avanzando en posiciones de columna)

Ahora, en caso de que la posicion mActual si sea 1, quiere decir que se esta ubicado en la columna correcta, por lo que se repite el proceso,
esta vez con el contador nPosicion, para encontrar el elemento n dentro de la columna m. Nuevamente, se resuelve por medio de llamadas
recursivas que va aplicando un cdr a la mActual, y restandole 1 a la nPosicion.

Una vez que tanto mPosicion como nPosicion son iguales a 1 se puede afirmar que la iteracion se encuentra ubicada "sobre" el elemento deseado,
por lo que se llega a la posicion de parada y se retorna el car de la mActual (que contiene al elemento dentro de la posicion seleccionada).
|#
(define (encontrarFichaPorPosicion matriz mPosicion nPosicion mActual)
  (if (null? mActual)
      (encontrarFichaPorPosicion (cdr matriz) mPosicion nPosicion (car matriz))
      (if (> mPosicion 1)
          (encontrarFichaPorPosicion (cdr matriz) (- mPosicion 1) nPosicion (car matriz))
          (if (> nPosicion 1)
              (encontrarFichaPorPosicion matriz mPosicion (- nPosicion 1) (cdr mActual))
              (car mActual)
          )
     )
  )
)                                    
#|
Funcion auxiliar utilizada para notificar al usuario de las posiciones donde se encuentran sus fichas y las de la maquina. Recibe como
parametros a la lista de tuplas de las posiciones, ademas del elemento que se busco (puede ser 1 o 2).

En caso de que el elemento sea 1, se notifica refiriendose al jugador. En caso de que el elemento sea 2, se notifica refiriendose
a la maquina. En caso de que el elemento sea 0, notifica en referencia a los espacios vacios dentro de la matriz. Seguidamente
se muestra en consola la lista de posiciones.

Finalmente se retorna la lista de posiciones para el resto de funciones que asi lo requieran.
|#
(define (actualizarPosiciones posiciones elemento)
  (if (= elemento 1)
      (display "El jugador tiene las siguientes fichas en juego ")
      (if (= elemento 2)
          (display "La maquina tiene las siguientes fichas en juego ")
          (display "Los candidatos posibles para colocar son los siguientes ")
      )
  )
  (display posiciones)
  (display "\n")
  posiciones   
)
(define (ultimoElementoLista lista)
  (if (null? (cdr lista))
      (car lista)
      (ultimoElementoLista (cdr lista))
  )
)
;--------------------------------------------------------TURNO DE LA MAQUINA-------------------------------------------------------------------
#|
Funcion encargada de colocar la ficha de la maquina. Recibe como parametros la matriz de juego y el elemento que se colocara dentro de la misma,
que es por defecto un 2. Esta funcion tiene la finalidad de ayudar a controlar el flujo de juego, al llamarse de forma recursiva y alternada
con la funcion respectiva del turno del usuario. Basicamente inicializa el algoritmo voraz.
|#
(define (placeBotSymbol matriz elemento)
  (ejecutarAlgoritmoVoraz matriz)
)

#|
Funcion encargada de verificar si existe un gane horizontal, ya sea por parte del usuario o de la maquina. Recibe como parametros a las fichas
ya sea del usuario o de la maquina en los parametros llamados fichasMaquina y fichasMaquinaAuxiliar, una de las listas se utiliza para recorrer
la lista (se va recortando) y la otra se utiliza para comparar, por lo que es necesario contar con ambas. Adicionalmente, considerando que la
matriz cuenta con n filas y m columnas, se recibe como parametro este numero m, que sera la cantidad requerida por una linea horizontal para
ser considerada un gane valido. Finalmente, se recibe una ficha para ganar que es una tupla (n m), que se maneja de esta manera para detener
la recursion en el momento que deje de ser nula, es decir, en el momento que se encuentre la primera solucion valida, con la finalidad de
optimizar la ejecucion y no evaluar todas las posiciones si ya se encuentra un gane valido. Recibe tambien la lista de candidatos posibles
para colocar una ficha, con la finalidad de pasarlo a las funciones auxiliares.

Primeramente, valida que la lista de fichas maquina no sea una lista nula. Si es nula, quiere decir que no se encontro una solucion valida
entre todas las fichas colocadas por el usuario o por la maquina, por lo que se retorna un valor nulo por defecto de la ficha para ganar.

En caso de no ser nula, se evalua si la ficha para ganar es nula. En caso de ser nula, quiere decir que no se ha dado con una solucion exitosa
dentro de las posibles lineas horizontales, por lo que se llama de manera recursiva, recortando la lista de fichas auxiliares, y actualizando
la posible ficha para ganar llamando a la funcion fichaParaGanarHorizontal.

En caso de que la ficha para ganar no sea nula, quiere decir que se ha encontrado una solucion horizontal, y se devuelve este valor para que
el programa no tenga que verificar el resto de fichas colocadas por el usuario o por la maquina.
|#
(define (revisarGaneHorizontal fichasMaquina fichasMaquinaAuxiliar mParaGanar fichaParaGanar candidatos)
  (if (null? fichasMaquinaAuxiliar)
      fichaParaGanar

      (if (null? fichaParaGanar)
          (revisarGaneHorizontal fichasMaquina
                                 (cdr fichasMaquinaAuxiliar)
                                 mParaGanar
                                 (fichaParaGanarHorizontal fichasMaquina (car fichasMaquinaAuxiliar) mParaGanar '() candidatos)
                                 candidatos
          )
          fichaParaGanar
      )
  )
)
#|
Funcion que analiza cada una de las fichas colocadas por la maquina o por el usuario, y las compara con la lista de todas las fichas colocadas
por el mismo. Recibe como parametro la lista de fichas colocadas, la ficha especifica a analizar, la cantidad necesaria de fichas en la misma
fila para ganar, y mantiene un parametro recursivo con las posiciones presentes de fichas dentro de una determinada fila n, con la finalidad
de, en caso de faltar solo una ficha dentro de esa fila, cual es la posicion ausente que provocaria el gane de la maquina o del usuario.
Recibe tambien la lista de candidatos posibles para colocar una ficha, con la finalidad de pasarlo a las funciones auxiliares.

Primeramente, se valida como condicion de parada de la recursividad que la lista de fichas colocadas sea nula. En caso de ser nula, quiere
decir que ya se analizaron todas las posibles fichas en comparacion de la fichaAnalizar, por lo que la cantidad de fichas presentes en una
determinada fila n debe estar a una unidad de la cantidad mParaGanar para poder deducir que en esta fila n hay un gane posible.

De ser asi, se llama a la funcion auxiliar determinarFichaParaGanarHorizontal, que determinara cual es la ficha faltante en esa fila n, la cual
sera la ficha que permitira a la maquina o al usuario ganar la partida. En caso de que la cantidad de fichas presentes en esa fila n no este a
una unidad indica dos posibles situaciones: el jugador contrario ya tiene fichas en esa fila, o falta mas de un espacio vacio que rellenar
con fichas propias para poder generar una linea valida. En este ultimo escenario, se retorna una lista nula como representacion de que no
existe una solucion valida para esta fila n especifica.

En caso de que la lista de fichas colocadas no sea nula, quiere decir que dentro de la misma pueden haber fichas que posibiliten una linea
ganadora, por lo que se procede a comparar si el caar (el primer elemento del primer elemento) de las fichas colocadas (numero que indica la
fila n) sea igual a el car (primer elemento) de la ficha analizar. Esto significaria que la fila de la ficha por analizar es la misma de la
fila de la ficha actual dentro de la lista. Debido a esto, se llama de forma recursiva a la funcion, recortando la lista de fichas colocadas,
y agregando el cdar (el ultimo elemento de la tupla, del primer elemento) de las fichas colocadas. Esta actualizacion de presentes quiere decir
que, dentro de esa fila n, hay una nueva posicion m que si tiene una ficha colocada.

En caso de que no sea igual, simplemente se recorta la lista de las fichas colocadas dentro de la llamada recursiva.
|#
(define (fichaParaGanarHorizontal fichasMaquina fichaAnalizar mParaGanar presentes candidatos)
  (if (null? fichasMaquina)
      (if (= (+ (length presentes) 1) mParaGanar)
          (determinarFichaParaGanarHorizontal presentes fichaAnalizar 1 candidatos)
          '()
      )
      (if (= (caar fichasMaquina) (car fichaAnalizar))
          (fichaParaGanarHorizontal (cdr fichasMaquina)
                                    fichaAnalizar
                                    mParaGanar
                                    (append presentes (cdar fichasMaquina))
                                    candidatos
          )
          (fichaParaGanarHorizontal (cdr fichasMaquina)
                                     fichaAnalizar
                                     mParaGanar
                                     presentes
                                     candidatos
          )
      )
  )
)
#|
Funcion final dentro de la validacion de los ganes horizontales. El programa llega a esta funcion solo si de antemano se ha determinado que
la cantidad de fichas de un mismo jugador dentro de una fila esta a una unidad de la cantidad necesaria para ganar, dada por la dimension m
de la matriz. Recibe como parametros la lista de posiciones m donde hay fichas, la ficha en cuestion que esta siendo analizada, y un contador
que sirve de parametro auxiliar recursivo. Finalmente, recibe la lista de candidatos para verificar si se puede colocar una ficha en el
lugar indicado dentro de la matriz

Si la cantidad de posiciones presentes es nula, quiere decir que se ha llegado al final de posibles posiciones a analizar dentro de la fila,
por lo que se retorna entonces la posicion de la ficha ganadora como (posicion n de la ficha a analizar, contador), representando que la unica
posicion valida seria la ultima de la fila, la que tenga un valor de columna m.

En caso de que aun queden elementos en la lista de posiciones presentes, se verifica si el contador es igual al primer elemento de esta lista.
De ser asi, quiere decir que la posicion m ausente no es la actual, ya que el contador es una forma de preguntarle al programa si un valor, que
empieza en 1 y termina en m, esta dentro de lista de posiciones posibles que ya tienen una ficha del jugador. Debido a esto, se sabe
que la posicion ganadora, ausente de ficha, no es la que se encuentra en la iteracion actual, por lo que se procede a la siguiente llamada,
recortando la lista de presentes, y aumentando en 1 el contador.

Si el contador no es igual al primer elemento, se puede decir que entonces el contador es de hecho la posicion m donde se tiene que colocar
la ficha dentro de una determinada fila n, ya que el contador crece de forma sincronica conforme se compara el primer elemento de la lista
ordenada de posibles m's. En el momento en que se llegue a una discrepancia entre contador y primer elemento de la lista de presentes (que esta
ordenada de menor a mayor, al ser rellenada de forma paulatina por el recorrido de la matriz) se retorna el valor de (posicion n de la ficha
a analizar, contador).

Es importante recalcar que la colocacion de fichas solo ocurre si la posicion dada por (fichaAnalizar, contador) esta dentro de la lista
candidatos, por lo que antes de cada colocacion de fichas, se llama a la funcion revisarCandidatos para determinar si se coloca o no la ficha.
|#
(define (determinarFichaParaGanarHorizontal presentes fichaAnalizar contador candidatos)
  (if (null? presentes)
      (if (revisarCandidatos candidatos (append (list (car fichaAnalizar)) (list contador)))
          (append (list (car fichaAnalizar)) (list contador))
          '()
      )
      (if (= contador (car presentes))
          (determinarFichaParaGanarHorizontal (cdr presentes)
                                              fichaAnalizar
                                              (+ contador 1)
                                              candidatos
          )
          (if (revisarCandidatos candidatos (append (list (car fichaAnalizar)) (list contador)))
              (append (list (car fichaAnalizar)) (list contador))
              '()
          )
      )
  ) 
)
#|
Funcion que tiene un comportamiento similar a la de revisarGaneHorizontal. Recibe como parametros las dos listas de fichas colocadas por el
usuario o por la maquina, ademas de la cantidad necesaria de fichas colocadas en una misma columna m, llamada nParaGanar, y determinada por la
dimension n de la matriz. La lista de candidatos cumple la misma funcion que en la definicion anterior. De igual manera que la validacion
horizontal, son tres funciones las que componen esta parte del codigo.
Primeramente, se recorre la lista de fichasMaquinaAuxiliar, y en cada iteracion, hasta que la misma este vacia (es decir, que sea nula), se
trata de encontrar una nueva ficha para ganar por medio de la funcion fichaParaGanarVertical.
|#
(define (revisarGaneVertical fichasMaquina fichasMaquinaAuxiliar nParaGanar fichaParaGanar candidatos)
  (if (null? fichasMaquinaAuxiliar)
      fichaParaGanar
      (if (null? fichaParaGanar)
          (revisarGaneVertical fichasMaquina
                               (cdr fichasMaquinaAuxiliar)
                               nParaGanar
                               (fichaParaGanarVertical fichasMaquina (car fichasMaquinaAuxiliar) nParaGanar '() candidatos)
                               candidatos
          )
          fichaParaGanar
      )
  )
)
#|
Por cada ficha colocada por el usuario o por la maquina, se intenta encontrar una ficha que permita el gane por medio de esta funcion. Recibe
como parametro a las fichas colocadas por el usuario o por la maquina, la ficha especifica a analizar en la iteracion actual, la dimension
que debe tener la linea para ganar, dada por nParaGanar, una lista presentes que sirve de auxiliar dentro de la recursividad, y la lista de
candidatos donde, de poder formar una linea, el sistema podria o no colocar una ficha.

Esta funcion se recorre de manera recursiva hasta que la lista de fichas del jugador sea vacia, en este momento se valida si la cantidad
de fichas presentes en una determinada columna m esta a una unidad de la cantidad necesaria, lo cual indicaria que solo falta una ficha
para ganar en dicha columna m. De cumplirse lo anterior, se ejecuta la funcion determinarFichaParaGanarVertical, que encontrara la ficha
que el jugador debe colocar para ganar.

Si la lista de fichas del jugador no es vacia, se seguiran comparando sus valores m con el de la iteracion actual, con el fin de verificar
cuantas fichas hay presentes dentro de una columna, y poder decidir si se puede determinar una ficha que permita el gane.
|#
(define (fichaParaGanarVertical fichasMaquina fichaAnalizar nParaGanar presentes candidatos)
  (if (null? fichasMaquina)
      (if (= (+ (length presentes) 1) nParaGanar)
          (determinarFichaParaGanarVertical presentes fichaAnalizar 1 candidatos)
          '()
      )
      (if (= (length fichasMaquina) 1)
          (if (= (cadar fichasMaquina) (cadr fichaAnalizar))
              (fichaParaGanarVertical (cdr fichasMaquina)
                                       fichaAnalizar
                                        nParaGanar
                                        (append presentes (list (caar fichasMaquina)))
                                        candidatos
              )
              (fichaParaGanarVertical (cdr fichasMaquina)
                                        fichaAnalizar
                                        nParaGanar
                                        presentes
                                        candidatos
              )
          )
                                        
          (if (= (cadar fichasMaquina) (cadr fichaAnalizar))
              (fichaParaGanarVertical (cdr fichasMaquina)
                                        fichaAnalizar
                                        nParaGanar
                                        (append presentes (list (caar fichasMaquina)))
                                        candidatos
              )
              (fichaParaGanarVertical (cdr fichasMaquina)
                                        fichaAnalizar 
                                        nParaGanar
                                        presentes
                                        candidatos
              )
          )
      )
  )
)
#|
Funcion utilizada para determinar con cual ficha el usuario o la maquina pueden ganar con una linea vertical. Si el sistema ingresa aqui,
significa que, o usuario o maquina, estan a una ficha de ganar en alguna de las columnas, pero no necesariamente pueden colocar una ficha
en esa posicion. Para eso justamente es que funciona el parametro de candidatos, que contiene todas las posiciones vacias dentro de la matriz,
y hace posible la colocacion de fichas si la posicion deseada esta contendida en dicha lista.

El funcionamiento es el mismo que el de determinarFichaParaGanarHorizontal, variando solo la forma de retornar el resultado, refiriendose a
una posicion (m n), debido a que la linea que se quiere formar es vertical.
|#
(define (determinarFichaParaGanarVertical presentes fichaAnalizar contador candidatos)
  (if (null? presentes)
      (if (revisarCandidatos candidatos (append (list contador) (list (cadr fichaAnalizar))))
          (append (list contador) (list (cadr fichaAnalizar)))
          '()
      )
      (if (= contador (car presentes))
          (determinarFichaParaGanarVertical (cdr presentes)
                                              fichaAnalizar
                                              (+ contador 1)
                                              candidatos
          )
          (if (revisarCandidatos candidatos (append (list contador) (list (cadr fichaAnalizar))))
              (append (list contador) (list (cadr fichaAnalizar)))
              '()
          )
      )
  ) 
)
#|
Función que se encarga de retornar todas las diagonales válidas que podrían generar un gane dentro de la matriz de juego. Recibe como parámetro a la matriz.
Se compone de cuatro subfunciones que encuentran las diagonales de tipo descendentes y ascendentes, en dos variantes denominadas “diagonales verticales” y “diagonales horizontales”.
|#
(define (diagonalesValidas matriz)
  (append (diagonalesDescendentesVerticales (length (car matriz))
                                            (length matriz)
                                            1
                                            (length matriz)
                                            (length matriz)
                                            '()
                                            '()
          )
          (diagonalesDescendentesHorizontales (length (car matriz))
                                              (length matriz)
                                              2
                                              1
                                              2
                                              '()
                                              '()
          )
          (descartarDiagonales (diagonalesAscendentesVerticales (length (car matriz))
                                                                (length matriz)
                                                                (length (car matriz))
                                                                (length matriz)
                                                                (length matriz)
                                                                '()
                                                                '()
                               )
                               '()
          )
          (descartarDiagonales (diagonalesAscendentesHorizontales (length (car matriz))
                                                                  (length matriz)
                                                                  (- (length (car matriz)) 1)
                                                                  1
                                                                  (- (length (car matriz)) 1)
                                                                  '()
                                                                  '()
                               )
                               '()
          )
  )           
)
#|
Función encargada de identificar las diagonales de tipo ascendente vertical que se encuentran dentro de la matriz.
Recibe como parámetros a la cantidad de columnas mMax, la cantidad de filas nMax, un contador de la fila nActual, un contador de la fila donde comenzó
la diagonal actual nInicio, la diagonalActual que se va conformando de manera recursiva, y la lista de diagonales que este tipo que pueden ser formadas.
Esta función va recorriendo, por cada nInicio (que inicia en nMax y termina en 0) el diagonal que se puede formar hasta llegar a un punto donde no queden
posiciones que agregar al diagonalActual. Lleva un recuento del nActual y mActual, y en el momento en que ambas superen a nMax y mMax respectivamente,
se sabe que no quedan posiciones en diagonal que agregar al diagonalActual, por lo que se procede a repetir el proceso, pero reduciendo la fila donde
empieza el diagonal nInicio, con el fin de considerar todos los diagonales de tipo ascendente vertical.
|#  
(define (diagonalesAscendentesVerticales mMax nMax mActual nActual nInicio diagonalActual diagonales)
  (if (= nInicio 0)
      diagonales
      (if (= nActual nMax)
          (diagonalesAscendentesVerticales mMax
                                           nMax
                                           mMax
                                           (- nInicio 1)
                                           (- nInicio 1)
                                           '()
                                           (append diagonales (list (append diagonalActual (list (append (list nActual) (list mActual)))))))
          (if (= mActual 1)
              (diagonalesAscendentesVerticales mMax
                                               nMax
                                               mMax
                                               (- nInicio 1)
                                               (- nInicio 1)
                                               '()
                                               (append diagonales (list (append diagonalActual (list (append (list nActual) (list mActual)))))))
              (diagonalesAscendentesVerticales mMax
                                               nMax
                                               (- mActual 1)
                                               (+ nActual 1)
                                               nInicio
                                               (append diagonalActual (list (append (list nActual) (list mActual))))
                                                diagonales)))))
#|
Función encargada de identificar las diagonales de tipo descendente vertical que se encuentran dentro de la matriz.
Recibe como parámetros a la cantidad de columnas mMax, la cantidad de filas nMax, un contador que indica el mActual,
un contador que indica el nActual, un contador mInicio que indica la columna donde inició el diagonal actual, el
diagonalActual y la lista de diagonales de tipo descendentes verticales. De igual manera que la función anterior,
va analizando cada diagonal hasta que se llega a que mInicio es mayor a mMax, por lo que se dice que no quedan diagonales
de tipo descendente vertical.
|#  
(define (diagonalesAscendentesHorizontales mMax nMax mActual nActual mInicio diagonalActual diagonales)
  (if (= mInicio 0)
      diagonales
      (if (= nActual nMax)
          (diagonalesAscendentesHorizontales mMax
                                             nMax
                                             (- mInicio 1)
                                             1
                                             (- mInicio 1)
                                             '()
                                             (append diagonales (list (append diagonalActual (list (append (list nActual) (list mActual)))))))
          (if (= mActual 1)
              (diagonalesAscendentesHorizontales mMax
                                                 nMax
                                                 (- mInicio 1)
                                                 1
                                                 (- mInicio 1)
                                                 '()
                                                 (append diagonales (list (append diagonalActual (list (append (list nActual) (list mActual)))))))
              (diagonalesAscendentesHorizontales mMax
                                                 nMax
                                                 (- mActual 1)
                                                 (+ nActual 1)
                                                 mInicio
                                                 (append diagonalActual (list (append (list nActual) (list mActual))))
                                                 diagonales)))))
                                                                                  
(define (diagonalesDescendentesVerticales mMax nMax mActual nActual nInicio diagonalActual diagonales)
  (if (= nInicio 0)
      diagonales
      (if (= (- nActual 1) nMax)
          (if (> (length diagonalActual) 2)
              (diagonalesDescendentesVerticales mMax
                                                nMax
                                                1
                                                (- nInicio 1)
                                                (- nInicio 1)
                                                '()
                                                (append diagonales (list diagonalActual))
              )
              (diagonalesDescendentesVerticales mMax
                                                nMax
                                                1
                                                (- nInicio 1)
                                                (- nInicio 1)
                                                '()
                                                diagonales
              )
          )
          (if (<= mActual mMax)    
              (diagonalesDescendentesVerticales mMax
                                                nMax
                                                (+ mActual 1)
                                                (+ nActual 1)
                                                nInicio
                                                (append diagonalActual (list (append (list nActual) (list mActual))))
                                                diagonales
              )
              (diagonalesDescendentesVerticales mMax
                                                nMax
                                                (+ mActual 1)
                                                (+ nActual 1)
                                                nInicio
                                                diagonalActual
                                                diagonales
              )))))

(define (diagonalesDescendentesHorizontales mMax nMax mActual nActual mInicio diagonalActual diagonales)
  (if (> mInicio mMax)
      diagonales
      (if (> nActual nMax)
          (if (> (length diagonalActual) 2)
              (diagonalesDescendentesHorizontales mMax
                                                  nMax
                                                  (+ mInicio 1)
                                                  1
                                                  (+ mInicio 1)
                                                  '()
                                                  (append diagonales (list diagonalActual)))
              (diagonalesDescendentesHorizontales mMax
                                                  nMax
                                                  (+ mInicio 1)
                                                  1
                                                  (+ mInicio 1)
                                                  '()
                                                  diagonales))
          (if (> mActual mMax)
              (if (> (length diagonalActual) 2)
                  (diagonalesDescendentesHorizontales mMax
                                                      nMax
                                                      (+ mInicio 1)
                                                      1
                                                      (+ mInicio 1)
                                                      '()
                                                      (append diagonales (list diagonalActual)))
                  (diagonalesDescendentesHorizontales mMax
                                                      nMax
                                                      (+ mInicio 1)
                                                      1
                                                      (+ mInicio 1)
                                                      '()
                                                      diagonales))
              (diagonalesDescendentesHorizontales mMax
                                                  nMax
                                                  (+ mActual 1)
                                                  (+ nActual 1)
                                                  mInicio
                                                  (append diagonalActual (list (append (list nActual) (list mActual))))
                                                  diagonales)))))
#|
Función que recibe como parámetros una lista de diagonales y una lista vacía de diagonalesValidos, y por medio de
llamadas recursivas y comparaciones de cada diagonal, se determina cuales tienen una longitud mayor o igual a tres,
por lo que pueden ser consideradas diagonalesValidos. Al analizar cada diagonal de diagonales, retorna la lista de diagonalesValidos.
|# 
(define (descartarDiagonales diagonales diagonalesValidos)
  (if (null? diagonales)
      diagonalesValidos
      (if (>= (length (car diagonales)) 3)
          (descartarDiagonales (cdr diagonales) (append diagonalesValidos (list (car diagonales))))
          (descartarDiagonales (cdr diagonales) diagonalesValidos))))
#|
Función encargada de analizar si existe una posición en la cual, en caso de colocar una ficha, se produzca un gane para el jugador.
Recibe como parámetros a la matriz, una lista de diagonales válidos, la posible fichaGanar (que por defecto es nula), el usuario
(jugador o maquina) que quiere revisar si existe una posición para ganar y un código de tipo string que se utiliza más adelante
para saber si la información requerida es para la parte lógica o la parte de interfaz. De igual manera que con revisarGaneHorizontal
y revisarGaneVertical, se llama esta función de forma recursiva hasta que analizarDiagonal retorne una posición que no sea nula,
o se acaben los posibles diagonales.
|# 
(define (revisarGaneDiagonal matriz diagonales fichaGanar usuario codigo)
  (if (null? diagonales)
      fichaGanar
      (if (null? fichaGanar)
          (revisarGaneDiagonal matriz
                               (cdr diagonales)
                               (analizarDiagonal matriz usuario (car diagonales) (car diagonales) 0 codigo)
                               usuario
                               codigo)
          fichaGanar
      )
  )
)
#|
Función encargada de analizar cada diagonal para determinar si existe un posible gane dentro de la misma. Recibe como parámetros a la matriz,
al usuario que quiere verificar si puede ganar, el diagonal a analizar, una copia de la diagonal a analizar diagonalAuxiliar, la cantidad de
fichas que hay en el diagonal, y el mismo código de la función anterior. Por cada posición dentro del diagonal, y en caso de que la función
analizarPosicionDiagonal retorne un true, se suma un 1 a la cantidad, por lo que, al terminarse las posiciones de diagonal, y si la cantidad
está a una unidad de la longitud de diagonalAuxiliar, se retorna el diagonalAuxiliar o la posición para ganar proveniente de
determinarFichaParaGanarDiagonal, en función de si el código es “Interfaz” o “Lógica”. El motivo por el cual se utiliza el código es porque,
para la interfaz se tienen que enviar solo la posición inicial y final de la diagonal (con el fin de dibujar la línea del gane), y para la lógica
se tiene que retornar únicamente la posición que permita el gane.
|# 
(define (analizarDiagonal matriz usuario diagonal diagonalAuxiliar cantidad codigo)
  (if (null? diagonal)
      (if (= (+ cantidad 1) (length diagonalAuxiliar))
          (determinarFichaGaneDiagonal matriz diagonalAuxiliar diagonalAuxiliar codigo)
          '())
      (analizarDiagonal matriz
                        usuario
                        (cdr diagonal)
                        diagonalAuxiliar
                        (+ cantidad (analizarPosicionDiagonal matriz usuario (car diagonal)))
                        codigo)))
#|
Función que recibe como parámetros a la matriz, el usuario que se busca y la posición en la que se busca dentro de la matriz.
Se utilizar a la función encontrarFichaPorPosicion para determinar si en esa posición específica existe una ficha de valor usuario (1 o 2).
De ser así retorna un 1 que es sumado a la cantidad de la función anterior.
|# 
(define (analizarPosicionDiagonal matriz usuario posicion)
  (if (= (encontrarFichaPorPosicion matriz (car posicion) (cadr posicion) '()) usuario)
      1
      0))
#|
Función utilizada para determinar cuál es la posición donde hace falta una ficha dentro del diagonal. Recibe como parámetros a la matriz y a la diagonal.
Por cada elemento posición dentro de diagonal, se verifica si analizarPosicionVaciaDiagonal retorna un true. De ser así, quiere decir que la posición actual
dentro de la diagonal es vacía, por lo que se retorna dicha posición. De no ser así, se procede con la siguiente posición de la diagonal, hasta que la misma sea nula.
|# 
(define (determinarFichaGaneDiagonal matriz diagonal diagonalAuxiliar codigo)
  (if (null? diagonal)
      '()
      (if (analizarPosicionVaciaDiagonal matriz (car diagonal))
          (if (equal? codigo "Interfaz")
              diagonalAuxiliar
              (car diagonal))
          (determinarFichaGaneDiagonal matriz (cdr diagonal) diagonalAuxiliar codigo))))
(define (analizarPosicionVaciaDiagonal matriz posicion)
  (if (= (encontrarFichaPorPosicion matriz (car posicion) (cadr posicion) '()) 0)
      #t
      #f))
#|
Función encargada de que la máquina conforme sus líneas, en caso no poder ganar o no tener que colocar fichas para no perder.
Recibe como parámetros las fichasUsuario, el usuario que va a conformar la línea, una lista de posibles posiciones para conformar que se va rellenando recursivamente,
y una lista de candidatos que contiene todas las posiciones vacías dentro de la matriz. Por cada ficha del usuario, esta función agrega nuevas posibles posiciones
por medio de la función actualizarPosicionesConformar, y al ser la lista de fichasUsuario vacía, se llama a la función seleccionarPosicionesConformar,
que determinará en cual de las posibles posiciones hay que conformar la línea.
|# 
(define (conformarLineas matriz fichasUsuario usuario posibles candidatos)
  (if (null? fichasUsuario)
      (seleccionarPosicionConformar matriz posibles (random (length candidatos)) (car candidatos) candidatos)
      (conformarLineas matriz
                       (cdr fichasUsuario)
                       usuario
                       (actualizarPosicionConformar posibles
                                                    (caar fichasUsuario)
                                                    (cadar fichasUsuario)
                                                    (length (car matriz))
                                                    (length matriz)
                                                    candidatos
                                                    #t #t #t #t
                                                    )
                       candidatos
      )
  )
)
#|
Función encargada de determinar en cuales posibles posiciones podría la máquina continuar conformando sus líneas. Recibe como parámetro una
lista de posibles soluciones que cada vez se va llenando, un valor mParaConformar que indica la posición m de la ficha que está siendo analizada,
un valor nParaConformar que indica la posición n de la ficha que está siendo analizada, un valor mMax para verificar si la posible posición excede
el máximo de columnas, un valor nMax para verificar si la posible posición excede el máximo de filas, y las cuatro coordenadas cartesianas N E S O
como banderas que cambiarán de true a false si ya se han revisado las posiciones adyacentes en dichas coordenadas. Esta función se llama a si misma
en el orden N E S O, cambiando en cada iteración los valores de estas banderas a false (para no repetir validaciones), con el fin de revisar cada
“vecino” de las fichas que previamente se han colocado. Para ser tomado en cuenta como un “vecino” válido, y ser colocado dentro de posibles,
se debe cumplir que la posición de dicho “vecino” no exceda a mMax o nMax, además de que su posición m y n debe ser mayor que 0. Una vez que se
evaluan las cuatro coordenadas N E S O, se retorna la lista de posibles.
|# 
(define (actualizarPosicionConformar posibles mParaConformar nParaConformar mMax nMax candidatos N E S O)
  (if (equal? N #t)
      (if (and (> nParaConformar 1) (revisarCandidatos candidatos (append (list nParaConformar) (list (- mParaConformar 1)))))
          (actualizarPosicionConformar (append posibles (list (append (list mParaConformar) (list (- nParaConformar 1)))))
                                       mParaConformar nParaConformar mMax nMax candidatos #f E S O
          )
          (actualizarPosicionConformar posibles mParaConformar nParaConformar mMax nMax candidatos #f E S O)
      )
      (if (equal? E #t)
          (if (and (< mParaConformar mMax) (revisarCandidatos candidatos (append (list (+ mParaConformar 1)) (list nParaConformar))))
              (actualizarPosicionConformar (append posibles (list (append (list (+ mParaConformar 1)) (list nParaConformar))))
                                           mParaConformar nParaConformar mMax nMax candidatos N #f S O
              )
              (actualizarPosicionConformar posibles mParaConformar nParaConformar mMax nMax candidatos N #f S O)
          )
          (if (equal? S #t)
              (if (and (< nParaConformar nMax) (revisarCandidatos candidatos (append (list mParaConformar) (list (+ mParaConformar 1)))))
                  (actualizarPosicionConformar (append posibles (list (append (list mParaConformar) (list (+ mParaConformar 1)))))
                                               mParaConformar nParaConformar mMax nMax candidatos N E #f O
                  )
                  (actualizarPosicionConformar posibles mParaConformar nParaConformar mMax nMax candidatos N E #f O)
              )
              (if (equal? O #t)
                  (if (and (> mParaConformar 1) (revisarCandidatos candidatos (append (list (- mParaConformar 1)) (list nParaConformar))))
                      (actualizarPosicionConformar (append posibles (list (append (list (- mParaConformar 1)) (list nParaConformar))))
                                                   mParaConformar nParaConformar mMax nMax candidatos N E S #f
                      )
                      (actualizarPosicionConformar posibles mParaConformar nParaConformar mMax nMax candidatos N E S #f)
                  )
                  posibles
              )
          )
      )
  )
)
(define (seleccionarPosicionConformar matriz posibles posicion candidatoActual candidatos)
  (if (> posicion 1)
      (seleccionarPosicionConformar matriz posibles (- posicion 1) (car candidatos) (cdr candidatos))
      candidatoActual
  )
)
#|
Función encargada de colocar la ficha correspondiente al caso de conformar línea. Recibe como parámetros a la matriz y a l
a posición de la fichaColocar. Se encarga de formar el mensaje para la interfaz con la forma indicada en el código a continuación.
|# 
(define (colocarFichaConformarLineas matriz fichaColocar)
  (printPrueba(append (list '())
                      (list (append (list (car fichaColocar))
                      (list (cadr fichaColocar))
              )
          )
          (list (placePlayerSymbol (car fichaColocar)
                                     (cadr fichaColocar)
                                     1
                                     (length matriz)
                                     matriz
                                     matriz
                                     '()
                                     2
                )
          )          
  ))
)
#|
Funcion utilizada para revisar la lista de candidatos y determinar si una posicion es valida para colocar una ficha en la matriz. Recibe como
parametros a la lista de candidatos y a la posicion donde se quiere colocar la ficha.
En caso de que la lista de candidatos sea nula, quiere decir que no se encontro antes a la posicion dentro de la lista de candidatos, por lo
que se define como nula la colocacion de dicha fila, y se retorna un false.

En caso de que la lista de candidatos no sea vacia, se comparan las posiciones de la tupla de la primera posicion de candidatos con las
posiciones de la tupla posicion, y si ambas coinciden, se dice que la ficha va a ser colocada en un candidato correcto, que corresponde a un
espacio vacio dentro de la matriz.

Si alguna de las posiciones de la tupla de la primera posicion de candidatos es diferente que las posiciones de la tupla posicion, se retorna
la funcion de manera recursiva, recortando la lista de candidatos, ya que la ficha no pretende ser colocada en dicha primera posicion de
candidatos.
|#
(define (revisarCandidatos candidatos posicion)
  (if (null? candidatos)
      #f
      (if (= (caar candidatos) (car posicion))
          (if (= (cadar candidatos) (cadr posicion))
              #t
              (revisarCandidatos (cdr candidatos) posicion)
          )
          (revisarCandidatos (cdr candidatos) posicion)
       )
  )
)
;----------------------------------------------------------ALGORITMO VORAZ---------------------------------------------------------------------
#|
Funcion que inicia la ejecucion del algoritmo voraz. Es llamada cada vez que la maquina tiene que colocar una ficha, y por medio de las
diversas subfunciones, se determina cual seria la posicion idonea para colocar la ficha, en funcion de las fichas propias de la maquina, y las
colocadas por el usuario. Recibe como parametro a la matriz de juego.
|#
(define (ejecutarAlgoritmoVoraz matriz)
  (display "Ejecutando el algoritmo voraz \n")
  (display matriz)
  (display "\n")
  (funcionDeViabilidad matriz
                      (encontrarConjuntoDeCandidatos matriz)
                      (encontrarFichasColocadas matriz '() 0 1 1 '())
                      (encontrarFichasColocadas matriz '() 0 1 2 '())
  )
)
#|
Primera parte del algoritmo voraz. Este funcion se encarga de encontrar el conjunto de candidatos posibles que pueden contribuir a la solucion,
es decir, las posiciones que pueden ser ocupadas por la nueva ficha a colocar por la maquina. Recibe como parametro a la matriz de juego, y
llama a la funcion explicada anteriormente encontrarFichasColocadas, que retorna una lista con todas las posiciones con elemento 0 (vacias)
dentro de la matriz.
|#
(define (encontrarConjuntoDeCandidatos matriz)
  (encontrarFichasColocadas matriz '() 0 1 0 '())
)

#|
Segunda parte del algoritmo voraz. Esta funcion se encarga de determinar la viabilidad del conjunto de candidatos. Recibe como parametros
a la matriz, la lista de posibles candidatos, las fichas colocadas por el jugador, y las fichas colocadas por la maquina.
|#
(define (funcionDeViabilidad matriz candidatosParaColocar fichasJugador fichasMaquina)
  ;Maquina revisa si puede ganar de forma horizontal
  (if (not (null? (revisarGaneHorizontal fichasMaquina fichasMaquina (length (car matriz)) '() candidatosParaColocar)))
      (printPrueba(funcionSolucion (append (list (append (list (append (list (car (revisarGaneHorizontal fichasMaquina fichasMaquina (length (car matriz)) '() candidatosParaColocar)))
                                                      (list 1)
                                  )
                            )
                            (list (append (list (car (revisarGaneHorizontal fichasMaquina fichasMaquina (length (car matriz)) '() candidatosParaColocar)))
                                          (list (length (car matriz)))
                                  )
                            )      
                    )
              )
              (list (append (list (car (revisarGaneHorizontal fichasMaquina fichasMaquina (length (car matriz)) '() candidatosParaColocar)))
                            (list (cadr (revisarGaneHorizontal fichasMaquina fichasMaquina (length (car matriz)) '() candidatosParaColocar)))
                    )                   
              )
              (list (placePlayerSymbol (car (revisarGaneHorizontal fichasMaquina fichasMaquina (length (car matriz)) '() candidatosParaColocar))
                                         (cadr (revisarGaneHorizontal fichasMaquina fichasMaquina (length (car matriz)) '() candidatosParaColocar))
                                         1
                                         (length matriz)
                                         matriz
                                         matriz
                                         '()
                                         2
                    )
              )
      )))
      ;Maquina revisa si puede ganar de forma vertical
      (if (not (null? (revisarGaneVertical fichasMaquina fichasMaquina (length matriz) '() candidatosParaColocar)))
          (printPrueba(funcionSolucion(append (list (append (list (append (list 1)
                                                          (list (cadr (revisarGaneVertical fichasMaquina fichasMaquina (length matriz) '() candidatosParaColocar)))
                                      )
                                )
                                (list (append (list (length matriz))
                                              (list (cadr (revisarGaneVertical fichasMaquina fichasMaquina (length matriz) '() candidatosParaColocar)))
                                      )    
                                )
                        )
                  )
                  (list (append (list (car (revisarGaneVertical fichasMaquina fichasMaquina (length matriz) '() candidatosParaColocar)))
                                (list (cadr (revisarGaneVertical fichasMaquina fichasMaquina (length matriz) '() candidatosParaColocar)))
                        )
                  )
                  (list (placePlayerSymbol (car (revisarGaneVertical fichasMaquina fichasMaquina (length matriz) '() candidatosParaColocar))
                                             (cadr (revisarGaneVertical fichasMaquina fichasMaquina (length matriz) '() candidatosParaColocar))
                                             1
                                             (length matriz)
                                             matriz
                                             matriz
                                             '()
                                             2 
                        )
                  )        
          )))
          ;Maquina revisa si puede ganar de forma diagonal
          (if (not (null? (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 2 "Logica")))
              (printPrueba(funcionSolucion(append (list (append (list (car (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 2 "Interfaz")))
                                    (list (ultimoElementoLista (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 2 "Interfaz")))
                            )
                      )
                      (list (append (list (car (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 2 "Logica")))
                                    (list (cadr (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 2 "Logica")))
                            )
                      )
                      (list (placePlayerSymbol (car (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 2 "Logica"))
                                                 (cadr (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 2 "Logica"))
                                                 1
                                                 (length matriz)
                                                 matriz
                                                 matriz
                                                 '()
                                                 2
                            )
                     )
              )))
              ;Maquina revisa si puede perder de forma horizontal
              (if (not (null? (revisarGaneHorizontal fichasJugador fichasJugador (length (car matriz)) '() candidatosParaColocar)))
                  (printPrueba(append (list '())
                          (list (append (list (car (revisarGaneHorizontal fichasJugador fichasJugador (length (car matriz)) '() candidatosParaColocar)))
                                        (list (cadr (revisarGaneHorizontal fichasJugador fichasJugador (length (car matriz)) '() candidatosParaColocar)))
                                )
                          )
                          (list (placePlayerSymbol (car (revisarGaneHorizontal fichasJugador fichasJugador (length (car matriz)) '() candidatosParaColocar))
                                                     (cadr (revisarGaneHorizontal fichasJugador fichasJugador (length (car matriz)) '() candidatosParaColocar))
                                                     1
                                                     (length matriz)
                                                     matriz
                                                     matriz
                                                     '()
                                                     2
                                )
                         )
                  ))
                  ;Maquina revisa si puede perder de forma vertical
                  (if (not (null? (revisarGaneVertical fichasJugador fichasJugador (length matriz) '() candidatosParaColocar)))
                      (printPrueba(append (list '())
                              (list (append (list (car (revisarGaneVertical fichasJugador fichasJugador (length matriz) '() candidatosParaColocar)))
                                            (list (cadr (revisarGaneVertical fichasJugador fichasJugador (length matriz) '() candidatosParaColocar)))
                                    )
                              )
                              (list (placePlayerSymbol (car (revisarGaneVertical fichasJugador fichasJugador (length matriz) '() candidatosParaColocar))
                                                 (cadr (revisarGaneVertical fichasJugador fichasJugador (length matriz) '() candidatosParaColocar))
                                                 1
                                                 (length matriz)
                                                 matriz
                                                 matriz
                                                 '()
                                                 2
                                    )
                              )
                      ))
                      ;Maquina revisa si puede perder de forma diagonal
                      (if (not (null? (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 1 "Logica")))
                          (printPrueba(append (list '())
                                  (list (append (list (car (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 1 "Logica")))
                                                (list (cadr (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 1 "Logica")))
                                        )
                                  )
                                  (list (placePlayerSymbol (car (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 1 "Logica"))
                                                             (cadr (revisarGaneDiagonal matriz (diagonalesValidas matriz) '() 1 "Logica"))
                                                             1
                                                             (length matriz)
                                                             matriz
                                                             matriz
                                                             '()
                                                             2
                                        )
                                  )
                          )) 
                          (colocarFichaConformarLineas matriz (conformarLineas matriz fichasJugador 1 '() candidatosParaColocar))
                      )  
                  )
              )           
          )
      )
  )
)




(define (fullMatrix matriz)
  (if (= (length (encontrarFichasColocadas matriz '() 0 1 0 '())) 0)
      #t
      #f
  )
)

(define (printPrueba mensaje)
  (display "--------------------------------------------------------------------------------------------------------")
  (display "\n")
  (display "El mensaje de este turno es: ") 
  (display mensaje)
  (display "\n")
  (display "---------------------------------------------------------------------------------------------------------")
  (display "\n")
  mensaje
  )

(define (funcionSolucion mensaje)
  (display "SOLUCIÓN GLOBAL CONSEGUIDA: La máquina ha conseguido dar con la solución global: ")
  (display (car mensaje))
  (display "\n")
  mensaje
  )





;(TTT 4 4)
(provide (all-defined-out))