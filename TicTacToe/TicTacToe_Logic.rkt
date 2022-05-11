#lang racket

;-------------------------------------------------------- Game start and matrix generation ----------------------------------------------------------------------

;;Function that validates the value of te column and row (between 3 and 10 for both), if it applies calls the createMatrix function
;;Receives: n (rows), m (columns)
;;Returns: Call for another function, message if failed.
;;Restrictions: Must be integers. 
(define (GenerateLogicMatrix n m)
    (cond ((and (>= m 3) (<= m 10) (>= n 3) (<= n 10)) (createMatrix n m '()))
    (else (display "The values given were incorrect."))))
              
#|
Function that creates the matrix recursively, for every column specified by the user, it adds a row with size n using createRow.
Receives: n (rows), m (columns)
Returns: Call for another function, message if failed.
Restrictions: Must be integers.
|#
(define (createMatrix n m matrix)
  (if (> m 0)(createMatrix n (- m 1) (append matrix (list(createRow n '()))))matrix))
#|
Function that creates the rows for the matrix, called for every column in the matrix, and recursively adds a 0 for every row in the matrix, so that it ends up creating a n*m matrix full of 0 (value set as empty tile)
|#
(define (createRow n row)
  (if (> n 0)(createRow (- n 1) (append row '(0)))row))

;----------------------------------------------------- Turn management -----------------------------------------------------------------------
#|
Function in charge of completing the movement made by the player. First checks that the values of m and n do not surpass the maximum dimensions of the matrix (initially stated by player).
In case it surpasses the maximum it notifies as an error. Then checks if the position where the player wants to place the tile is not occupied already, if it is, it provides an error notification.
In case none of these cases apply, and the tile is available, it calls the placePlayerSymbol function.
Receives: matrix, m (column), n (row), nMax (maximum rows for the matrix), mMax (maximum columns for the matrix)
|#
(define (executePlayerSelec matrix m n mMax nMax)
  (if (> m mMax)(errorNotification 1 matrix mMax nMax)
  (if (> n nMax)(errorNotification 2 matrix mMax nMax)          
  (if (= (findTile matrix m n '()) 0)(placePlayerSymbol m n 1 mMax matrix matrix '() 1)(errorNotification 3 matrix mMax nMax)))))  
#|
Function that places de X tile, first verifies if the m position is equal to the counter. If yes, means the current recursive call is equal to the correct column position, so it places it on that column and on the specified row.
To do this, it recursively calls placePlayerSymbol, so that it shortens the oldMatrix by aplying cdr and appends the columns with the symbol placed in the correct n position into the newMatrix.
If the counter is different from m (column), means we are not in the correct column, so it calls itself again, adding +1 to thecounter and applying cdr to the matrix. Also appends the current column to the newMatrix.
Lastly, for the end case, is when counter is greater than mMax, in this moment the entire matrix has had been traversed, reconstructed and updated from the oldMatrix, once this happens, it calls updateMatrix to update the current one.
Receives: m (column position), n (row position), counter (starts as 1), mMax (maximum columns of the matrix), oldMatrix (matrix to be added the tile), newMatrix (matrix to be formed from oldMatrix) and element (designated value for player = 1)
|#
(define (placePlayerSymbol m n counter mMax oldMatrix matrix newMatrix element)
  (if (= m counter)(placePlayerSymbol m n (+ counter 1) mMax oldMatrix (cdr matrix)(append newMatrix (list(placeTileInRow n 1 (length (car matrix)) (car matrix) '() element)))element)
  (if (>= m counter)(placePlayerSymbol m n (+ counter 1) mMax oldMatrix (cdr matrix)(append newMatrix (list(car matrix)))element)
  (if (<= counter mMax)(placePlayerSymbol m n (+ counter 1) mMax oldMatrix (cdr matrix)(append newMatrix (list(car matrix)))element)(updateMatrix oldMatrix newMatrix m n element)))))
#|
Function that places the symbol in the specified column, obtained from placePlayerSymbol. Works similar to placePlayerSymbol, as it reconstructs the new row from the old row recursively until the counter is the same as the
specified value for n. When this happens the value is swapped for the element. The stop condition is when the counter is greater than the nMax, in this moment we know the row is correctly reconstructed and traversed. In this
moment it returns the updated row to the placePlayerSymbol function, so it can continue to reconstruct the matrix.
Receives: nPos (position inside the row to place the symbol), counter (starts as 1), nMax (total dimension of the row), row (original row), newRow (row to be updated when added the element)and element (1 as designated)
|#
(define (placeTileInRow nPos counter nMax row newRow element)
  (if (= nPos counter)(placeTileInRow nPos(+ counter 1) nMax (cdr row) (append newRow(list element)) element)
  (if (>= nPos counter)(placeTileInRow nPos(+ counter 1) nMax (cdr row) (append newRow (list(car row))) element)
  (if (<= counter nMax)(placeTileInRow nPos (+ counter 1) nMax (cdr row) (append newRow (list(car row))) element) newRow))))

(define (printCheck message)
  (display "--------------------------------------------------------------------------------------------------------")
  (display "\n")
  (display "El message de este turno es: ") 
  (display message)
  (display "\n")
  (display "---------------------------------------------------------------------------------------------------------")
  (display "\n")
  message
  )

(define (solution message)
  (display "SOLUCIÓN GLOBAL CONSEGUIDA: La máquina ha conseguido dar con la solución global: ")
  (display (car message))
  (display "\n")
  message
  )
#|
Function that shows the user the updated matrix (in console) for control. Also, it returns the matrix to the gameloop so it can work again after the user used his turn. Works as the loop for the game, and status checks. 
Receives: matrix (old matrix to take into consideration), newMatrix (matrix to be reconstructed from the old one), mPos (column value), nPos (row value), element (element to be added)
|#
(define (updateMatrix matrix newMatrix mPos nPos element)
  (display "Checking...")
  (display (append (list mPos) (list nPos)))
  (display (checkDiagonalWin matrix (validDiagonals matrix) '() 1 "Logic"))
  (display matrix)
  (display "\n")
  (if (= element 1)(if (equal? (checkHorizontalWin (findPlacedTiles matrix '() 0 1 1 '()) (findPlacedTiles matrix '() 0 1 1 '()) (length (car matrix)) '() (findPlacedTiles matrix '() 0 1 0 '()))
                                (append (list mPos) (list nPos)))
          (printCheck(append (list (append (list (append (list (car (checkHorizontalWin (findPlacedTiles matrix '() 0 1 1 '())
                                                                                (findPlacedTiles matrix '() 0 1 1 '())
                                                                                (length (car matrix))
                                                                                '()
                                                                                (findPlacedTiles matrix '() 0 1 0 '())))) (list 1) ))
                                            (list (append (list (car (checkHorizontalWin (findPlacedTiles matrix '() 0 1 1 '())
                                                                                (findPlacedTiles matrix '() 0 1 1 '())
                                                                                (length (car matrix))
                                                                                '()
                                                                                (findPlacedTiles matrix '() 0 1 0 '()))))(list (length (car matrix)))))))
                              (list (append (list mPos) (list nPos)))
                              (list newMatrix)))
          (if (equal? (checkVerticalWin (findPlacedTiles matrix '() 0 1 1 '()) (findPlacedTiles matrix '() 0 1 1 '()) (length matrix) '() (findPlacedTiles matrix '() 0 1 0 '()))
                      (list (append (list mPos) (list nPos))))
          (printCheck(append (list (append (list (append (list (cadr (checkVerticalWin (findPlacedTiles matrix '() 0 1 1 '())
                                                                                   (findPlacedTiles matrix '() 0 1 1 '())
                                                                                   (length matrix)'()
                                                                                   (findPlacedTiles matrix '() 0 1 0 '()))))(list 1)))
                                    (list (append (list (cadr (checkVerticalWin (findPlacedTiles matrix '() 0 1 1 '())
                                                                                   (findPlacedTiles matrix '() 0 1 1 '())
                                                                                   (length matrix)'()
                                                                                   (findPlacedTiles matrix '() 0 1 0 '()))))(list (length matrix))))))
                      (list (append (list mPos) (list nPos)))
                      (list newMatrix)))
              (if (equal? (checkDiagonalWin matrix (validDiagonals matrix) '() 1 "Logic") (append (list mPos) (list nPos)))
                  (printCheck(append (list (append (list (car (checkDiagonalWin matrix (validDiagonals matrix) '()  1  "GUI")))
                                                    (list (listLastElement (checkDiagonalWin matrix (validDiagonals matrix) '()  1  "GUI")))))
                          (list (append (list mPos) (list nPos)))
                          (list newMatrix)))
                  (printCheck(append (list '())
                          (list (append (list mPos) (list nPos)))
                          (list newMatrix)))))) newMatrix))          
#|
Function that notifies the user if there is any error, if selected m or n is greater than maximum, user will be notified. errType 1 is when m exceed the maximum,
errType 2 is when n exceeds the maximum and the last errType is when the selected tile is already occupied.
Receives: errType (numcoded for management), matrix (matrix tobe worked on), nMax and mMax (dimensions of the matrix).
|#
(define (errorNotification errType matrix mMax nMax)
  (if (= errType 1)(display "Column position selected is not valid for the matrix dimensions \n")
  (if (= errType 2)(display "Row position selected is not valid for the matrix dimensions \n")
      (display "The position selected is already occupied by another symbol \n")))
  (executePlayerSelec matrix (string->number (read-line)) (string->number (read-line)) mMax nMax))

;-------------------------------------------------------- Traversing the matrix --------------------------------------------------------------------
#|
Function that finds all the positions (m n) that contains 1 (player value), 2 (bot value) or 0 (empty value).
Base case: when both currentm (current column) and the matrix are null (all elements were analyzed). If  this is the case, it calls updatePositions to notify the user.
If only currentm is null, means all positions from that column have been checked, so it calls itself with the cdr of the matrix, the new currentm will be the car of the matrix, the mCounter will be added by 1, nCounter will be
restored to 1 but keeps the element as well as the positions found.
If currentm is not null, means there are still elements to be checked in the column. Checks if the car of currentm is equal to the element, if it is, calls itself recursively, changing currentm to cdr of currentm, adding up nCounter
and adding those positions as a tuple, if is not equal, does the same without adding the positions.
Receives: matrix (current matrix), currentm (column that is being checked), mCounter (counter to save m value if element is found), nCounter (counter to save n value if element is found), element (to be found), positions (list)
|#
(define (findPlacedTiles matrix currentm mCounter nCounter element positions)
  (if (null? currentm)
      (if (null? matrix)
          (updatePositions positions element)(findPlacedTiles (cdr matrix) (car matrix) (+ mCounter 1) 1 element positions))
      (if (equal? (car currentm) element)
          (findPlacedTiles matrix (cdr currentm) mCounter (+ nCounter 1) element (append positions (list (list mCounter nCounter))))                                                                          
          (findPlacedTiles matrix (cdr currentm) mCounter (+ nCounter 1) element positions))))
#|
Function that finds the value of the tile in an specified position. Works similar to findPlacedTiles.
If currentm is null, means the element is not in that column. So recursively calls itself again, with the cdr of the matrix as the matrix and car of the matrix as currentm.
If currentm is not null, but mPos is greater than 1, means that mPos is further in the matrix, so recursively calls it self with cdr of the matrix, mPos - 1 and with currentm as the car of the matrix.
If currentm is not null, and is equal to 1, means that it is placed in the correct column, so it repeats the process changing the row value till found.
If mPos and nPos are equal to 1, means it is placed on the wanted tile, so it hits the base case and returns the car of currentm (containing the element inside the selected position).
Receives: matrix (current matrix), nPos (row position), mPos (column position) and currentm (column current being checked)
|#
(define (findTile matrix mPos nPos currentm)
  (if (null? currentm)
      (findTile (cdr matrix) mPos nPos (car matrix))
  (if (> mPos 1)
      (findTile (cdr matrix) (- mPos 1) nPos (car matrix))
  (if (> nPos 1)
      (findTile matrix mPos (- nPos 1) (cdr currentm))
      (car currentm)))))                                  
#|
Function that notifies the user of the position of their tiles and the bot's. If element is 1, means it's user's. If element is 2, means it's bot's. If element is 0, notifies user of the blank spaces in the matrix, then
shows the position list on the console.
Receives: positions (list with tuples of the positions), element (can be either 1, 2 or 0.
|#
(define (updatePositions positions element)
  (if (= element 1)
      (display "Next turn is for the player")
  (if (= element 2)
      (display "Next turn is for the bot")
      (display "Posible candidates to place are:")))
      (display positions) (display "\n") positions)
#|
Function that returns the last element of a list
|#
(define (listLastElement lista)
  (if (null? (cdr lista))
      (car lista)
      (listLastElement (cdr lista))))
;-------------------------------------------------------- AI's turn -------------------------------------------------------------------
#|
Start Function of the greedy algorithm. It places the bot's tile. This function is used for an organized flow of the turns, alternating between the player's function and the bot's function.
Receives: matrix, element. 
|#
(define (placeBotSymbol matrix element)
  (runGreedyAlgorithm matrix))
;-------------------------------------------------------- Horizontal Win Check ---------------------------------------------------------------------------------------------------
#|
Function that verifies if there is a horizontal win, either from the player or the bot. First it validates that botTilesAux is not null, if it is, means there was not a valid solution in all of the tiles placed by the user and
the bot, so it returns a null tileToWin. If it is not null, it checks if the tile to win is null, if it is, means there is no correct solution between the possible horizontal lines, so it calls itselft recursively, aplying cdr
to the tile list and updating the posible tile to win calling the function tileToWinHorizontaly.
If tileToWin is not null, means it found a horizontal win solution, and returns that tile, so the program doesn't have to verify the rest of the tiles placed.
Receives: botTiles (used to compare), botTilesAux (used to traverse the list (applying cdr)), mToWin (required value of the horizontal line to be considered a win), tiletoWin (tuple), candidates (candidate list, called in to use in other functions).
|#
(define (checkHorizontalWin botTiles botTilesAux mToWin tileToWin candidates)
  (if (null? botTilesAux)
      tileToWin
  (if (null? tileToWin)
      (checkHorizontalWin botTiles (cdr botTilesAux) mToWin (tileToWinHorizontaly botTiles (car botTilesAux) mToWin '() candidates) candidates)
      tileToWin)))
#|
Function that analizes every tile placed by the bot or the user and compares them with the list of all the tiles placed by it. First as the base case, it validates that the botTiles list is null, if it is, means all the posible
 tiles had been checked and compared to tileToCheck, so the quantity of the present tiles in the row is one position away from the mToWin, so it can deduce that in this row there is a possible win. If this happens it calls the
function determinateTileToWinHorizontaly, that determines which is the tile missing in that row to win. If there is more than one tile left to win, means that either the player already has tiles on that row, or that there is more
than one blank tile to fill, if this happens, it returns a null list to show there is no valid solution for that row.
If the botTiles is not null, means that there might be tiles that can provide a win, so it checks that the caar of the botTiles, is the same as the car of the tileToCheck, meaning that the row of the tileToCheck is the same as the
row row of the current tile in the list. So it calls itselft recursively, applying cdr to botTiles, and appending the cdar of the botTiles to the position list. This means that inside that n row, there is another position in m
(column) that contains the placed tile.
If they are not the same, it only calls itself aplying cdr to botTiles so it can check the next row.
 Receives: botTiles (list of placed tiles), tileToCheck (tile to analize), mToWin (quantity of tiles needed in that row to win), positions (recursive param with the position of the tiles placed on that row) and candidates (also posible candidates).
|#
(define (tileToWinHorizontaly botTiles tileToCheck mToWin positions candidates)
  (if (null? botTiles)
      (if (= (+ (length positions) 1) mToWin)
          (determinateTileToWinHorizontaly positions tileToCheck 1 candidates) '())
      (if (= (caar botTiles) (car tileToCheck))
          (tileToWinHorizontaly (cdr botTiles) tileToCheck mToWin (append positions (cdar botTiles)) candidates)
          (tileToWinHorizontaly (cdr botTiles) tileToCheck mToWin positions candidates))))
#|
Last function in the process of horizontal win check. Only reachable if the program previously determined that the quantity of tiles of the same player are one away from the needed to win (m dimension).
If positions is null, means it reached the end of posible positions to analize inside the row, so it returns the position of the winning tile ( n position of tileToCheck, counter).
If positions is not null, checks if counter is equal to the first element of the list, if it is, means that the missing m position is not the current one. Meaning the tile to win is not present in the current recursion, so it calls
itseft again, shrinking the positions list and adding 1 to the counter.
If counter is different from the first element, means that the counter is the m position where the tile has to be placed inside the row. If found a discrepancy between counter and the first element of positions, it returns the value
of ( n position of tileToCheck, counter).
Tile placement only occurs if the position given by (tileToCheck, counter) is inside the list of candidates, so it calls checkCandidates to see if it is valid to place the tile or not.
Receives: positions (list of m positions that contains tiles), tileToCheck (the tile to be checked), counter (recursive auxiliar parameter), candidates (to check if the winning tile is inside this list)
|#
(define (determinateTileToWinHorizontaly positions tileToCheck counter candidates)
  (if (null? positions)
      (if (checkCandidates candidates (append (list (car tileToCheck)) (list counter)))
          (append (list (car tileToCheck)) (list counter)) '())
      (if (= counter (car positions))
          (determinateTileToWinHorizontaly (cdr positions) tileToCheck (+ counter 1) candidates)
          (if (checkCandidates candidates (append (list (car tileToCheck)) (list counter)))
              (append (list (car tileToCheck)) (list counter))
              '()))))
;;---------------------------------------------------------- Vertical Win Check -------------------------------------------------------------------------------
#|
Function that works the same as checkHorizontalWin, also made up by three different functions. First it traverse botTilesAux recursively until it becomes null, and keeps looking for a tile
to win by calling tileToWinVerticaly.
Receives: botTiles (list to check), botTilesAux (list to be reconstructed), nToWin (quantity of tiles needed placed on the same column), tileToWin (the tile currently being checked), candidates (list of candidate tiles)
|#
(define (checkVerticalWin botTiles botTilesAux nToWin tileToWin candidates)
  (if (null? botTilesAux)
      tileToWin
      (if (null? tileToWin)
          (checkVerticalWin botTiles (cdr botTilesAux) nToWin (tileToWinVerticaly botTiles (car botTilesAux) nToWin '() candidates) candidates)
          tileToWin)))
#|
Function that tries to find the tile that lets a win for every tile placed by the user or the bot. Traverses the botTiles list recursively until it is null, in this moment, it validates if the quantity of present tiles in a
determined column is one away from the needed account to win. If this applies, it calls the function determinateTileToWinVerticaly, to find the tile that the player needs to place to win.
If botTiles is not null, it will continue comparing m values with the one being checked in the current recursion, so it can check how many tiles  are present in the column, and so it can decide if there is a tile that can win the game.
Receives: botTiles (list of placed tiles), tileToCheck (tile to analize), nToWin (quantity of tiles needed in that column to win), positions (recursive param with the position of the tiles placed on that column) and candidates (also posible
|#
(define (tileToWinVerticaly botTiles tileToCheck nToWin positions candidates)
  (if (null? botTiles)
      (if (= (+ (length positions) 1) nToWin)
          (determinateTileToWinVerticaly positions tileToCheck 1 candidates)
          '())
      (if (= (length botTiles) 1)
          (if (= (cadar botTiles) (cadr tileToCheck))
              (tileToWinVerticaly (cdr botTiles) tileToCheck nToWin (append positions (list (caar botTiles))) candidates)
              (tileToWinVerticaly (cdr botTiles) tileToCheck nToWin positions candidates))                         
          (if (= (cadar botTiles) (cadr tileToCheck))
              (tileToWinVerticaly (cdr botTiles) tileToCheck nToWin (append positions (list (caar botTiles))) candidates)
              (tileToWinVerticaly (cdr botTiles) tileToCheck nToWin positions candidates)))))
#|
Function that determines the tile needed for the player or the bot to win vertically. If this function is called, means either the user or the bot are one position away from winning in one of the columns, checks if that tile to
win is present in the candidates list, if it is the player can win with it. Works the same as determinateTileToWinHorizontaly
|#
(define (determinateTileToWinVerticaly positions tileToCheck counter candidates)
  (if (null? positions)
      (if (checkCandidates candidates (append (list counter) (list (cadr tileToCheck))))
          (append (list counter) (list (cadr tileToCheck)))
          '())
      (if (= counter (car positions))
          (determinateTileToWinVerticaly (cdr positions) tileToCheck (+ counter 1) candidates)
          (if (checkCandidates candidates (append (list counter) (list (cadr tileToCheck))))
              (append (list counter) (list (cadr tileToCheck))) '()))))

;;---------------------------------------------------------- Diagonal Win Check -------------------------------------------------------------------------------
#|
Función que se encarga de retornar todas las diagonals válidas que podrían generar un gane dentro de la matrix de juego. Recibe como parámetro a la matrix.
Se compone de cuatro subfunciones que encuentran las diagonals de tipo descendentes y ascendentes, en dos variantes denominadas “diagonals verticales” y “diagonals horizontales”.
|#
(define (validDiagonals matrix)
  (append (topDownVerticalDiagonals (length (car matrix)) (length matrix) 1 (length matrix) (length matrix) '() '())
          (topDownHorizontalDiagonals (length (car matrix)) (length matrix) 2 1 2 '() '())
          (discardDiagonals  (upwardVerticalDiagonals (length (car matrix)) (length matrix) (length (car matrix)) (length matrix) (length matrix) '() '()) '())
          (discardDiagonals  (upwardHorizontalDiagonals (length (car matrix)) (length matrix) (- (length (car matrix)) 1) 1 (- (length (car matrix)) 1) '() '()) '())))
#|
Función encargada de identificar las diagonals de tipo ascendente vertical que se encuentran dentro de la matrix.
Recibe como parámetros a la amountSpaces de columnas mMax, la amountSpaces de filas nMax, un contador de la fila nActual, un contador de la fila donde comenzó
la diagonal actual start_n, la currentDiagonal que se va conformando de manera recursiva, y la lista de diagonals que este tipo que pueden ser formadas.
Esta función va recorriendo, por cada start_n (que inicia en nMax y termina en 0) el diagonal que se puede formar hasta llegar a un punto donde no queden
positiones que agregar al currentDiagonal. Lleva un recuento del nActual y mActual, y en el momento en que ambas superen a nMax y mMax respectivamente,
se sabe que no quedan positiones en diagonal que agregar al currentDiagonal, por lo que se procede a repetir el proceso, pero reduciendo la fila donde
empieza el diagonal start_n, con el fin de considerar todos los diagonals de tipo ascendente vertical.
|#  
(define (upwardVerticalDiagonals mMax nMax mActual nActual start_n currentDiagonal diagonals)
  (if (= start_n 0) diagonals
      (if (= nActual nMax)
          (upwardVerticalDiagonals mMax
                                           nMax
                                           mMax
                                           (- start_n 1)
                                           (- start_n 1)
                                           '()
                                           (append diagonals (list (append currentDiagonal (list (append (list nActual) (list mActual)))))))
          (if (= mActual 1)
              (upwardVerticalDiagonals mMax
                                               nMax
                                               mMax
                                               (- start_n 1)
                                               (- start_n 1)
                                               '()
                                               (append diagonals (list (append currentDiagonal (list (append (list nActual) (list mActual)))))))
              (upwardVerticalDiagonals mMax
                                               nMax
                                               (- mActual 1)
                                               (+ nActual 1)
                                               start_n
                                               (append currentDiagonal (list (append (list nActual) (list mActual))))
                                                diagonals)))))
#|
Función encargada de identificar las diagonals de tipo descendente vertical que se encuentran dentro de la matrix.
Recibe como parámetros a la amountSpaces de columnas mMax, la amountSpaces de filas nMax, un contador que indica el mActual,
un contador que indica el nActual, un contador start_m que indica la columna donde inició el diagonal actual, el
currentDiagonal y la lista de diagonals de tipo descendentes verticales. De igual manera que la función anterior,
va analizando cada diagonal hasta que se llega a que start_m es mayor a mMax, por lo que se dice que no quedan diagonals
de tipo descendente vertical.
|#  
(define (upwardHorizontalDiagonals mMax nMax mActual nActual start_m currentDiagonal diagonals)
  (if (= start_m 0)
      diagonals
      (if (= nActual nMax)
          (upwardHorizontalDiagonals mMax
                                             nMax
                                             (- start_m 1)
                                             1
                                             (- start_m 1)
                                             '()
                                             (append diagonals (list (append currentDiagonal (list (append (list nActual) (list mActual)))))))
          (if (= mActual 1)
              (upwardHorizontalDiagonals mMax
                                                 nMax
                                                 (- start_m 1)
                                                 1
                                                 (- start_m 1)
                                                 '()
                                                 (append diagonals (list (append currentDiagonal (list (append (list nActual) (list mActual)))))))
              (upwardHorizontalDiagonals mMax
                                                 nMax
                                                 (- mActual 1)
                                                 (+ nActual 1)
                                                 start_m
                                                 (append currentDiagonal (list (append (list nActual) (list mActual))))
                                                 diagonals)))))
                                                                                  
(define (topDownVerticalDiagonals mMax nMax mActual nActual start_n currentDiagonal diagonals)
  (if (= start_n 0)
      diagonals
      (if (= (- nActual 1) nMax)
          (if (> (length currentDiagonal) 2)
              (topDownVerticalDiagonals mMax
                                                nMax
                                                1
                                                (- start_n 1)
                                                (- start_n 1)
                                                '()
                                                (append diagonals (list currentDiagonal))
              )
              (topDownVerticalDiagonals mMax
                                                nMax
                                                1
                                                (- start_n 1)
                                                (- start_n 1)
                                                '()
                                                diagonals
              )
          )
          (if (<= mActual mMax)    
              (topDownVerticalDiagonals mMax
                                                nMax
                                                (+ mActual 1)
                                                (+ nActual 1)
                                                start_n
                                                (append currentDiagonal (list (append (list nActual) (list mActual))))
                                                diagonals
              )
              (topDownVerticalDiagonals mMax
                                                nMax
                                                (+ mActual 1)
                                                (+ nActual 1)
                                                start_n
                                                currentDiagonal
                                                diagonals )))))

(define (topDownHorizontalDiagonals mMax nMax mActual nActual start_m currentDiagonal diagonals)
  (if (> start_m mMax)
      diagonals
      (if (> nActual nMax)
          (if (> (length currentDiagonal) 2)
              (topDownHorizontalDiagonals mMax
                                                  nMax
                                                  (+ start_m 1)
                                                  1
                                                  (+ start_m 1)
                                                  '()
                                                  (append diagonals (list currentDiagonal)))
              (topDownHorizontalDiagonals mMax
                                                  nMax
                                                  (+ start_m 1)
                                                  1
                                                  (+ start_m 1)
                                                  '()
                                                  diagonals))
          (if (> mActual mMax)
              (if (> (length currentDiagonal) 2)
                  (topDownHorizontalDiagonals mMax
                                                      nMax
                                                      (+ start_m 1)
                                                      1
                                                      (+ start_m 1)
                                                      '()
                                                      (append diagonals (list currentDiagonal)))
                  (topDownHorizontalDiagonals mMax
                                                      nMax
                                                      (+ start_m 1)
                                                      1
                                                      (+ start_m 1)
                                                      '()
                                                      diagonals))
              (topDownHorizontalDiagonals mMax
                                                  nMax
                                                  (+ mActual 1)
                                                  (+ nActual 1)
                                                  start_m
                                                  (append currentDiagonal (list (append (list nActual) (list mActual))))
                                                  diagonals)))))
#|
Función que recibe como parámetros una lista de diagonals y una lista vacía de validDiagonals, y por medio de
llamadas recursivas y comparaciones de cada diagonal, se determina cuales tienen una longitud mayor o igual a tres,
por lo que pueden ser consideradas validDiagonals. Al analizar cada diagonal de diagonals, retorna la lista de validDiagonals.
|# 
(define (discardDiagonals diagonal_list validDiagonals)
  (if (null? diagonal_list)
      validDiagonals
      (if (>= (length (car diagonal_list)) 3)
          (discardDiagonals (cdr diagonal_list) (append validDiagonals (list (car diagonal_list))))
          (discardDiagonals (cdr diagonal_list) validDiagonals))))
#|
Función encargada de analizar si existe una posición en la cual, en caso de colocar una ficha, se produzca un gane para el jugador.
Recibe como parámetros a la matrix, una lista de diagonals válidos, la posible flag_win (que por defecto es nula), el player
(jugador o maquina) que quiere revisar si existe una posición para ganar y un código de tipo string que se utiliza más adelante
para saber si la información requerida es para la parte lógica o la parte de interfaz. De igual manera que con checkHorizontalWin
y revisarGaneVertical, se llama esta función de forma recursiva hasta que checkDiagonal retorne una posición que no sea nula,
o se acaben los possibles diagonals.
|# 
(define (checkDiagonalWin matrix diagonals_list flag_win player code)
  (if (null? diagonals_list)
      flag_win
      (if (null? flag_win)
          (checkDiagonalWin matrix
                               (cdr diagonals_list)
                               (checkDiagonal matrix player (car diagonals_list) (car diagonals_list) 0 code)
                               player
                               code)
          flag_win)))
#|
Función encargada de analizar cada diagonal para determinar si existe un posible gane dentro de la misma. Recibe como parámetros a la matrix,
al player que quiere verificar si puede ganar, el diagonal a analizar, una copia de la diagonal a analizar auxDiagonal, la amountSpaces de
fichas que hay en el diagonal, y el mismo código de la función anterior. Por cada posición dentro del diagonal, y en caso de que la función
checkDiagonalPos retorne un true, se suma un 1 a la amountSpaces, por lo que, al terminarse las positiones de diagonal, y si la amountSpaces
está a una unidad de la longitud de auxDiagonal, se retorna el auxDiagonal o la posición para ganar proveniente de
determinarFichaParaGanarDiagonal, en función de si el código es “Interfaz” o “Lógica”. El motivo por el cual se utiliza el código es porque,
para la interfaz se tienen que enviar solo la posición inicial y final de la diagonal (con el fin de dibujar la línea del gane), y para la lógica
se tiene que retornar únicamente la posición que permita el gane.
|# 
(define (checkDiagonal matrix player diagonal auxDiagonal amountSpaces code)
  (if (null? diagonal)
      (if (= (+ amountSpaces 1) (length auxDiagonal))
          (findWinTileDiagonal matrix auxDiagonal auxDiagonal code)
          '())
      (checkDiagonal matrix
                        player
                        (cdr diagonal)
                        auxDiagonal
                        (+ amountSpaces (checkDiagonalPos matrix player (car diagonal)))
                        code)))
#|
Función que recibe como parámetros a la matrix, el player que se busca y la posición en la que se busca dentro de la matrix.
Se utilizar a la función encontrarFichaPorposition para determinar si en esa posición específica existe una ficha de valor player (1 o 2).
De ser así retorna un 1 que es sumado a la amountSpaces de la función anterior.
|# 
(define (checkDiagonalPos matrix player position)
  (if (= (findTile matrix (car position) (cadr position) '()) player)
      1
      0))
#|
Función utilizada para determinar cuál es la posición donde hace falta una ficha dentro del diagonal. Recibe como parámetros a la matrix y a la diagonal.
Por cada element posición dentro de diagonal, se verifica si analizeEmptyDiagonalPos retorna un true. De ser así, quiere decir que la posición actual
dentro de la diagonal es vacía, por lo que se retorna dicha posición. De no ser así, se procede con la siguiente posición de la diagonal, hasta que la misma sea nula.
|# 
(define (findWinTileDiagonal matrix diagonal auxDiagonal code)
  (if (null? diagonal)
      '()
      (if (analizeEmptyDiagonalPos matrix (car diagonal))
          (if (equal? code "Interfaz")
              auxDiagonal
              (car diagonal))
          (findWinTileDiagonal matrix (cdr diagonal) auxDiagonal code))))
(define (analizeEmptyDiagonalPos matrix position)
  (if (= (findTile matrix (car position) (cadr position) '()) 0)
      #t
      #f))
#|
Función encargada de que la máquina conforme sus líneas, en caso no poder ganar o no tener que colocar fichas para no perder.
Recibe como parámetros las player_tiles, el player que va a conformar la línea, una lista de possibles positiones para conformar que se va rellenando recursivamente,
y una lista de candidates que contiene todas las positiones vacías dentro de la matrix. Por cada ficha del player, esta función agrega nuevas possibles positiones
por medio de la función actualizarpositionesConformar, y al ser la lista de player_tiles vacía, se llama a la función seleccionarpositionesConformar,
que determinará en cual de las possibles positiones hay que conformar la línea.
|# 
(define (formLines matrix player_tiles player possibles candidates)
  (if (null? player_tiles)
      (selectPositon_form matrix possibles (random (length candidates)) (car candidates) candidates)
      (formLines matrix
                       (cdr player_tiles)
                       player
                       (formLines_aux possibles (caar player_tiles) (cadar player_tiles) 
                                                    (length (car matrix)) (length matrix) candidates #t #t #t #t)
                       candidates)))
#|
Función encargada de determinar en cuales possibles positiones podría la máquina continuar conformando sus líneas. Recibe como parámetro una
lista de possibles soluciones que cada vez se va llenando, un valor m_toForm que indica la posición m de la ficha que está siendo analizada,
un valor n_toForm que indica la posición n de la ficha que está siendo analizada, un valor mMax para verificar si la posible posición excede
el máximo de columnas, un valor nMax para verificar si la posible posición excede el máximo de filas, y las cuatro coordenadas cartesianas N E S O
como banderas que cambiarán de true a false si ya se han revisado las positiones adyacentes en dichas coordenadas. Esta función se llama a si misma
en el orden N E S O, cambiando en cada iteración los valores de estas banderas a false (para no repetir validaciones), con el fin de revisar cada
“vecino” de las fichas que previamente se han colocado. Para ser tomado en cuenta como un “vecino” válido, y ser colocado dentro de possibles,
se debe cumplir que la posición de dicho “vecino” no exceda a mMax o nMax, además de que su posición m y n debe ser mayor que 0. Una vez que se
evaluan las cuatro coordenadas up right down left, se retorna la lista de possibles.
|# 
(define (formLines_aux possibles m_toForm n_toForm mMax nMax candidates up right down left)
  (if (equal? up #t)
      (if (and (> n_toForm 1) (checkCandidates candidates (append (list n_toForm) (list (- m_toForm 1)))))
          (formLines_aux (append possibles (list (append (list m_toForm) (list (- n_toForm 1)))))
                                       m_toForm n_toForm mMax nMax candidates #f right down left)
          (formLines_aux possibles m_toForm n_toForm mMax nMax candidates #f right down left))
      (if (equal? right #t)
          (if (and (< m_toForm mMax) (checkCandidates candidates (append (list (+ m_toForm 1)) (list n_toForm))))
              (formLines_aux (append possibles (list (append (list (+ m_toForm 1)) (list n_toForm))))
                                           m_toForm n_toForm mMax nMax candidates up #f down left)
              (formLines_aux possibles m_toForm n_toForm mMax nMax candidates up #f down left))
          (if (equal? down #t)
              (if (and (< n_toForm nMax) (checkCandidates candidates (append (list m_toForm) (list (+ m_toForm 1)))))
                  (formLines_aux (append possibles (list (append (list m_toForm) (list (+ m_toForm 1)))))
                                               m_toForm n_toForm mMax nMax candidates up right #f left)
                  (formLines_aux possibles m_toForm n_toForm mMax nMax candidates up right #f left))
              (if (equal? left #t)
                  (if (and (> m_toForm 1) (checkCandidates candidates (append (list (- m_toForm 1)) (list n_toForm))))
                      (formLines_aux (append possibles (list (append (list (- m_toForm 1)) (list n_toForm))))
                                                   m_toForm n_toForm mMax nMax candidates up right down #f)
                      (formLines_aux possibles m_toForm n_toForm mMax nMax candidates up right down #f))
                  possibles )))))
(define (selectPositon_form matrix possibles position actualCandidate candidates)
  (if (> position 1)
      (selectPositon_form matrix possibles (- position 1) (car candidates) (cdr candidates))
      actualCandidate ))
#|
Función encargada de colocar la ficha correspondiente al caso de conformar línea. Recibe como parámetros a la matrix y a l
a posición de la tileToPlace. Se encarga de formar el message para la interfaz con la forma indicada en el código a continuación.
|# 
(define (placeTilesFormLines matrix tileToPlace)
  (printCheck(append (list '())
                      (list (append (list (car tileToPlace))
                      (list (cadr tileToPlace))))
          (list (placePlayerSymbol (car tileToPlace)
                                     (cadr tileToPlace)
                                     1
                                     (length matrix)
                                     matrix
                                     matrix
                                     '()
                                     2 )))))
#|
Funcion utilizada para revisar la lista de candidates y determinar si una position es valida para colocar una ficha en la matrix. Recibe como
parametros a la lista de candidates y a la position donde se quiere colocar la ficha.
En caso de que la lista de candidates sea nula, quiere decir que no se encontro antes a la position dentro de la lista de candidates, por lo
que se define como nula la colocacion de dicha fila, y se retorna un false.

En caso de que la lista de candidates no sea vacia, se comparan las positiones de la tupla de la primera position de candidates con las
positiones de la tupla position, y si ambas coinciden, se dice que la ficha va a ser colocada en un candidato correcto, que corresponde a un
espacio vacio dentro de la matrix.

Si alguna de las positiones de la tupla de la primera position de candidates es diferente que las positiones de la tupla position, se retorna
la funcion de manera recursiva, recortando la lista de candidates, ya que la ficha no pretende ser colocada en dicha primera position de
candidates.
|#
(define (checkCandidates candidates position)
  (if (null? candidates)
      #f
      (if (= (caar candidates) (car position))
          (if (= (cadar candidates) (cadr position))
              #t
              (checkCandidates (cdr candidates) position)
          )
          (checkCandidates (cdr candidates) position) )))
;----------------------------------------------------------ALGORITMO VORAZ---------------------------------------------------------------------
#|
Funcion que inicia la ejecucion del algoritmo voraz. Es llamada cada vez que la maquina tiene que colocar una ficha, y por medio de las
diversas subfunciones, se determina cual seria la position idonea para colocar la ficha, en funcion de las fichas propias de la maquina, y las
colocadas por el player. Recibe como parametro a la matrix de juego.
|#
(define (runGreedyAlgorithm matrix)
  (display "Ejecutando el algoritmo voraz \n")
  (display matrix)
  (display "\n")
  (viabilityFunction matrix
                      (findCandidates matrix)
                      (findPlacedTiles matrix '() 0 1 1 '())
                      (findPlacedTiles matrix '() 0 1 2 '())))
#|
Primera parte del algoritmo voraz. Este funcion se encarga de encontrar el conjunto de candidates possibles que pueden contribuir a la solucion,
es decir, las positiones que pueden ser ocupadas por la nueva ficha a colocar por la maquina. Recibe como parametro a la matrix de juego, y
llama a la funcion explicada anteriormente encontrarFichasColocadas, que retorna una lista con todas las positiones con element 0 (vacias)
dentro de la matrix.
|#
(define (findCandidates matrix)
  (findPlacedTiles matrix '() 0 1 0 '())
)

#|
Segunda parte del algoritmo voraz. Esta funcion se encarga de determinar la viabilidad del conjunto de candidates. Recibe como parametros
a la matrix, la lista de possibles candidates, las fichas colocadas por el jugador, y las fichas colocadas por la maquina.
|#
(define (viabilityFunction matrix candidatesToPlace player_tiles botTiles)
  ;Maquina revisa si puede ganar de forma horizontal
  (if (not (null? (checkHorizontalWin botTiles botTiles (length (car matrix)) '() candidatesToPlace)))
      (printCheck(solution (append (list (append (list (append (list (car (checkHorizontalWin botTiles botTiles (length (car matrix)) '() candidatesToPlace)))
                                                      (list 1)))
                            (list (append (list (car (checkHorizontalWin botTiles botTiles (length (car matrix)) '() candidatesToPlace)))
                                          (list (length (car matrix)))))))
              (list (append (list (car (checkHorizontalWin botTiles botTiles (length (car matrix)) '() candidatesToPlace)))
                            (list (cadr (checkHorizontalWin botTiles botTiles (length (car matrix)) '() candidatesToPlace)))))
              (list (placePlayerSymbol (car (checkHorizontalWin botTiles botTiles (length (car matrix)) '() candidatesToPlace))
                                         (cadr (checkHorizontalWin botTiles botTiles (length (car matrix)) '() candidatesToPlace))
                                         1
                                         (length matrix)
                                         matrix
                                         matrix
                                         '()
                                         2 )))))
      ;Maquina revisa si puede ganar de forma vertical
      (if (not (null? (checkVerticalWin botTiles botTiles (length matrix) '() candidatesToPlace)))
          (printCheck(solution(append (list (append (list (append (list 1)
                                                          (list (cadr (checkVerticalWin botTiles botTiles (length matrix) '() candidatesToPlace)))))
                                (list (append (list (length matrix))
                                              (list (cadr (checkVerticalWin botTiles botTiles (length matrix) '() candidatesToPlace)))))))
                  (list (append (list (car (checkVerticalWin botTiles botTiles (length matrix) '() candidatesToPlace)))
                                (list (cadr (checkVerticalWin botTiles botTiles (length matrix) '() candidatesToPlace)))))
                  (list (placePlayerSymbol (car (checkVerticalWin botTiles botTiles (length matrix) '() candidatesToPlace))
                                             (cadr (checkVerticalWin botTiles botTiles (length matrix) '() candidatesToPlace))
                                             1
                                             (length matrix)
                                             matrix
                                             matrix
                                             '()
                                             2 )))))
          ;Maquina revisa si puede ganar de forma diagonal
          (if (not (null? (checkDiagonalWin matrix (validDiagonals matrix) '() 2 "Logica")))
              (printCheck(solution(append (list (append (list (car (checkDiagonalWin matrix (validDiagonals matrix) '() 2 "Interfaz")))
                                    (list (listLastElement (checkDiagonalWin matrix (validDiagonals matrix) '() 2 "Interfaz")))))
                      (list (append (list (car (checkDiagonalWin matrix (validDiagonals matrix) '() 2 "Logica")))
                                    (list (cadr (checkDiagonalWin matrix (validDiagonals matrix) '() 2 "Logica")))))
                      (list (placePlayerSymbol (car (checkDiagonalWin matrix (validDiagonals matrix) '() 2 "Logica"))
                                                 (cadr (checkDiagonalWin matrix (validDiagonals matrix) '() 2 "Logica"))
                                                 1
                                                 (length matrix)
                                                 matrix
                                                 matrix
                                                 '()
                                                 2 )))))
              ;Maquina revisa si puede perder de forma horizontal
              (if (not (null? (checkHorizontalWin player_tiles player_tiles (length (car matrix)) '() candidatesToPlace)))
                  (printCheck(append (list '())
                          (list (append (list (car (checkHorizontalWin player_tiles player_tiles (length (car matrix)) '() candidatesToPlace)))
                                        (list (cadr (checkHorizontalWin player_tiles player_tiles (length (car matrix)) '() candidatesToPlace))) ))
                          (list (placePlayerSymbol (car (checkHorizontalWin player_tiles player_tiles (length (car matrix)) '() candidatesToPlace))
                                                     (cadr (checkHorizontalWin player_tiles player_tiles (length (car matrix)) '() candidatesToPlace))
                                                     1
                                                     (length matrix)
                                                     matrix
                                                     matrix
                                                     '()
                                                     2 ))))
                  ;Maquina revisa si puede perder de forma vertical
                  (if (not (null? (checkVerticalWin player_tiles player_tiles (length matrix) '() candidatesToPlace)))
                      (printCheck(append (list '())
                              (list (append (list (car (checkVerticalWin player_tiles player_tiles (length matrix) '() candidatesToPlace)))
                                            (list (cadr (checkVerticalWin player_tiles player_tiles (length matrix) '() candidatesToPlace))) ))
                              (list (placePlayerSymbol (car (checkVerticalWin player_tiles player_tiles (length matrix) '() candidatesToPlace))
                                                 (cadr (checkVerticalWin player_tiles player_tiles (length matrix) '() candidatesToPlace))
                                                 1
                                                 (length matrix)
                                                 matrix
                                                 matrix
                                                 '()
                                                 2 ))))
                      ;Maquina revisa si puede perder de forma diagonal
                      (if (not (null? (checkDiagonalWin matrix (validDiagonals matrix) '() 1 "Logica")))
                          (printCheck(append (list '())
                                  (list (append (list (car (checkDiagonalWin matrix (validDiagonals matrix) '() 1 "Logica")))
                                                (list (cadr (checkDiagonalWin matrix (validDiagonals matrix) '() 1 "Logica")))))
                                  (list (placePlayerSymbol (car (checkDiagonalWin matrix (validDiagonals matrix) '() 1 "Logica"))
                                                             (cadr (checkDiagonalWin matrix (validDiagonals matrix) '() 1 "Logica"))
                                                             1
                                                             (length matrix)
                                                             matrix
                                                             matrix
                                                             '()
                                                             2 )))) 
                          (placeTilesFormLines matrix (formLines matrix player_tiles 1 '() candidatesToPlace)))))))))

#|
Method for changing the value of an element in a matrix

params:
line, columns: ubication of the element.
value: new value of the entry|#

(define (replace_value_matrix newValue line column matrix)
  (if (and (<= line (length matrix)) (<= column (length(car matrix))))
      
      (replace_m_aux newValue line column matrix '() 1)
      
      #f

      )
  )

(define (replace_m_aux newValue line column matrix newMatrix current_line)
  (if (equal? current_line line)
      (replace_m_aux newValue
                     line
                     column
                     (cdr matrix)
                     (append newMatrix (list(replace_value_matrix newValue (car matrix) column)))
                     (+ 1 current_line))
     
       (if (not (null? matrix))
           (replace_m_aux newValue
                          line
                          column
                          (cdr matrix)
                          (append newMatrix (list(car matrix)))
                          (+ 1 current_line))
      newMatrix
       )
   )
  )

(define (fullMatrix matrix)
  (if (= (length (findPlacedTiles matrix '() 0 1 0 '())) 0)
      #t
      #f))






                                          

;(TTT 4 4)
(provide (all-defined-out))