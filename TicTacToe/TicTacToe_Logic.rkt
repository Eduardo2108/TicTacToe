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
Function that finds all the valid diagonals that could make a player win the game.
Receives: the matrix of the board
Returns: a list of all valid diagonals.
|#
(define (validDiagonals matrix)
  (append (topDownVerticalDiagonals (length (car matrix)) (length matrix) 1 (length matrix) (length matrix) '() '())
          (topDownHorizontalDiagonals (length (car matrix)) (length matrix) 2 1 2 '() '())
          (discardDiagonals  (upwardVerticalDiagonals (length (car matrix)) (length matrix) (length (car matrix)) (length matrix) (length matrix) '() '()) '())
          (discardDiagonals  (upwardHorizontalDiagonals (length (car matrix)) (length matrix) (- (length (car matrix)) 1) 1 (- (length (car matrix)) 1) '() '()) '())))
#|
Function that identifies the upward vertical diagonals in wich the players can win
Recieves: mMax: number of columns
          nMax: number of lines
          nActual: the actual line the algorithm is processing
          start_n: the line where the diagonal started
          currentDiagonal: diagonal generated recursively
          diagonals: all formed diagonals      
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
Function that identifies the upward horizontal diagonals in wich the players can win
Recieves: mMax: number of columns
          nMax: number of lines
          nActual: the actual line the algorithm is processing
          start_m: the column where the current diagonal started
          currentDiagonal: diagonal generated recursively
          diagonals: all formed diagonals 
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
#|
Function that identifies the downwards vertical diagonals in wich the players can win
Recieves: mMax: number of columns
          nMax: number of lines
          nActual: the actual line the algorithm is processing
          start_n: the line where the diagonal started
          currentDiagonal: diagonal generated recursively
          diagonals: all formed diagonals      
|#  
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
#|
Function that identifies the downwards horizontal diagonals in wich the players can win
Recieves: mMax: number of columns
          nMax: number of lines
          nActual: the actual line the algorithm is processing
          start_m: the column where the current diagonal started
          currentDiagonal: diagonal generated recursively
          diagonals: all formed diagonals 
|#  
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
  Function that determines wich diagonals are valid for the win.
|# 
(define (discardDiagonals diagonal_list validDiagonals)
  (if (null? diagonal_list)
      validDiagonals
      (if (>= (length (car diagonal_list)) 3)
          (discardDiagonals (cdr diagonal_list) (append validDiagonals (list (car diagonal_list))))
          (discardDiagonals (cdr diagonal_list) validDiagonals))))

#|  
  Function that checks if there is any position in which any player can win. Calls check diagonal

  Receives: matrix: game matrix
            diagonals_list: list of all diagonals
            flag_win: true if the diagonal is a win.
            player: determines if is the player or the machine
            code: inner part of the program, used for GUI methods.
  Returns: a list of valid diagonals where a player can win.
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
  Function that checks each diagonal to see if there is a possible win in a diagonal of tiles.

  Receives: matrix: matrix game
            player: user or machine
            diagonal: the diagonal to check
            auxDiagonal: copie of diagonal, for the recurion
            amountSpaces: amount of tiles in the diagonal
            code: inner part of the program, for GUI methods.
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

  Function that checks if there is a player in the tile.
  Recieves: matrix
            player: user or machine
            position: position of the tile to check
  Returns: 1 if theres a player on the tile, 0 if not.
|# 
(define (checkDiagonalPos matrix player position)
  (if (= (findTile matrix (car position) (cadr position) '()) player)
      1
      0))
#|
  Function that determines in which positions of a diagonal a win can be generated by placing a player.
  Receives: matrix
            diagonal
            auxDiagonal
            code
|# 
(define (findWinTileDiagonal matrix diagonal auxDiagonal code)
  (if (null? diagonal)
      '()
      (if (analizeEmptyDiagonalPos matrix (car diagonal))
          (if (equal? code "Interfaz")
              auxDiagonal
              (car diagonal))
          (findWinTileDiagonal matrix (cdr diagonal) auxDiagonal code))))
#|
  Function that determines if a tile has or not a player
|#
(define (analizeEmptyDiagonalPos matrix position)
  (if (= (findTile matrix (car position) (cadr position) '()) 0)
      #t
      #f))

#|
  Function that allows the AI to generete its lines.
  Receives: matrix
            player_tiles: a list of tiles
            player
            possibles: posible positions, filled recursively
            candidates: all empty positions inside the matrix

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
  Funtion that determines in which positions the AI could continue forming its lines.
  Receives: possibles: list of possible lines, fill recurively
            m_toForm: position m of the tile being analized
            n_toForm: position n of the tile being analized
            mMax: amount of columns
            nMax: amount of lines
            candidates: all empty tiles in the matrix
            up, right, down left: flags to keep track of directions checked.
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
  Function that places a player in a tile in case of forming a line.
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
  Function used to check the candidates list and determine if a position is valid to place a player's tile in the matrix.
  Receives: candidates: list of posible tiles
            position: position (m n) of the tile
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
;----------------------------------------------------------greedy Algorithm---------------------------------------------------------------------
#|
  Function that executes the greedy Algorithm. Its called in every turn of the AI.
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
  First part of the greed algorithm. Finds the possible candidates of the solution.
  Receives: matrix
|#
(define (findCandidates matrix)
  (findPlacedTiles matrix '() 0 1 0 '())
)

#|
  Second part of the algorithm. Determines the viability of all the candidates.
  Receives: matrix
            candidatesToPlace: all possible candidates
            player_tiles: all tiles placed by the player.
            botTiles: all the tiles placed by the AI
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