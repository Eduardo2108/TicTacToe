;;TicTacToe GUI
;;Desarrollado por: Jose Espinoza, Eduardo Zumbado
;;Tarea 1 - CE3104 - Lenguajes, Compiladores e Interpretes
#lang racket
(require graphics/graphics)
(require racket/draw)
(open-graphics)

(require racket/include)
(require "TicTacToe_Logic.rkt")

;;--------------Initial GUI values and image loading------------------------------------------------------------------

;;Lenght for the GUI window
(define lngth 1100)
;;Width for the GUI window
(define wdth 700)

;;Creates the window
(define window (open-viewport "TicTacToe" wdth lngth))

;;Creates a hidden pixmap window for pixel control
(define hidden-window (open-pixmap "hidden" wdth lngth))

;;Draws a black rectangle to be used as a background
((draw-solid-rectangle hidden-window)(make-posn 0 0)wdth lngth "black")

;;Draws the game initialization background
((draw-pixmap hidden-window) "Images/first-page.png" (make-posn 0 0))
(copy-viewport hidden-window window)

;;-------------Graphic matrix implementation--------------------------------------------------------------------------

;;Defines the matrix
(define matrix '())
;;Function that draws the lines of the matrix
;;Receives: m (integer for rows) and n (integer for columns)
;;Returns: Graphical lines for the board
;;Restrictions: Must be integers
(define(drawLines m n)
  (cond ((and (>= m 3) (<= m 10) (>= n 3) (<= n 10))
      (drawRows (- n 1) m)
      (drawColumns (- m 1) n))
        (else
         #f)))
;;Function that draws the rows of the matrix
;;Receives: m (integer for rows, row quantity) and lngth (states the length for the line to be drawn)
;;Returns: Graphical lines for the rows
;;Restrictions: Must be integers
(define (drawRows m lngth)
  (cond ((equal? m 0)
         #t)
        (else
         ((draw-solid-rectangle hidden-window) (make-posn 100 (+ (* m 65) 10)) (* lngth 65) 4  "red")
         (drawRows (- m 1) lngth))))
;;Function that draws the columns of the matrix
;;Receives: n (integer for columns, column quantity) and lngth (states the length for the line to be drawn)
;;Returns: Graphical lines for the columns
;;Restrictions: Must be integers
(define (drawColumns n lngth)
  (cond ((equal? n 0) #t)
        (else
         ((draw-solid-rectangle hidden-window) (make-posn (+ (* n 65) 100) 10) 4 (* lngth 65) "red")
         (drawColumns (- n 1) lngth))))

;;-------------------Drawing of X and O symbols-----------------------------------------------------------------

;;Function that draws X symbol con the matrix
;;Receives: x (x position), y (y position)
;;Returns: Places X image in that position
;;Restrictions: None.
(define (drawX x y)
  ((draw-pixmap hidden-window) "Images/X.png" (make-posn (+ 45(* 65 x))(- (* 65 y) 45))
  (copy-viewport hidden-window window)))
;;Function that draws Y symbol con the matrix
;;Receives: x (x position), y (y position)
;;Returns: Places Y image in that position
;;Restrictions: None.
(define (drawO x y)
  ((draw-pixmap hidden-window) "Images/O.png" (make-posn (+ 45(* 65 x))(- (* 65 y) 45))
  (copy-viewport hidden-window window)))

;;-----------------------------Mouse implementation for clicking the tiles--------------------------------------
;; Function that works as a cycle and waits for the left-click to be pressed, once pressed saves the value of x and y of the position clicked and saves it on the list.
;; m: size of rows available for the game
;; n: size of columns available for the game
;; list: list of the coordinated used by an object(starts as an empty list)
(define (mouseClick m n list)
  (let* ((click (get-mouse-click window))
         (x (posn-x (query-mouse-posn window)))
         (y (posn-y (query-mouse-posn window))))
  (cond ((equal? (left-mouse-click? click)
         (mouseClick m n (append list (tileCheck x y m n list)) )))
  (else
   (mouseClick m n list )))))
;; Function that provides a value of row depending on the y coordinates
;; Receives: y (position on y provided by mouse function)
;; Returns: value of row
(define (setRowValue y)
  (cond ((and (<= 10 y) (> 75 y))
         '(1))
        ((and (<= 75 y) (> 140 y))
         '(2))
        ((and (<= 140 y) (> 205 y))
         '(3))
        ((and (<= 205 y) (> 270 y))
         '(4))
        ((and (<= 270 y) (> 335 y))
         '(5))
        ((and (<= 335 y) (> 400 y))
         '(6))
        ((and (<= 400 y) (> 465 y))
         '(7))
        ((and (<= 465 y) (> 530 y))
         '(8))
        ((and (<= 530 y) (> 595 y))
         '(9))
        ((and (<= 595 y) (> 660 y))
         '(10))
        (else
         (list y))))
;; Function that provides a value of column depending on the x coordinates
;; Receives: x (position on x provided by mouse function)
;; Returns: value of column
(define (setColumnValue x)
  (cond ((and (<= 100 x) (> 165 x))
         '(1))
        ((and (<= 165 x) (> 230 x))
         '(2))
        ((and (<= 230 x) (> 295 x))
         '(3))
        ((and (<= 295 x) (> 360 x))
         '(4))
        ((and (<= 360 x) (> 425 x))
         '(5))
        ((and (<= 425 x) (> 490 x))
         '(6))
        ((and (<= 490 x) (> 555 x))
         '(7))
        ((and (<= 555 x) (> 620 x))
         '(8))
        ((and (<= 620 x) (> 685 x))
         '(9))
        ((and (<= 685 x) (> 750 x))
         '(10))
        (else
         (list x))))
;;Function that verifies if the selected tile is available to draw a X on it.
;;Receives: x (x coordinate in pixels), y  (y coordinate in pixels), m (size of the rows), n (size of the columns, list (list containing the coordinates of the objects)
;;Returns: The position where the X is going to be placed (using setRowValue and setColumnValue), will return (1, 2), where 1 is the row and 2 is the column
(define (tileCheck x y m n lista)
  (let* ((row (setRowValue y))
         (column (setColumnValue x))
         (position (cons (car column) row)))
  (cond (( and (<= (car row) n) (<= (car column) m) (not (miembro? position lista)))
         (drawX (car column) (car row))
         (playerTurn column row n)
         (let* ((result botTurn))
           (cond ((null? result)
                  (list position))
                 (else
                  (append (list (botTurn)) (list position))))))
        (else
         (showNotAvailable)
         '()))))
;; Function that checks if an element is inside the list
;; Receives: element and list
;; Returns: either true or false

(define (miembro? ele lista)
  (cond( (null? lista)
         #f)
       ( (equal? ele (car lista))
         #t)
       (else
        (miembro? ele (cdr lista)))))

;;-----------------------------------------Turn Management----------------------------------------------------------------------------------------------------------------

;; Function that places the X in the logical matrix (inside TicTacToe_Logic.rkt), validates if there is a winner or a draw, updates the logical matrix.Only calls methods from TicTacToe_Logic.rkt for logical management.
;; Receives: column (column obtained from mouseClick and validated by tileCheck), row (row obtained from mouseClick and validated by tileCheck)
(define (playerTurn column row m)
  (let* ( (result (placePlayerSymbol (car row) (car column) 1 m matrix matrix '() 1))
          (row (car result))
          (pos (cadr result))
          (newMatrix (caddr result)))
    (cond ((null? row)
           (checkDraw)
           (set! matrix newMatrix))
          (else
           (drawHorizontalDiagonal (cadar row) (caar row) (cadadr row)(caadr row))
           (drawVertical  (cadar row) (caar row)  (cadadr row) (caadr row))
           (showWinner)))))
;;Function that places the tile for the AI, calls in the greedy algorithm from TicTacToe_Logic.rkt. Logically and graphically inserts the O tile in the matrix. As well as the playerTurn it first checks if the AI won or if there is a draw.
;;Receives: None.
;;Returns: The position where the AI placed the tile as a tuple (1,2)
(define (botTurn)
  (cond ((equal? (checkDraw) #f)
         (let* ( (result (placeBotSymbol matrix 2))
          (row (car result))
          (pos (cadr result))
          (newMatrix (caddr result)))
        (cond ((null? row)
                 (drawO (cadr pos) (car pos))
                 (set! matrix newMatrix)
                 (checkDraw)
                 (list (cadr pos) (car pos)))
                (else
                 (drawO (cadr pos) (car pos))
                 (drawHorizontalDiagonal (cadar row) (caar row) (cadadr row)(caadr row))
                 (drawVertical  (cadar row) (caar row)  (cadadr row) (caadr row))
                 (showLoser)
                 '() ))))))
;; Function that checks if the matrix is full, if this happens then no one won and is a draw
;; Returns: True if matrix is full, False if not. 
(define (checkDraw)
  (cond (( equal? (fullMatrix matrix) #t)
         (showDraw)
         #t)
        (else
         #f)))

;;--------------------------------- Windows for winning, losing and drawing --------------------------------------------------------------
;;Functions that open a pop up window with the massage if you won, lost or drawn. Also there is another pop up notification if the tile you tried to use is not available (already used)
;;Returns: Pop up window with the message, closes after couple of seconds. 
(define (showLoser)
  (define windowL (open-viewport "Loser" 300 50))
  ((draw-viewport windowL) "black")
  ((draw-string windowL) (make-posn 50 20) "You Lose, better luck next time" "red")
  (sleep 5)
  (close-viewport windowL))

(define (showWinner)
  (define windowW (open-viewport "Winner" 300 50))
  ((draw-viewport windowW) "black")
  ((draw-string windowW) (make-posn 50 20) "You win! Congratulations!" "red")
  (sleep 5)
  (close-viewport windowW))

(define (showNotAvailable)
  (define windowA (open-viewport "Not Available" 300 50))
  ((draw-viewport windowA) "black")
  ((draw-string windowA) (make-posn 50 20) "Sorry this tile is not available foryou to place an X" "red")
  (sleep 1.5)
  (close-viewport windowA))

(define (showDraw)
  (define windowD (open-viewport "Draw" 300 50))
  ((draw-viewport windowD) "black")
  ((draw-string windowD) (make-posn 50 20) "It's a draw! Better luck next time" "red")
  (sleep 5)
  (close-viewport windowD))

(define (showWrongNumbers)
  (define windowN (open-viewport "Draw" 300 50))
  ((draw-viewport windowN) "black")
  ((draw-string windowN) (make-posn 50 20) "Numbers placed for columns and rows are not valid, please run the program again and type values between 3 and 10." "red")
  (sleep 2)
  (close-viewport windowN))

;;-------------------------------Line Drawing when won-------------------------------------------------------

;;Function that draws a line between two positions horizontaly and diagonaly
;;Receives: column1 and row1 (point of start of the line) and column2 and row 2 (point of end of the line)
;;Returns: Drawing of the line on the GUI
(define (drawHorizontalDiagonal column1 row1 column2 row2)
  ((draw-line hidden-window) (make-posn (+ 65 (* 65 column1)) (- (* 65 row1) 27)) (make-posn (+ 65 (* 65 column2)) (- (* 65 row2) 27)) "red")
  ((draw-line hidden-window) (make-posn (+ 65 (* 65 column1)) (- (* 65 row1) 26)) (make-posn (+ 65 (* 65 column2)) (- (* 65 row2) 26)) "red")
  ((draw-line hidden-window) (make-posn (+ 65 (* 65 column1)) (- (* 65 row1) 25)) (make-posn (+ 65 (* 65 column2)) (- (* 65 row2) 25)) "red")
  ((draw-line hidden-window) (make-posn (+ 65 (* 65 column1)) (- (* 65 row1) 24)) (make-posn (+ 65 (* 65 column2)) (- (* 65 row2) 24)) "red")
  ((draw-line hidden-window) (make-posn (+ 65 (* 65 column1)) (- (* 65 row1) 23)) (make-posn (+ 65 (* 65 column2)) (- (* 65 row2) 23)) "red")
  (copy-viewport hidden-window window))
 
;;Function that draws a line between two positions vertically
;;Receives: column1 and row1 (point of start of the line) and column2 and row 2 (point of end of the line)
;;Returns: Drawing of the line on the GUI
(define (drawVertical column1 row1 column2 row2)
  ((draw-line hidden-window) (make-posn (+ 65 (* 65 column1)) (- (* 65 row1) 25)) (make-posn (+ 65 (* 65 column2)) (- (* 65 row2) 25)) "red")
  ((draw-line hidden-window) (make-posn (+ 66 (* 65 column1)) (- (* 65 row1) 25)) (make-posn (+ 66 (* 65 column2)) (- (* 65 row2) 25)) "red")
  ((draw-line hidden-window) (make-posn (+ 67 (* 65 column1)) (- (* 65 row1) 25)) (make-posn (+ 67 (* 65 column2)) (- (* 65 row2) 25)) "red")
  ((draw-line hidden-window) (make-posn (+ 68 (* 65 column1)) (- (* 65 row1) 25)) (make-posn (+ 68 (* 65 column2)) (- (* 65 row2) 25)) "red")
  ((draw-line hidden-window) (make-posn (+ 69 (* 65 column1)) (- (* 65 row1) 25)) (make-posn (+ 69 (* 65 column2)) (- (* 65 row2) 25)) "red")
  (copy-viewport hidden-window window))

;;------------------------------ GAME INITIALIZATION ----------------------------------------------------------------------------------------
;;Function that starts the game, creates the graphic instances needed to open the game GUI and generates the logical matrix.
;;Receives: m and n, values for columns and rows.
;;Returns: Graphic and logic matrix ready to be updated (played)
;;Restrictions: Must be integers between 3 and 10. 
(define (TTT m n)
  (cond ((and (>= m 3) (<= m 10) (>= n 3) (<= n 10))
         ((draw-solid-rectangle hidden-window) (make-posn 0 0) lngth wdth "black")
         ((draw-solid-rectangle hidden-window) (make-posn 850 65) 180 30 "black")
         ((draw-string hidden-window) (make-posn 900 85) "Player's token: X" "red")
         ((draw-solid-rectangle hidden-window) (make-posn 800 350) 300 8 "black")
         ((draw-solid-rectangle hidden-window) (make-posn 855 380) 180 30 "black")
         ((draw-string hidden-window) (make-posn 875 400) "Bot's token: O" "red")

         (drawLines m n)(copy-viewport hidden-window window)
         (set! matrix (GenerateLogicMatrix m n))
         (mouseClick m n '()))
        (else
         (showWrongNumbers))))
(copy-viewport hidden-window window)















