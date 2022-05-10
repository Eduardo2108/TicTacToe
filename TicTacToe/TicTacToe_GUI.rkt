;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TicTacToe_GUI) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;TicTacToe GUI
;;Desarrollado por: Jose Espinoza, Eduardo Zumbado
;;Tarea 1 - CE3104 - Lenguajes, Compiladores e Interpretes

(require graphics/graphics)
(require racket/draw)
(open-graphics)

(require racket/include)
(require "TicTacToe_Logic.rkt")

;;--------------Initial GUI values and image loading------------------------------------------------------------------

;;Lenght for the GUI window
(define lngth 800)
;;Width for the GUI window
(define wdth 800)

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
      ((drawRows (- n 1) m)
      (drawColumns (- m 1) n)))
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
         (((draw-solid-rectangle hidden-window) (make-posn 100 (+ (* m 65) 10)) (* lngth 65) 4  "red")
         (drawRows (- m 1) lngth)))))
;;Function that draws the columns of the matrix
;;Receives: n (integer for columns, column quantity) and lngth (states the length for the line to be drawn)
;;Returns: Graphical lines for the columns
;;Restrictions: Must be integers
(define (drawColumns n lngth)
  (cond ((equal? n 0) #t)
        (else
         (((draw-solid-rectangle hidden-window) (make-posn (+ (* n 65) 100) 10) 4 (* lngth 65) "red")
         (drawColumns (- n 1) lngth)))))

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
  (cond (((equal? (left-mouse-click? click)
         (mouseClick m n (append list (verificarDibujoX x y m n list))  )))
  (else
   (mouseClick m n lista ))))))
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




























