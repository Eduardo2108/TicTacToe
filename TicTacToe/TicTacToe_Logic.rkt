;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TicTacToe_Logic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

; Method for creating the lines in the matrix
#|
Params:
n amount of lines
line empty list to create the line

Returns:
a list of n elements 'e' for the empty places.

|#
(define (create_line n line)
  ( if ( > n 0)
       (create_line (- n 1) (append line '(0)))
       line))

; Method for creating the matrix of the game
#| Params:
n: lines
m: columns
matrix: empty list

Returns:
a new matrix mxn with 'e' in all entries|#

(define (generate_matrix matrix m n)
  (if (> m 0)
      (generate_matrix (append matrix (list(create_line n '( ) ))) (- m 1) n)
                       matrix))


#|
Method for replacing an element on a list with other one

Params:
i: index of the element (starts on 1). Must be within the range of the list
list: arrange of elements
newValue: new value to replace the old one.

|#


(define (replace newValue list i)
  (cond ((>= i (length list)) #f )
        (else (replace_aux newValue list i 1 '() ))))


(define (replace_aux newValue lista i counter newList)
  (if (equal? counter i)
      (replace_aux newValue (cdr lista) i (+ 1 counter) (append newList (list newValue) ))
      (if (not(equal? lista '()))
          (replace_aux newValue (cdr lista) i (+ 1 counter) (append newList (list (car lista)) ))
          newList
          )
      )
  )












