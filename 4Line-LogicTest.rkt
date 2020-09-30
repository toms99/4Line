#lang racket


;; Create the matrix. 
(define (4line rows columns)
  (cond 
    [(< rows 8) (error "Debe ser mínimo un tablero 8x8")]
    [(< columns 8) (error "Debe ser mínimo un tablero 8x8")]
    [(> rows 16) (error "Debe ser máximo un tablero 16x16")]
    [(> columns 16) (error "Debe ser máximo un tablero 16x16")]
  [else (createBoardMatrix rows columns '())]))

(define (createBoardMatrix rows columns result)
  (cond ((zero? columns)
         result)
   (else
         (createBoardMatrix rows (- columns 1) (cons (createList rows '()) result)))))

(define (createList rows result)
  (cond ((zero? rows)
        result)
   (else
        (createList (- rows 1) (append '(0) result)))))


;; Select a column to insert the coin
(define (insertCoin column player matrix)
  (cond
    [(= column 0) (cons (insertCoin_aux (car matrix) player) (cdr matrix))]
    [else (cons (car matrix) (append (insertCoin (- column 1) player (cdr matrix))))]))


(define (insertCoin_aux selectedRow player)
  (cond
    [(null? (cdr selectedRow)) (append (list player) (cdr selectedRow))]
    [(not(zero? (cadr selectedRow))) (append (list player) (cdr selectedRow))]
    [else (cons (car selectedRow) (insertCoin_aux (cdr selectedRow) player))]))

;; Returns the row of the coin inserted
(define (insertCoinRow column player matrix)
  (cond
    [(= column 1) (insertCoinRow_aux (car matrix) 1 player) ]
    [else (insertCoinRow (- column 1) player (cdr matrix))]))


(define (insertCoinRow_aux selectedColumn row player)
  (cond
    [(null? (cdr selectedColumn)) row]
    [(not(zero? (cadr selectedColumn))) row]
    [else (insertCoinRow_aux (cdr selectedColumn) (+ row 1) player)]))

;; ---------------- Funcion verificar ganador ------------

;; Checks if someone is a winner.
(define (checkWinner matrix)
  (checkRows matrix)
  (checkColumns matrix)
  (checkDiagonals matrix))



;; Checks if someone win by 4 in a row.
(define (checkRows matrix)
  (cond ((null? matrix)
         0)
  (else
         (checkRowsAux (car matrix) 0 0)
         (checkRows (cdr matrix)))))

(define (checkRowsAux list pointsP1 pointsP2)
  (cond ((= pointsP1 4) 
         1)
        ((= pointsP2 4)
         2)
        ((null? list)
         0)     
        ((equal? 1 (car list))
         (checkRowsAux (cdr list) (+ 1 pointsP1) 0))
        ((equal? 2 (car list))
         (checkRowsAux (cdr list) 0 (+ 1 pointsP2)))
  (else
        (checkRowsAux (cdr list) 0 0))))


;; Checks if a player win by 4 in a column.
(define (checkColumns matrix)
  (checkColumnsAux matrix 1 1 0 0))

(define (checkColumnsAux matrix row column pointsP1 pointsP2)
  (cond ((= pointsP1 4)
         1)
        ((= pointsP2 4)
         1)
        ((> column (length (car matrix)))
         0)
        ((> row (length matrix))
         (checkColumnsAux matrix 1 (+ column 1) 0 0))
        ((= 1 (getByIndexRow matrix row column 1))
         (checkColumnsAux matrix (+ row 1) column (+ pointsP1 1) 0))
        ((= 2 (getByIndexRow matrix row column 1))
         (checkColumnsAux matrix (+ row 1) column 0 (+ pointsP2 1)))
   (else
         (checkColumnsAux matrix (+ row 1) column 0 0))))



;; A couple of functions that get the element in
;; (row, column) index.
(define (getByIndexRow matrix row column cont)
  (cond ((null?  matrix)
         -1)
        ((= row cont)
         (getByIndexColumn (car matrix) column 1))
  (else
         (getByIndexRow (cdr matrix) row column (+ cont 1)))))

(define (getByIndexColumn list column cont)
  (cond ((null? list)
         -1)
        ((= column cont)
         (car list))
  (else
         (getByIndexColumn (cdr list) column (+ cont 1)))))

;; Checks if someone wins by 4 in a diagnal.
(define (checkDiagonals matrix)
  (selectDiagonalsToCheck matrix 1 1 1))

(define (selectDiagonalsToCheck matrix row column cont)
  (cond ((> row (length matrix))
         0)
        ((> column (length (car matrix)))
         (checkDiagonalsAux matrix row column 0 0 -1)
         (checkDiagonalsAux matrix row 1 0 0 1)
         (selectDiagonalsToCheck matrix (+ row 1) column (+ cont 1)))
  (else
         (checkDiagonalsAux matrix row column 0 0 1)
         (checkDiagonalsAux matrix row column 0 0 -1)
         (selectDiagonalsToCheck matrix row (+ column 1) (+ cont 1)))))

;; Auxiliary function for checkDiagonal.
;; It checks the right and left diagonal of a specific matrix index.
;; Coeficient indicates if it has to check right (1) diagonal or left (-1) diagonal.
(define (checkDiagonalsAux matrix row column pointsP1 pointsP2 coeficient)
  (cond ((= pointsP1 4)
         1)
        ((= pointsP2 4)
         2)
        ((or (< row 1) (< column 1))
         0)
        ((> column (length (car matrix)))
         0)
        ((> row (length matrix))
         0)
        ((= 1 (getByIndexRow matrix row column 1))
         (checkDiagonalsAux matrix (+ row 1) (+ column coeficient) (+ pointsP1 1) 0 coeficient))
        ((= 2 (getByIndexRow matrix row column 1))
         (checkDiagonalsAux matrix (+ row 1) (+ column coeficient) 0 (+ pointsP2 1) coeficient))
   (else
         (checkDiagonalsAux matrix (+ row 1) (+ column coeficient) 0 0 coeficient))))

;; Checks if someone is close to win by Columns.
;; Returns '(playerCloseToWin rowWhereItCanWin)
;; Checks if someone win by 4 in a row.
(define (checkVerticales matrix)
  (checkVerticalesAux2 matrix '(0 0) 1))

(define (checkVerticalesAux2 matrix result cont)
  (cond ((or (= 1 (car result)) (= 2 (car result)))
         result)
        ((null? matrix)
         result)
  (else
         (checkVerticalesAux2 (cdr matrix) (checkVerticalesAux (car matrix) 0 0 cont) (+ cont 1)))))

(define (checkVerticalesAux lista pointsP1 pointsP2 cont)
  (cond ((null? lista)
         '(0 0))   
        ((and (= pointsP1 3) (equal? 0 (car lista))  (fullColumn? lista)) 
         (list 1 cont))
        ((and (= pointsP2 3) (equal? 0 (car lista))  (fullColumn? lista))
         (list 2 cont))          
        ((equal? 1 (car lista))
         (checkVerticalesAux (cdr lista) (+ 1 pointsP1) 0 cont))
        ((equal? 2 (car lista))
         (checkVerticalesAux (cdr lista) 0 (+ 1 pointsP2) cont))
  (else
        (checkVerticalesAux (cdr lista) 0 0 cont))))

;; Checks if someone is close to win by Rows.
;; Returns '(playerCloseToWin rowWhereItCanWin)
(define (checkHorizontales matrix)
  (checkHorizontalesAux matrix 1 1 0 0))

(define (checkHorizontalesAux matrix row column pointsP1 pointsP2)
  (cond ((and (= pointsP1 3) (= 0 (getByIndexRow matrix row column 1))  (< 0 (getByIndexRow matrix row (+ column 1) 1)))
         (list 1 row))
        ((and (= pointsP2 3) (= 0 (getByIndexRow matrix row column 1))  (< 0 (getByIndexRow matrix row (+ column 1) 1)))
         (list 2 row))
        ((> column (length (car matrix)))
         '(0 0))
        ((> row (length matrix))
         (checkHorizontalesAux matrix 1 (+ column 1) 0 0))
        ((= 1 (getByIndexRow matrix row column 1))
         (checkHorizontalesAux matrix (+ row 1) column (+ pointsP1 1) 0))
        ((= 2 (getByIndexRow matrix row column 1))
         (checkHorizontalesAux matrix (+ row 1) column 0 (+ pointsP2 1)))
   (else
         (checkHorizontalesAux matrix (+ row 1) column 0 0))))


;; Checks if someone is close to win by Diagonals.
;; Returns '(playerCloseToWin rowWhereItCanWin)

(define (checkDiagonales matrix)
  (selectDiagonalesToCheck matrix 1 1 1 '(0 0)))

(define (selectDiagonalesToCheck matrix row column cont result)
  (cond ((or (< 0 (car result)) (> row (length matrix)))
         result)
        ((> column (length (car matrix)))
         (selectDiagonalesToCheck matrix (+ row 1) column (+ cont 1)
                                  (checkDiagonalesAux matrix row column 0 0 -1 result))
         (selectDiagonalesToCheck matrix (+ row 1) column (+ cont 1)
                                  (checkDiagonalesAux matrix row 1 0 0 1 result)))
  (else
         (selectDiagonalesToCheck matrix row (+ column 1) (+ cont 1)
                                  (checkDiagonalesAux matrix row column 0 0 1 result))
         (selectDiagonalesToCheck matrix row (+ column 1) (+ cont 1)
                                  (checkDiagonalesAux matrix row column 0 0 -1 result)))))

;; Auxiliary function for checkDiagonal.
;; It checks the right and left diagonal of a specific matrix index.
;; Coeficient indicates if it has to check right (1) diagonal or left (-1) diagonal.
(define (checkDiagonalesAux matrix row column pointsP1 pointsP2 coeficient result)
  (cond ((and (= pointsP1 3) (= 0 (getByIndexRow matrix row column 1)) (< 0 (getByIndexRow matrix ( + row 1) (+ column 1) 1)))
         (list 1 column row))
        ((and (= pointsP2 3) (= 0 (getByIndexRow matrix row column 1)) (< 0 (getByIndexRow matrix ( + row 1) (+ column 1) 1)))
         (list 2 column row))
        ((or (< row 1) (< column 1))
         '(0 0))
        ((or (> column (length (car matrix))) (> row (length matrix)))
         '(0 0))
        ((= 1 (getByIndexRow matrix row column 1))
         (checkDiagonalesAux matrix (+ row 1) (+ column coeficient) (+ pointsP1 1) 0 coeficient result))
        ((= 2 (getByIndexRow matrix row column 1))
         (checkDiagonalesAux matrix (+ row 1) (+ column coeficient) 0 (+ pointsP2 1) coeficient result))
   (else
         (checkDiagonalesAux matrix (+ row 1) (+ column coeficient) 0 0 coeficient result))))

;; Checks if a column is full played.
(define (fullColumn? lista)
  (cond ((zero? (car lista))
         #f)
  (else
         #t)))

;;  ---------- Exporting all -----------

(provide (all-defined-out))

(define x '((1 0 0 0 0)
            (0 1 0 0 0)
            (0 0 1 0 0)
            (0 0 0 0 0)))

(checkDiagonales x)





