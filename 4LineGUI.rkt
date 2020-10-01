#lang racket
;; #lang racket/gui
(require racket/gui/base)
(require racket/include)
(require "4Line-Logic.rkt")


;; --------------- MainWindow ---------------

; Make a mainFrame by instantiating the frame% class
(define frame (new frame%
                   [label "4Line"]
                   [width 400]
                   [height 400]))

; Make a slider to define number of columns
(define columnSlider (new slider%
                    (label "Columns")
                    (parent frame)
                    (min-value 8)
                    (max-value 16)
                    (init-value 10)))

; Make a slider to define number of rows
(define rowsSlider (new slider%
                    (label "Rows")
                    (parent frame)
                    (min-value 8)
                    (max-value 16)
                    (init-value 10)))

; The logo of the game for the main button.
(define gameLogo (make-object bitmap%
               (build-path (current-directory) "sources" "4LineLogo.png")))

; Make a button in the frame
; This is the "new game" button
(new button% [parent frame]
             [label gameLogo]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (createBoardButtons (send columnSlider get-value) (send rowsSlider get-value))
                         (send frame show #f)
                         (send gameFrame show #t)
                         )])

; Close button.
(new button% [parent frame]
             [label "CLOSE"]
             [callback (lambda (button event)
                         (send frame show #f))])


;; --------------- GameWindow ---------------

;; Board buttons matrix
(define buttonsMatrix '())


; GameFrame
(define gameFrame (new frame%
                   [label "4Line"]
                   [width 1050]
                   [height 800]))

; Make a static text message in the frame
(define msg (new message% [parent gameFrame]
                          [label "No events so far..."]))

; Button to go back to MainWindow
(new button% [parent gameFrame]
             [label "Back to Menu"]
             [callback (lambda (button event) 
                         (send gameFrame show #f)
                         (send frame show #t)
                         )])


;; Creates the panel that manages all the holeButtons
(define boardPanel
  (new vertical-pane%	 
       [parent gameFrame]	 
       [alignment '(center center)]	 
       [min-width 630]	 
       [min-height 630]	 
       [stretchable-width #f]	 
       [stretchable-height #f]))

(define (createGameWindow gameWindow)
  (new message% [parent gameWindow]
                [label "No events so far..."])
  (new button% [parent gameWindow]
               [label "Back to Menu"]
               [callback (lambda (button event) 
                         (send gameWindow show #f)
                         (send frame show #t)
                         )]))

;; --------------------- Define a Winner window --------------
; The image for winner.
(define winnerImage (make-object bitmap%
               (build-path (current-directory) "sources" "PCwinner.png")))

; the image for looser.
(define loserImage (make-object bitmap%
               (build-path (current-directory) "sources" "PCWINNER.png")))


(define (winnerWindow winner)
  (winnerWindowAux winner (new frame%
                   [label "Game Over"]
                   [width 400]
                   [height 400]) ))

(define (winnerWindowAux winner parent)
  (enableButtons #f)
  (cond((= 2 winner)
        (send gameFrame show #f)
        (new button% [parent parent]
               [label loserImage]
               [callback (lambda (button event) 
                         (send parent show #f)
                         )]))
       ((= 1 winner)
        (send gameFrame show #f)
        (new button% [parent parent]
               [label winnerImage]
               [callback (lambda (button event) 
                         (send parent show #f)
                         )])))
  (send parent show #t))

;; Creates all the holeButtons inside board.
;; param: number of columns and rows.
;; output: display the board buttons.
(define (createBoardButtons columns rows)
  ;; Creates the gui matrix and the logic matrix.
  (createBoardButtonsRows rows columns 1 (4line rows columns)))

;; Create the gui matrix.
;; param: rows, columns, cont, logicMatrix.
;; output: all the row board buttons.
(define (createBoardButtonsRows rows columns cont logicMatrix)
  (cond ((> cont rows)
         (set! buttonsMatrix (list buttonsMatrix logicMatrix)))
   (else
         (set! buttonsMatrix (cons
                              (createBoardButtonsColumns cont columns (createHorizontalStandardPane) 1 '() rows columns)
                              buttonsMatrix))
         (createBoardButtonsRows rows columns (+ cont 1) logicMatrix))))

;; Auxiliary function for createBoardButtonsRows
;; param: rows, columns, parentFrame, rowList, totalRows, totalColumns.
;; output: all the column board buttons.
(define (createBoardButtonsColumns row columns parent cont list totalRows totalColumns)
  (cond ((> cont columns)
         list)
  (else
        (createBoardButtonsColumns row columns parent (+ cont 1)
                                   (createHoleButton parent row cont list totalRows totalColumns #t)
                                   totalRows totalColumns))))

;; Disables/enables all the board buttons.
;; param: boolean.
;; output: all the buttons enabled or disabled.
(define (enableButtons enable?)
  (enableButtonsRows (car buttonsMatrix) enable?))

;; Auxiliary function for enableButtons.
;; param: listOfButtonsToEnable, boolean.
;; output: the rowButtons enabled/desabled.
(define (enableButtonsRows buttonsList enable?)
  (cond ((null? buttonsList))
  (else
   (enableButtonsColumns (car buttonsList) (cadr buttonsMatrix) enable?)
   (enableButtonsRows (cdr buttonsList) enable?))))

;; Auxiliary function for enableButtons.
;; param: listOfButtonsToEnable, boolean.
;; output: the columnButtons enabled/desabled.
(define (enableButtonsColumns buttonsList logicList enable?)
  (cond ((null? buttonsList))
  (else
   (send (car buttonsList) enable enable?)
   (enableButtonsColumns (cdr buttonsList) (cdr logicList) enable?))))


;; creates vertical Pane inside boardPanel
;; param: non
;; output: a vertical pane.
(define (createHorizontalStandardPane)
  (new horizontal-pane%	 
       [parent boardPanel]	 	 	 
       [alignment '(center center)]))

;; The three colored coins for the board
(define coinGrey (make-object bitmap%
               (build-path (current-directory) "sources" "holeCoinBasic.png")))
(define coinRed (make-object bitmap%
               (build-path (current-directory) "sources" "holeCoinRed.png")))
(define coinGold (make-object bitmap%
               (build-path (current-directory) "sources" "holeCoinGold.png")))

;; Creates an individual holeButton.
;; param: parentObject, row, column, list, totalRows, totalColumns, booleEnable
;; output: one new button in the board.
(define (createHoleButton parent rows column list totalRows totalColumns enabled?)
        (cons (new button%
             [label coinGrey]
             [parent parent]
             [callback (lambda (button event)
                         (setChoseHole rows column totalRows totalColumns 1 (cadr buttonsMatrix)))]
             [min-width 45]	 
   	     [min-height 45]
             [enabled enabled?]) list))

;; Create a matrix for the gameBoard
;; The form of this matrix is:
;; '(0 0 0)(0 0 0)(0 0 0)
;; param: totalRows, totalColumns, resultMatrix.
;; output: matrix.
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


;; When the user click a boardButton this function is called.
;; param: rowButton, columnButton, totalRows totalColumns.
;; output: calls the main function so the PC can play and inserts a coin in the hole.
(define (setChoseHole row column totalRows totalColumns playerOn oldMatrix)
  (send msg set-label (~a row
                          "x"
                          column))
  (cond ((= 1 playerOn)
         (send (matrixGet (- totalRows (insertCoinRow column 1 (cadr buttonsMatrix))) (- totalColumns (- column 1) ) 0 (car buttonsMatrix)) set-label coinRed)
         (set! buttonsMatrix (list (car buttonsMatrix)
                                   (insertCoin column 1 (cadr buttonsMatrix))))
         (mainAux 2))
        ((= 2 playerOn)
         (cond ((= column 0)
                (setChoseHole row (length (car oldMatrix)) totalRows totalColumns playerOn oldMatrix)
                (mainAux 1))
         (else
               (send (matrixGet  (- totalRows  (insertCoinRow column 1  oldMatrix))   (- totalColumns  (- column 1))  0 (car buttonsMatrix)) set-label coinGold)
               (mainAux 1))))))

;; Gets the button we want in the matrix.
;; param: rowButton, columnButton, cont = 1, listOfButtons
;; output: the button we are looking for.
(define (matrixGet row column cont list)
  (cond ((<= row cont)
         (matrixGetAux row column 1 (car list)))
  (else
    (matrixGet row column (+ cont 1) (cdr list)))))

;; Auxiliary function for matrixGet.
;; It searches in the columns of the matrix.
;; param: rowButton, columnButton, cont = 1, listOfButtons
;; output: the button we are looking for.
(define (matrixGetAux row column cont list)
  (cond ((<= column cont)
         (car list))
  (else
    (matrixGetAux row column (+ cont 1) (cdr list)))))


;; Returns the column where is a different element.
(define (findDifference matrixA matrixB cont result)
  (cond ((null? matrixA) 0)
        ((not (zero? result)) result)
  (else
         (findDifference (cdr matrixA) (cdr matrixB) (+ cont 1)
                         (findDifferenceAux (car matrixA) (car matrixB) cont)))))

(define (findDifferenceAux listA listB cont)
  (cond ((null?  listA) 0)
        ((not (= (car listA) (car listB)))  cont)
  (else (findDifferenceAux (cdr listA) (cdr listB) cont))))


(define (setTheDifferentButton matrixA matrixB playerOn)
  (set! buttonsMatrix (list (car buttonsMatrix)
                                   (cadr matrixA)))

  (print (car matrixA))
  (print (findDifference (cadr matrixA) matrixB 1 0))
  (setChoseHole 1 (findDifference (cadr matrixA) matrixB 1 0 )
                (length (car (cadr matrixA)))
                (length (cadr matrixA))                
                playerOn
                matrixB)
  )

;; --------------- Building the main ---------------

(define (main)
  ; Show the frame by calling its show method
  (send frame show #t))

;; Auxiliary function.
;; Is called during the game to let the other player play.
;; param: player
;; output: none, it only runs the logic.
(define (mainAux playerOn)
  (print (cadr buttonsMatrix))
  (cond ((not (= 0 (checkWinner (cadr buttonsMatrix))))
         (winnerWindow (checkWinner (cadr buttonsMatrix))))
        ((= playerOn 1)
         (enableButtons #t))
        ((= playerOn 2)
         (enableButtons #f)
         (setTheDifferentButton (AI 2 (cadr buttonsMatrix))
                                (cadr buttonsMatrix)
                                playerOn)
         
         (mainAux 1))))

;; --------------- Starting the program -----------------

(main)