#lang racket
;; #lang racket/gui
(require racket/gui/base)
(require racket/include)
(require "4Line-LogicTest.rkt")


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

(define rowsSlider (new slider%
                    (label "Rows")
                    (parent frame)
                    (min-value 8)
                    (max-value 16)
                    (init-value 10)))


; Make a button in the frame
(new button% [parent frame]
             [label "NEW GAME"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (createBoardButtons (send columnSlider get-value) (send rowsSlider get-value))
                         (send frame show #f)
                         (send gameFrame show #t)            
                         )])

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
                         (send frame show #t)
                         (send gameFrame show #f)
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

;; Creates all the holeButtons inside board.
;; param: number of columns and rows.
(define (createBoardButtons columns rows)
  ;; Creates the gui matrix and the logic matrix.
  (createBoardButtonsRows rows columns 1 (4line rows columns)))

;; Create the gui matrix.
;; Display all the needed buttons.
(define (createBoardButtonsRows rows columns cont logicMatrix)
  (cond ((> cont rows)
         (set! buttonsMatrix (list buttonsMatrix logicMatrix)))
   (else
         (set! buttonsMatrix (cons
                              (createBoardButtonsColumns cont columns (createHorizontalStandardPane) 1 '() rows columns)
                              buttonsMatrix))
         (createBoardButtonsRows rows columns (+ cont 1) logicMatrix))))

;; Auxiliary function for createBoardButtonsRows
;; Sets the buttons in the columns.
(define (createBoardButtonsColumns row columns parent cont list totalRows totalColumns)
  (cond ((> cont columns)
         list)
  (else
        (createBoardButtonsColumns row columns parent (+ cont 1)
                                   (createHoleButton parent row cont list totalRows totalColumns #t)
                                   totalRows totalColumns))))

;; Disables/enables all the board buttons.
(define (enableButtons enable?)
  (enableButtonsRows (car buttonsMatrix) enable?))

(define (enableButtonsRows buttonsList enable?)
  (cond ((null? buttonsList))
  (else
   (enableButtonsColumns (car buttonsList) enable?)
   (enableButtonsRows (cdr buttonsList) enable?))))

(define (enableButtonsColumns buttonsList enable?)
  (cond ((null? buttonsList))
  (else
   (send (car buttonsList) enable enable?)
   (enableButtonsColumns (cdr buttonsList) enable?))))

;; creates vertical Pane inside boardPanel
(define (createHorizontalStandardPane)
  (new horizontal-pane%	 
       [parent boardPanel]	 	 	 
       [alignment '(center center)]))

;; The three colored coins.
(define coinGrey (make-object bitmap%
               (build-path (current-directory) "sources" "holeCoinBasic.png")))
(define coinRed (make-object bitmap%
               (build-path (current-directory) "sources" "holeCoinRed.png")))
(define coinGold (make-object bitmap%
               (build-path (current-directory) "sources" "holeCoinGold.png")))

;; Creates an individual holeButton
;; param: parent object, number columns.
(define (createHoleButton parent rows column list totalRows totalColumns enabled?)
        (cons (new button%
             [label coinGrey]
             [parent parent]
             [callback (lambda (button event)
                         (setChoseHole rows column totalRows totalColumns))]
             [min-width 45]	 
   	     [min-height 45]
             [enabled enabled?]) list))

;; Create a matrix for the gameBoard
;; The form of this matrix is:
;; '(0 0 0)(0 0 0)(0 0 0)
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


;; When the user chooses a button
(define (setChoseHole row column totalRows totalColumns)
  (send msg set-label (~a row
                          "x"
                          column))
 (send (matrixGet (- totalRows (insertCoinRow column 1 (cadr buttonsMatrix))) (- totalColumns column ) 0 (car buttonsMatrix)) set-label coinRed)
 (set! buttonsMatrix (list (car buttonsMatrix)
                           (insertCoin (- column 1) 1 (cadr buttonsMatrix))))
 (mainAux 2))

;; Gets the button we want in the matrix.
(define (matrixGet row column cont list)
  (cond ((= row cont)
         (matrixGetAux row column 0 (car list)))
  (else
    (matrixGet row column (+ cont 1) (cdr list)))))

(define (matrixGetAux row column cont list)
  (cond ((= column cont)
         (car list))
  (else
    (matrixGetAux row column (+ cont 1) (cdr list)))))


(define x (createBoardMatrix 10 10 '()))
(set! x (cons 0 (car x)))


;; --------------- Building the main ---------------
(define (main)
  ; Show the frame by calling its show method
  (send frame show #t))

(define (mainAux playerOn)
  (cond ((not (= 0 (checkWinner (cadr buttonsMatrix))))
         (checkWinner (cadr buttonsMatrix)))
        ((= playerOn 1)
         (enableButtons #t))
        ((= playerOn 2)
         (enableButtons #f))))

;; --------------- Testing the program -----------------

(main)








