
(in-package :chessboard)

(defclass chess-board-application (application:application)
  ()
  (:documentation "A Chess Board Application"))


(defun new-instance ()
  "Create a new application instance"
  (make-instance 'chess-board-application
                 :title "Chess Board"
                 :tick-interval 50))


(defparameter *square-light-color* '(0.8 0.7 0.7)
  "The color of light chess squares")

(defparameter *square-dark-color* '(0.1 0.1 0.2)
  "The color of dark chess squares")


(defconstant +num-cols+ 8)
(defconstant +num-rows+ 8)

(defun square-color (row col)
  "Sets the gl square color, given a row and col"
  (if (zerop (mod (+ row col) 2))
      (apply #'gl:color *square-light-color*)
      (apply #'gl:color *square-dark-color*)))


(defun chess-board ()
  "Render a chessboard"

  ;; Pre-defined values:
  (let* ((screen-start-x -1)
         (screen-start-y -1)
         (screen-end-x 1)
         (screen-end-y 1)

         ;; Calculated values:
         (screen-width (- screen-end-x screen-start-x))
         (screen-height (- screen-end-y screen-start-y))
         (w (/ screen-width 8))
         (h (/ screen-height 8)))
    
    (render:with-temp-matrix
      (gl:translate screen-start-x screen-start-x 0)
      
      (loop for i upto +num-rows+ do
           (loop for j upto +num-cols+ do
                (square-color i j)
                (render:quad i j w h)))


      ;;(square-color 3 4)
      (render:quad 3 3 w h "BlackQueen")
      )))


(defun load-piece-textures ()
  "Load all known piece textures"

  (let ((pieces '(:pawn :bishop :knight :rook :queen :king))
        (colors '(:white :black)))
    
    (loop for piece in pieces do
         (loop for color in colors do
              (let ((path (format nil "./bitmaps/~:(~a~)~:(~a~)_64.png" color piece))
                    (name (format nil "~:(~a~)~:(~a~)" color piece)))
                (texture:get-texture-from-png-file path name))))))


(defmethod application:application-render ((board chess-board-application))
  (load-piece-textures)
  
  (gl:clear-color 0.5 0.05 0.05 1.0) ;; Nice snake color: (gl:clear-color 0 0.5 0.25 1.0)
  (gl:color 0 1 0)
  (gl:clear :color-buffer)
  (chess-board)

  (glut:swap-buffers))


(application:run-application (new-instance)) 
