
(in-package :chessboard)

(defparameter *current-game*
  (make-instance 'queen:game)
  "Foo")

(defclass chess-board-application (application:application)
  ()
  (:documentation "A Chess Board Application"))


(defun new-instance ()
  "Create a new application instance"
  (make-instance 'chess-board-application
                 :title "Chess Board"
                 :tick-interval 100))


(defparameter *square-dark-color*
  (loop for x in '(#x73 #x6b #x5f) collect (float (/ x 255)))
  "The color of light chess squares")


(defparameter *square-light-color*
  (loop for x in '(#xf7 #xd0 #xbc) collect
       (float (/ x 255)))
  "The color of dark chess squares")


(defconstant +num-cols+ 8)
(defconstant +num-rows+ 8)


(defun square-color (row col)
  "Sets the gl square color, given a row and col"
  (if (zerop (mod (+ row col) 2))
      (apply #'gl:color *square-light-color*)
      (apply #'gl:color *square-dark-color*)))


(defun texture-path (color piece)
  "Supply keywords for color and piece, receive texture path to load"
  (format nil "./bitmaps/~:(~a~)~:(~a~)_64.png" color piece))


(defun texture-name (color piece)
  "Supply keywords for color and piece, receive texture name"
  (format nil "~:(~a~)~:(~a~)" color piece))


(defun render-piece (pc row col width height)
  "Helper for rendering a piece (just like render:quad)"
  (let* ((c (queen:piece-char pc))
        
         (color (if (eql c (char-upcase c))
                    :white
                    :black))
        
         (piece (ecase (char-upcase c)
                  (#\B :bishop)
                  (#\R :rook)
                  (#\N :knight)
                  (#\P :pawn)
                  (#\K :king)
                  (#\Q :queen))))

    (gl:color 1 1 1)
    (render:quad row col width height
                 (texture-name color piece))))


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
         (h (/ screen-height 8))

         (b (queen:game-board *current-game*)))
    
    (render:with-temp-matrix
      (gl:translate screen-start-x screen-start-x 0)
      
      (loop for i below +num-rows+ do
           (loop for j below +num-cols+ do
                (square-color i j)
                (render:quad i j w h)

                (let ((pc (queen:board-get b (queen:board-index i j))))
                  (unless (zerop pc)
                    (render-piece pc i j w h))))))))


(defun load-piece-textures ()
  "Load all known piece textures"

  (let ((pieces '(:pawn :bishop :knight :rook :queen :king))
        (colors '(:white :black)))
    
    (loop for piece in pieces do
         (loop for color in colors do
              (let ((path (texture-path color piece))
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
