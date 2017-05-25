
;;
;; Utils
;;

(defun run-in-gui-thread (fn)
  (handler-case
      (jstatic "invokeLater"
               (jclass "javax.swing.SwingUtilities")
               (jinterface-implementation
                "java.lang.Runnable"

                "run"
                (lambda ()
                  (format t "Running block in GUI thread..~%")
                  (funcall fn))))
    (error (e)
      (format t "Could have caught that")
      (error e))))


(defmacro gui-progn (&body body)
  `(run-in-gui-thread (lambda ()
                        ,@body)))



;;
;; This defines a chessboard GUI using ABCL (Common Lisp)
;;

(defvar *light-square-color*
  #xf7d0bc)

(defparameter *light-square-hightlight-color*
  #x856d91)

(defvar *dark-square-color*
  #x736b5f)

(defparameter *dark-square-highlight-color*
  #x856d91)

;;
;;  Images
;;

(defvar *bitmap-directory* "./bitmaps")

(defun get-image-name (color piece size)
  "Inernal function used to determine which bitmap to load"
  (format nil "~a/~a~a_~a.png"
          *bitmap-directory*
          (string-capitalize color)
          (string-capitalize piece)
          size))

(defvar *image-cache* nil)

(defun get-image (color piece &optional (size 64))
  "Get an image by looking in the cache"
  (let ((a (assoc `(,color ,piece ,size) *image-cache* :test #'equal)))
    (if a
        (cdr a)
        (progn
          (push `((,color ,piece ,size) . ,(load-image color piece size))
                *image-cache*)
          (cdar *image-cache*)))))


(defun load-image (color piece size)
  "Low-level method for actually loading an image from disk"
  (handler-case
      (jstatic "read"
               (jclass "javax.imageio.ImageIO")
               (jnew "java.io.File" (get-image-name color piece size)))
    (warning ()
      nil)))


(defun load-piece-set (size)
  "Ensure an entire piece set is loaded into cache"
  (loop for piece in '(:pawn knight king queen rook bishop)
     do (and (get-image :white piece size)
             (get-image :black piece size))))


;;
;; GUI
;;

(defun make-move (board sq sq2)
  ;; Transfer image from sq to sq2
  (let ((image (jcall "getImage" (square-icon sq))))
    (when image
      (square-clear-piece sq)
      (jcall "setImage" (square-icon sq2) image)
      (jcall "setVisible" (square-icon-label sq2) t)))

  (setf (board-typed-chars board) nil)
  (setf (board-selected-square board) nil))

(defclass square ()
  ((panel :documentation "JPanel"
          :accessor square-internal-panel
          :initarg :panel)
   (icon :documentation "ImageIcon"
         :accessor square-icon
         :initarg :icon)
   (icon-label :documentation "JLabel containing ImageIcon"
               :accessor square-icon-label
               :initarg :icon-label)
   
   (index :documentation "Square index"
          :accessor square-index
          :initarg :index))
  
  (:documentation "Holds info about a square"))

(defclass chessboard ()
  ((frame :documentation "JFrame"
          :accessor chessboard-frame
          :initarg :frame)
   (squares :documentation "Array of JPanel-squares"
            :accessor chessboard-squares
            :initarg :squares)

   (selected-square :documentation "The last square clicked"
                    :accessor board-selected-square
                    :initform nil)

   (typed-chars :documentation "All typed characters, in reverse order"
                :accessor board-typed-chars
                :initform nil))
  
  (:documentation "Collects handles to resources related to chessboard"))


(defun square-color (index)
  "Determine if a square is dark or light"
  (multiple-value-bind (r c) (floor index 8)
    (if (zerop (mod (+ r c) 2))
        :dark
        :light)))

(defun square-gui-color (index)
  (ecase (square-color index)
    (:dark (jnew "java.awt.Color" *dark-square-color*))
    (:light (jnew "java.awt.Color" *light-square-color*))))

(defun square-gui-highlight-color (index)
  (ecase (square-color index)
    (:dark (jnew "java.awt.Color" *dark-square-highlight-color*))
    (:light (jnew "java.awt.Color" *light-square-hightlight-color*)))
  )

(defun make-square (index)
  "Construct a fancy square"
  (let* ((panel (jnew "javax.swing.JPanel"))
         (icon (jnew "javax.swing.ImageIcon"))
         (label (jnew "javax.swing.JLabel" icon)))
    (jcall "setBackground" panel (square-gui-color index))
    ;(jcall "add" panel (jnew "javax.swing.JLabel" (format nil "~a" index)))
    (jcall "add" panel label)

    (make-instance 'square
                   :panel panel
                   :icon icon
                   :icon-label label
                   :index index)))


(defun square-set-piece (square color piece)
  (let ((icon (square-icon square))
        (icon-label (square-icon-label square)))
    (jcall "setImage" icon (get-image color piece))
    (jcall "setVisible" icon-label t)))

(defun square-clear-piece (square)
  (let ((icon-label (square-icon-label square)))
    (jcall "setVisible" icon-label nil)))


(defun chessboard-find-square-with-point (board pt)
  (format t "Looking for point (~a, ~a)~%" (jcall "getY" pt) (jcall "getY" pt))
  (loop for ix from 0 upto 63
     when (jcall "contains" (square-internal-panel (chessboard-square board ix))
                 pt)
     do (return (chessboard-square board ix))))


(defun make-frame ()
  "Construct a fancy frame"
  (let (board
        (frame (jnew "javax.swing.JFrame" "Fancy Chessboard"))
        (layout (jnew "java.awt.GridLayout" 8 8))
        (squares (loop for i from 0 upto 63 collect
                      (make-square i))))
    
    (jcall "setSize" frame (* 64 9) (* 64 9))
    (jcall "setLayout" frame layout)
   

    (loop for sq in squares do
         (jcall "add" frame (square-internal-panel sq))
         (jcall "addMouseListener" (square-internal-panel sq)
                (let ((sq sq))
                  (jinterface-implementation
                   "java.awt.event.MouseListener"

                   "mouseClicked"
                   (lambda (ev)
                     (declare (ignore ev))
                     (format t "Mouse clicked!~%")
                     (chessboard-clicked board sq))

                   "mouseExited"
                   (lambda (ev)
                     (declare (ignore ev))
                     (unless (eql sq (board-selected-square board))
                       (square-highlight sq nil)))

                   "mouseEntered"
                   (lambda (ev)
                     (declare (ignore ev))
                     (square-highlight sq))

                   "mousePressed"
                   (lambda (ev)
                     (declare (ignore ev))
                     :TODO)

                   "mouseReleased"
                   (lambda (ev)
                     (declare (ignore ev))
                     :TODO)))))

                                        ; (jcall "setDefaultCloseOperation" frame (jfield "javax.swing.JFrame" "EXIT_ON_CLOSE"))

    (jcall "addKeyListener" frame
           (jinterface-implementation
            "java.awt.event.KeyListener"
            
            "keyPressed"
            (lambda (kev)
              (declare (ignore kev)))

            "keyReleased"
            (lambda (kev)
              (declare (ignore kev)))

            "keyTyped"
            (lambda (ev)
              (let ((c (jcall "getKeyChar" ev)))
                (board-something-typed board c)))))

    
    (jcall "setVisible" frame t)
    (jcall "setResizable" frame nil)
    (jcall "revalidate" frame)
        
    
    (setf board
          (make-instance 'chessboard
                         :frame frame
                         :squares (make-array '(64)
                                              :initial-contents squares)))))


(defun chessboard-square (board index)
  (aref (Chessboard-squares board) index))



(defun color-from-character (c)
  (if (eql c (char-upcase c))
      :white
      :black))

(defun piece-from-character (c)
  (ecase (char-upcase c)
    (#\K :king)
    (#\Q :queen)
    (#\R :rook)
    (#\N :knight)
    (#\B :bishop)
    (#\P :pawn)))

(defun number-from-character (c)
  (if (eql #\/ c)
      0
      (let ((n (search `(,c) "123456789")))
        (if n
            (+ 1 n)
            (error "Not a digit ~a" c)))))


(defun board-something-typed (board c)
  (when (search `(,c) "abcdefgh12345678x=BNRQKbnrqk-Oo")
    (push c (board-typed-chars board))
    (format t "Typed so far: ~a~%" (coerce (reverse (board-typed-chars board))
                                           'string))))

(defun chessboard-clicked (board square)
  "Called when a square has been clicked by the user"

  (if (board-selected-square board)
      (progn
        (unless (eql (board-selected-square board) square)
          ;; Two squares have been clicked.
          ;; Time to make a move!
          (make-move board (board-selected-square board) square))
        (setf (board-selected-square board) nil))
      
      
      (progn
        (setf (board-selected-square board) square)

        ;; Remove current highlighting
        (loop for ix from 0 upto 63 do
             (square-highlight (Chessboard-square board ix) nil))
  
        (square-highlight square))))


(defun chessboard-display-fen (board fen)
  (let ((ix 0))
    (loop for c across fen
       until (eql #\Space c)
       do (if (find c "NBRQKPnbrqkp")
              ;; Piece:
              (progn (square-set-piece (chessboard-square board ix)
                                       (color-from-character c)
                                       (piece-from-character c))
                     (incf ix))

              ;; Spaces:
              (loop for i from 0 upto (number-from-character c) do
                   (square-clear-piece (chessboard-square board (+ ix i)))
                 finally
                   (incf ix (number-from-character c)))))))


(defun square-highlight (sq &optional (on t)) 
  (let ((panel (square-internal-panel sq)))
    (if on
        (jcall "setBackground" panel (square-gui-highlight-color (square-index sq)))
        (jcall "setBackground" panel (square-gui-color (square-index sq))))))



;;
;; Test Code
;;

(defvar *current-frame* nil)

(gui-progn
  (let ((b (make-frame)))
    (Chessboard-display-fen b "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"       )
    (setf *current-frame* b)
    ))
