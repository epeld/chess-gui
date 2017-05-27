
(defvar *engine-process* nil)

(defvar *buffered-reader* nil)
(defvar *buffered-writer* nil)

(defclass analysis ()
  ((bestmove :documentation "The suggested best move in the current position"
             :type string
             :accessor analysis-bestmove
             :initform :unknown)
   (pvs :documentation "A list of variations"
        :accessor analysis-pvs
        :initform nil))
  (:documentation "An analysis is started with the engine command 'go' and stops when reciving 'bestmove'"))

(defclass engine ()
  ((process :documentation "Handle to external engine process"
            :type process
            :accessor engine-process)
   (reader :documentation "Java class Buffered Reader (for reading from the engine)"
           :accessor engine-reader)
   (writer :documentation "Java class Buffered Writer (for writing to from engine)"
           :accessor engine-writer)
   (state :documentation "The last known state of the engine"
           :type keyword
           :accessor engine-state
           :initform :not-initialized)
   (analysis :documentation "The current 'analysis'-report"
             :type analysis
             :accessor engine-analysis
             :initform nil))
  (:documentation "Everything you need to talk to a UCI engine"))

(defun prefixedp (prefix string)
  (string-equal prefix string :end2 (length prefix)))


(defun engine-handle-message (engine message)
  (cond
    ((prefixedp "info")
     (format t "Engine info message '~a'~%" message))
    
    ((prefixedp "pv")
     ;; TODO handle a pv
     (warn "unparsed pv '~a' ~%" message))

    ((prefixedp "bestmove")
     (setf (analysis-bestmove (engine-analysis engine))
           (subseq message (length "bestmove ")))
     (setf (engine-state engine) :idle))))


(defun start-process ()
  (let ((runtime (jstatic "getRuntime" "java.lang.Runtime")))
    (setf *engine-process*
          (jcall "exec"
                 runtime
                 (jarray-from-list '("stockfish"))
                 (jarray-from-list '("TEST=true"))
                 (jnew "java.io.File" ".")))))



(defun error-stream ()
  (jcall "getErrorStream" *engine-process*))

(defun output-stream ()
  (jcall "getOutputStream" *engine-process*))

(defun input-stream ()
  (jcall "getInputStream" *engine-process*))


(defun input-reader ()
  (unless *buffered-reader*
    (setf *buffered-reader* (jnew "java.io.BufferedReader" (jnew "java.io.InputStreamReader" (input-stream)))))
  *buffered-reader*)


(defun output-writer ()
  (unless *buffered-writer*
    (setf *buffered-writer* (jnew "java.io.BufferedWriter" (jnew "java.io.OutputStreamWriter" (output-stream)))))
  *buffered-writer*)


(defun next-line (&optional wait)
  (when (or wait (jcall "ready" (input-reader)))
    (jcall "readLine" (input-reader))))


(defun all-input-lines (&optional wait-first)
  (do ((line (next-line wait-first) (next-line))
       (lines nil))
      (t)

    (when (null line)
      (return (reverse lines)))
    
    (push line lines)))


(defun write-line (string)
  (jcall "write" (output-writer) string 0 (jcall "length" string))
  (jcall "newLine" (output-writer))
  (jcall "flush" (output-writer)))


;;(write-line "isready")
;;(all-input-lines)
;;(next-line)



;; (start-process)


(defun init-engine ()
  (write-line "uci")
  (do ((line (next-line t) (next-line t))
       (count 0 (1+ count))
       (options nil))
      ()
    
    (when (string= "uciok" line)
      (return options))

    (push (parse-option line)
          options)))


(defparameter *stockfish-options*
  "option name Use Debug Log type check default false
option name Use Search Log type check default false
option name Search Log Filename type string default SearchLog.txt
option name Book File type string default book.bin
option name Best Book Move type check default false
option name Contempt Factor type spin default 0 min -50 max 50
option name Mobility (Middle Game) type spin default 100 min 0 max 200
option name Mobility (Endgame) type spin default 100 min 0 max 200
option name Passed Pawns (Middle Game) type spin default 100 min 0 max 200
option name Passed Pawns (Endgame) type spin default 100 min 0 max 200
option name Space type spin default 100 min 0 max 200
option name Aggressiveness type spin default 100 min 0 max 200
option name Cowardice type spin default 100 min 0 max 200
option name Min Split Depth type spin default 4 min 4 max 12
option name Max Threads per Split Point type spin default 5 min 4 max 8
option name Threads type spin default 2 min 1 max 64
option name Use Sleeping Threads type check default true
option name Hash type spin default 32 min 1 max 8192
option name Clear Hash type button
option name Ponder type check default true
option name OwnBook type check default false
option name MultiPV type spin default 1 min 1 max 500
option name Skill Level type spin default 20 min 0 max 20
option name Emergency Move Horizon type spin default 40 min 0 max 50
option name Emergency Base Time type spin default 200 min 0 max 30000
option name Emergency Move Time type spin default 70 min 0 max 5000
option name Minimum Thinking Time type spin default 20 min 0 max 5000
option name Slow Mover type spin default 100 min 10 max 1000
option name UCI_Chess960 type check default false
option name UCI_AnalyseMode type check default false")


(defclass option ()
  ((name :type string
         :documentation "Option name"
         :accessor option-name
         :initarg :name)))


(defclass button (option)
  ())

(defclass check (option)
  ((value :documentation "Option value"
          :accessor option-value
          :initarg :value)))

(defclass string-option (option)
  ((value :documentation "Option value"
          :accessor option-value
          :initarg :value)))

(defclass spin (option)
  ((value :documentation "Option value"
          :accessor option-value
          :initarg :value)
   (min :documentation "Min Value"
        :accessor option-min-value
        :initarg :min)
   (max :documentation "Max Value"
        :accessor option-max-value
        :initarg :max)))


(defun parse-min (string)
  (let ((min (+ 1 (length "min") (search "min" string))))
    (parse-integer
     (subseq string
             min
             (search " " string :start2 min)))))


(defun parse-max (string)
  (let ((max (+ 1 (length "max") (search "max" string))))
    (parse-integer
     (subseq string
             max
             (search " " string :start2 max)))))

(defun parse-option-specific (type name value rest)
  (ecase type
    (:spin (make-instance 'spin :name name :value (parse-integer value) :min (parse-min rest) :max (parse-max rest)))
    (:string (make-instance 'string-option :name name :value value))
    (:check (make-instance 'check :name name :value (string-equal "true" value)))))

(defun parse-option (string)
  (let* ((name (search "name" string))
         (type (search "type" string :start2 name))
         (default (search "default" string :start2 type))
         name-end type-end default-end rest)

    (unless (and name type)
      (error "Invalid option '~a'" string))

    ;;
    ;; Move to the end of each respective string
    ;;
    (setf name-end (+ name (length "name") 1))
    (setf type-end (+ type (length "type") 1))

    (when default
      (setf default-end (+ default (length "default") 1))
      (setf rest (search " " string :start2 default-end)))

    (let ((name-str (subseq string name-end (- type 1)))
          (type-str (subseq string type-end (if default (- default 1) (length string))))
          default-str remainder)

      ;; Special case
      (if (string-equal "button" type-str)
          (make-instance 'button :name name-str)

          ;; Default case
          (progn
            (setf default-str (subseq string default-end rest))
            (setf remainder (if rest (subseq string (1+ rest)) nil))

            (parse-option-specific (find type-str '(:check :spin :string)
                                         :test #'string-equal)
                                   name-str default-str remainder))))))


;;(parse-option "option name UCI_AnalyseMode type check default false")
;;(parse-option "option name Book File type string default book.bin")
;;(parse-option "option name Slow Mover type spin default 100 min 10 max 1000")


(defun make-panel (&rest contents)
  (let ((panel (jnew "javax.swing.JPanel")))
    (loop for component in contents do
         (jcall "add" panel component))
    panel))


(defun south ()
  (jfield (jclass "java.awt.BorderLayout") "SOUTH"))

(defun north ()
  (jfield (jclass "java.awt.BorderLayout") "NORTH"))

(defun center ()
  (jfield (jclass "java.awt.BorderLayout") "CENTER"))

(defun west ()
  (jfield (jclass "java.awt.BorderLayout") "WEST"))

(defun east ()
  (jfield (jclass "java.awt.BorderLayout") "EAST"))

(defun create-frame ()
  (let ((frame (jnew "javax.swing.JFrame" "Engine Analysis"))
        content

        (fen-label (jnew "javax.swing.JLabel" "Position"))
        (fen-field (jnew "javax.swing.JTextField" *initial-fen*))
        
        (go-btn (jnew "javax.swing.JButton" "Start"))
        (settings-btn (jnew "javax.swing.JButton" "Settings"))
        (log-btn (jnew "javax.swing.JButton" "Engine Logs"))

        (layout (jnew "java.awt.BorderLayout"))

        (list (jnew "javax.swing.JList" (jarray-from-list '("foo" "bar")))))

    (setf content (jcall "getContentPane" frame))

    (jcall "setLayout" content layout)
    
    ;; FEN
    (jcall "setLabelFor" fen-label fen-field)
    (jcall "add" content (make-panel fen-label fen-field) (north))

    ;; Center
    ;;(jcall "add" content (jnew "javax.swing.JLabel" "TODO" (jfield (jclass "javax.swing.SwingConstants") "CENTER")) (center))
    (jcall "add" content list (center))
    (jcall "setSize" list 200 200)
    (jcall "setVisibleRowCount" list 5)

    ;; Buttons
    (jcall "add" content (make-panel go-btn settings-btn log-btn) (south))

    (jcall "setVisible" frame t)
    (jcall "pack" frame)))


; (create-frame)

(defvar *initial-fen* "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")


(defgeneric option-widget (t)
  (:documentation "Create a widget used to control the option"))

(defmethod option-widget ((s button))
  (jnew "javax.swing.JButton" "OK"))

(defmethod option-widget ((s string-option))
  (jnew "javax.swing.JTextField" (option-value s) 20))

(defmethod option-widget ((s spin))
  (let ((slider (jnew "javax.swing.JSlider" (option-min-value s) (option-max-value s) (option-value s))))
    (jcall "setPaintLabels" slider t)
    (jcall "setPaintTicks" slider t)
    (jcall "setLabelTable" slider (jcall "createStandardLabels" slider (ceiling (/ (- (option-max-value s) (option-min-value s)) 5))))
    slider))

(defmethod option-widget ((s check))
  (let ((b (jnew "javax.swing.JCheckBox")))
    (jcall "setSelected" b (option-value s))
    b))


(defun labeled (string component)
  (let ((panel (jnew "javax.swing.JPanel"))
        (lbl (jnew "javax.swing.JLabel" string)))
    (jcall "setLabelFor" lbl component)
    (jcall "add" panel lbl (west))
    (jcall "add" panel component (center))

    ;(jcall "setLayout" panel (jnew "java.awt.FlowLayout"))

    (jcall "setSize" panel 300 30)
    panel))

(defun create-options-frame (options)
  (let ((frame (jnew "javax.swing.JFrame" "Engine Settings")))

    ;(jcall "setLayout" frame (jnew "javax.swing.BoxLayout" (jcall "getContentPane" frame) (jfield "javax.swing.BoxLayout" "Y_AXIS")))
    (jcall "setLayout" frame (jnew "java.awt.GridLayout" (ceiling (length options) 3) 3))
    
    (loop for option in options do
         
       (let ((w (make-panel (labeled (option-name option) (option-widget option)))))
         (jcall "add" frame w)))

    (jcall "setVisible" frame t)
    (jcall "pack" frame)))

(defun split-lines (string)
  (do* ((prev 0 (+ 1 space))
        (space (search '(#\Newline) string) (search '(#\Newline) string :start2 prev))
        (lines nil (cons (subseq string prev space) lines)))
       ()
    (when (null space)
      (return (reverse lines)))))

;; TODO:
(create-options-frame (loop for line in (split-lines *stockfish-options*)
                         collect (parse-option line)))

