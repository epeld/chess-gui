
(in-package :chess.engine.gui)

(defvar *initial-fen* "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

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

(defun create-frame (model)
  (let ((frame (jnew "javax.swing.JFrame" "Engine Analysis"))
        content

        ;;(fen-label (jnew "javax.swing.JLabel" "Position"))
        (fen-field (jnew "javax.swing.JTextField" *initial-fen*))
        
        (go-btn (jnew "javax.swing.JButton" "Start"))
        (stop-btn (jnew "javax.swing.JButton" "Stop"))
        
        (settings-btn (jnew "javax.swing.JButton" "Settings"))
        (log-btn (jnew "javax.swing.JButton" "Engine Logs"))

        (layout (jnew "java.awt.BorderLayout"))

        (list (jnew "javax.swing.JList")))

    (setf content (jcall "getContentPane" frame))

    (jcall "setLayout" content layout)
    
    ;; FEN
    (jcall "add" content (labeled "Position" fen-field) (north))

    ;; Center
    ;;(jcall "add" content (jnew "javax.swing.JLabel" "TODO" (jfield (jclass "javax.swing.SwingConstants") "CENTER")) (center))
    (jcall "add" content list (center))
    (jcall "setSize" list 200 200)
    (jcall "setVisibleRowCount" list 5)
    (subscribe model
               (lambda (n)
                 (declare (ignore n))
                 (update-analysis-list list (engine-analysis (model-engine model)))
                 (jcall "pack" frame)))

    ;; Buttons
    (jcall "add" content (make-panel go-btn stop-btn settings-btn log-btn) (south))
    (subscribe model
               (lambda (n)
                 (declare (ignore n))
                 (let ((r (runningp (model-engine model))))
                   (jcall "setVisible" go-btn (not r))
                   (jcall "setVisible" stop-btn r)
                   (jcall "setEnabled" settings-btn (not r))
                   (jcall "setEnabled" log-btn (not r)))))

    (jcall "setVisible" log-btn nil)
    
    (jcall "addActionListener" go-btn
           (jinterface-implementation
            "java.awt.event.ActionListener"

            "actionPerformed"
            (lambda (ev)
              (declare (ignore ev))
              (start-analysis model))))

    (jcall "addActionListener" stop-btn
           (jinterface-implementation
            "java.awt.event.ActionListener"

            "actionPerformed"
            (lambda (ev)
              (declare (ignore ev))
              (stop-analysis model))))

    (jcall "setVisible" frame t)
    (jcall "pack" frame)
    (notify model)))


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

    ;;(jcall "setLayout" panel (jnew "java.awt.FlowLayout"))

    ;;(jcall "setSize" panel 300 30)
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


(defun update-analysis-list (list analysis)
  (let ((bestmove (analysis-bestmove-no-ponder analysis)))
    (jcall "setListData" list (jarray-from-list (list (or bestmove "??"))))))


(defun periodic-read (timer model)
  ;; (format t "Tick~%")
  (when (model-stop-timersp model)
    (format t "Stopping timer~%")
    (jcall "stop" timer))

  (process-pending-messages model))


(defun install-periodic-timer (model)
  "Construct a timer that will periodically poll and parse messages from the engine"
  (let (timer)

    (setf timer (jnew "javax.swing.Timer" 300
                      (jinterface-implementation
                       "java.awt.event.ActionListener"

                       "actionPerformed"
                       (lambda (ev)
                         (declare (ignore ev))
                         (periodic-read timer model)))))
    
    (jcall "start" timer)))


(defun init-data-layer ()
  "Helper. Set up the pre-requisite data-layer in order to create GUI"
  (let* ((engine (start-engine "stockfish"))
         (model (create-model engine)))

    (install-periodic-timer model)
    (setf *current-model* model)
    model))

;;
;; Top Level 'Main'
;;

(defun run-app ()
  (create-frame (init-data-layer)))
