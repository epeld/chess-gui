
(in-package :chess-engine)


(defvar *engine-process* nil
  
  "Reference to a running chess analysis engine")

(defun start-process (name)
  (setf *engine-process* (sb-ext:run-program name nil)))


(defun start-engine (&optional (name "stockfish"))
  "Start an engine process, returning an instance of the engine class"
  (let* ((p (start-process name))
         (engine (make-instance 'engine :process p)))

    (setf (engine-reader engine) (input-reader engine))
    (setf (engine-writer engine) (output-writer engine))

    ;; Read the version info directly
    (format t "Started Analysis Engine '~a'~%" (next-line engine t))
    (init-engine engine)
    (format t "UCI Initialized. Options parsed~%")
    
    engine))


(defun error-stream (engine)
  "Return the engine error stream"
  (sb-ext:process-error (chess.engine.types:engine-process engine)))


(defun output-stream (engine)
  "Return a writable stream for sending messages to the engine (output for US)"
  (sb-ext:process-input (engine-process engine)))


(defun input-stream (engine)
  "Return a readable stream for reading messages from the engine (input for US)"
  (sb-ext:process-output (engine-process engine)))


(defun input-reader (engine)
  "Helper for creating a buffered reader from a process"
  (input-stream engine))


(defun output-writer (engine)
  "Helper for creating a buffered writer to a process"
  (output-stream engine))


(defun next-line (engine &optional wait)
  "Read a single line of the engine's output, optionally blocking until a line is availble (or returning NIL)"
  (let ((reader (engine-reader engine)))
    (when (or wait (sb-gray:stream-listen reader))
      (let ((line (read-line reader)))
        (format t "engine-> ~a~%" line)
        line))))


(defun all-input-lines (engine &optional wait-first)
  "Read all available input lines, optionally blocking until at least one line is available"
  (let (lines)
    (do ((line (next-line engine wait-first) (next-line engine)))
        (nil)

      (when (null line)
        (return))
      
      (push line lines))
    (reverse lines)))


(defun send-line (engine string)
  "Send a single line of input to the engine"
  (let ((writer (engine-writer engine)))
    (format t "gui-> ~a~%" string)
    (write-line string writer)
    (force-output writer)))


(defun init-engine (engine)
  (send-line engine "uci")
  (let (options)
    (do ((line (next-line engine t) (next-line engine t))
         (count 0 (1+ count)))
        (nil)
    
      (when (string= "uciok" line)
        (setf (engine-state engine) :idle)
        (return))

      (handler-case
          (push (parse-option line)
                options)
        (type-error ()
          (warn "Failed to parse ~s" line))))

    (setf (engine-options engine) options)
    options))


(defun engine-go (engine position &optional go-args)
  "Run the engine, sending the position command first to make sure we are analysing the right position"
  (let ((cmd (if go-args
                 (concatenate 'string "go " go-args)
                 "go")))
    (setf (engine-state engine) :running)
    (setf (engine-analysis engine) (make-instance 'analysis))
    (send-line engine (format nil "position ~s" position))
    (send-line engine cmd)))


(defun engine-stop (engine)
  "Request that the engine stop."
  (send-line engine "stop"))


(defun runningp (engine)
  (eql :running (engine-state engine)))


(defun engine-handle-message (engine message)
  "Handle a message received from the engine"
  (cond
    ((prefixedp "info" message)
     (format nil "Engine info message '~a'~%" message))

    ;; TODO handle pv!
    
    ((prefixedp "bestmove" message)
     ;; TODO consider parsing the bestmove line here first..
     (setf (analysis-bestmove (engine-analysis engine))
           (subseq message (length "bestmove ")))
     (setf (engine-state engine) :idle))

    (t
     (warn "Unrecognized Engine Message ~s" message))))
