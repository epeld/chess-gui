
(defun start-process (name)
  (let ((runtime (jstatic "getRuntime" "java.lang.Runtime")))
    (setf *engine-process*
          (jcall "exec"
                 runtime
                 (jarray-from-list `(,name))
                 (jarray-from-list '("TEST=true"))
                 (jnew "java.io.File" ".")))))


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
  (jcall "getErrorStream" (engine-process engine)))


(defun output-stream (engine)
  (jcall "getOutputStream" (engine-process engine)))


(defun input-stream (engine)
  (jcall "getInputStream" (engine-process engine)))


(defun input-reader (engine)
  "Helper for creating a buffered reader from a process"
  (jnew "java.io.BufferedReader" (jnew "java.io.InputStreamReader" (input-stream engine))))


(defun output-writer (engine)
  "Helper for creating a buffered writer to a process"
  (jnew "java.io.BufferedWriter" (jnew "java.io.OutputStreamWriter" (output-stream engine))))


(defun next-line (engine &optional wait)
  (let ((reader (engine-reader engine)))
    (when (or wait (jcall "ready" reader))
      (let ((line (jcall "readLine" reader)))
        (format t "engine-> ~a~%" line)
        line))))


(defun all-input-lines (engine &optional wait-first)
  (let (lines)
    (do ((line (next-line engine wait-first) (next-line engine)))
        (nil)

      (when (null line)
        (return))
      
      (push line lines))
    (reverse lines)))


(defun write-line (engine string)
  (let ((writer (engine-writer engine)))
    (format t "gui-> ~a~%" string)
    (jcall "write" writer string 0 (jcall "length" string))
    (jcall "newLine" writer)
    (jcall "flush" writer)))


(defun init-engine (engine)
  (write-line engine "uci")
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
    (write-line engine (format nil "position ~s" position))
    (write-line engine cmd)))


(defun engine-stop (engine)
  "Request that the engine stop."
  (write-line engine "stop"))


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
