
(defvar *engine-process* nil)

(defvar *buffered-reader* nil)
(defvar *buffered-writer* nil)

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
       lines)
      ()

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
       options)
      ()
    
    (when (string= "uciok" line)
      (return))

    (push (parse-option line)
          options)))


(defvar *stockfish-options*
  "
option name Use Debug Log type check default false
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
option name UCI_AnalyseMode type check default false
")
