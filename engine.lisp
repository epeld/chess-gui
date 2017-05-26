
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


(defun next-line ()
  (when (jcall "ready" (input-reader))
    (jcall "readLine" (input-reader))))


(defun all-input-lines ()
  (do ((line (next-line) (next-line))
       lines)
      ()

    (when (null line)
      (return (reverse lines)))
    
    (push line lines)))


(defun write-line (string)
  (jcall "write" (output-writer) string 0 (jcall "length" string))
  (jcall "newLine" (output-writer))
  (jcall "flush" (output-writer)))


;(write-line "isready")
;(all-input-lines)
;(next-line)

; (start-process)
