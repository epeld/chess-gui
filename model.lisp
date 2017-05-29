

;;
;;  App Data Model
;;
(defclass data-model ()
  ((subscribers :documentation "List of lambdas to call when notifications go out"
               :type list
               :accessor model-subscribers
               :initform nil)
   (engine :documentation "An instance of the engine class"
           :type engine
           :accessor model-engine
           :initarg :engine)
   (stop-timers :documentation "Flag indicating if we should stop background tasks"
                :type boolean
                :accessor model-stop-timersp
                :initform nil)
   (position :documentation "The current position. Sent to engine when analysis starts"
             :type string
             :accessor model-position
             :initform "startpos"))
  (:documentation "Container for all the data in the application. Also manages notifications for when data changes"))



;;
;; Subscription stuff
;; 
(defun subscribe (model fn)
  "Add a notification subscriber to the model. Fn should accept one arguments: the notification (e.g a list of keywords or nil)"
  (push fn (model-subscribers model)))


(defun notify (model &optional namespace)
  "Send out a notification to all subscribers. If notification is specified, subscribers can use that to filter out notifications if they don't want to listen to all of them"
  ;; (format t "Notifying ~s~%" namespace)
  (loop for s in (model-subscribers model) do
       (handler-case
           (funcall s namespace)
         (error (err)
           (warn "Error occured when notifying ~s ~a~%" namespace err)))))

(defun create-model (engine)
  (make-instance 'data-model
                 :engine engine))


(defvar *current-model* nil)

;;
;;  Analysis
;;

(defun analysis-bestmove-no-ponder (analysis)
  "Return the concluded bestmove variation (sans any suggested ponder move)"
  (let ((bestmove (analysis-bestmove analysis)))
    (unless (eql :unknown bestmove)
      (subseq bestmove 0 (search " ponder" bestmove)))))

(defun start-analysis (model)
  "Start analysis, notifying listeners that state is now :RUNNING"
  (let ((s (engine-state (model-engine model))))
    (unless (eql :idle s)
      (error "Cannot start analysis while engine is ~a" s))  )

  (engine-go (model-engine model) (model-position model))
  (notify model '(:engine :state)))


(defun stop-analysis (model)
  "Requests that the engine STOP. Note that the engine will not stop immediately, so no state-notification will go out synchroneously from this call"
  (engine-stop (model-engine model)))


;;
;; Message Loop Utils
;;

;; TODO this belongs partially in engine-file. We need to split it somehow
(defun model-handle-message (model message)
  (let ((engine (model-engine model)))
    (cond
      ((prefixedp "info" message)
       (format nil "Engine info message '~a'~%" message))
    
      ((prefixedp "bestmove" message)
       (setf (analysis-bestmove (engine-analysis engine))
             (subseq message (length "bestmove ")))
       (setf (engine-state engine) :idle)
       (notify model '(:engine :state)))

      (t
       (warn "Unrecognized Engine Message ~s" message)))))


(defun process-pending-messages (model)
  "Read all output input from the engine and update our state"
  (let ((lines (all-input-lines (model-engine model))))
    (when lines
      (loop for line in lines do
           (handler-case
               (model-handle-message model line)
             (error ()
               (warn "Error on engine message ~s" line)))))))
