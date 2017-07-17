
(in-package :application)

(defvar *current-application* nil
  "The current application (you typically only run one at a time)")

(defclass application ()
  ((title :accessor application-title
          :initarg :title
          :initform (error "Must supply a title"))
   (tick-interval :accessor application-tick-interval
                  :initform 100
                  :initarg :tick-interval))
  (:documentation "Application class"))


(defgeneric application-handle-event (application t))
(defgeneric application-render (application))

(defun run-application (application)
  "Run an application"
  (setf *current-application* application)
  (unwind-protect
       (window:event-loop :title (application-title application)
                          :event-callback (lambda (event)
                                            (application-handle-event application event))
                          :renderer (lambda ()
                                      (application-render application))
                          :tick-interval (application-tick-interval application))
    (setf *current-application* nil)))



(defmethod application-handle-event ((app application) event)
  (unless (eq (first event) :tick)
    (format t "Got event ~a~%" event))
  (when (eq (first event) :keydown)
    (quote (cerror "continue?" "BAM")))

  t)


(defmethod application-render ((app application))
  ;(format t "Rendering~%")
  (gl:color 0 0.5 0)
  (gl:clear-color 0 0.5 0 1.0)
  (gl:clear :color-buffer)
  (glut:swap-buffers))
