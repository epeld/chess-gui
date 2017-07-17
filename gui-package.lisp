
(ql:quickload :cl-glut)
(ql:quickload :cl-glu)
(ql:quickload :zpng)
(ql:quickload :png)

(defpackage :foobar.render
  (:use :cl)
  (:export :with-temp-matrix
           :with-gl-primitives
           :quad)
  (:nicknames :render))

(defpackage :foobar.window
  (:use :cl)
  (:export :event-loop)
  (:nicknames :window))

(defpackage :foobar.application
  (:use :cl)
  (:export :application :run-application
           :application-render
           :application-handle-event)
  (:nicknames :application))


(defpackage :foobar.chessboard
  (:use :cl)
  (:nicknames :chessboard))


(defpackage :foobar.texture
  (:use :cl)
  (:nicknames :texture))
