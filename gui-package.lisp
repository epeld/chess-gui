
(ql:quickload :cl-glut)
(ql:quickload :cl-glu)
(ql:quickload :zpng)
(ql:quickload :png)
(ql:quickload :queen)

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
  (:export :bind
           :get-texture-from-png-file
           :get-texture-by-name
           :delete-textures)
  (:nicknames :texture))
