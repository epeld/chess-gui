
(in-package :render)

(defmacro with-temp-matrix (&body body)
  "Pushes the current matrix, executes body, then pops"
  `(progn (gl:push-matrix)
          (unwind-protect
               (progn ,@body))
          (gl:pop-matrix)))

(defmacro with-gl-primitives (primitive-type &body body)
  "Wrap body in a (gl:begin primitive-type) (gl:end) pair"
  `(progn (gl:begin ,primitive-type)
          (unwind-protect
               (progn ,@body)
            (gl:end))))

(defmacro with-object-coords (object &rest clauses)
  "Temporarily switch to an object-centric coordinate system.
Expects clauses of the form: (primitive-type body*) which will be used in with-gl-primitives to set up a gl rendering context."
  `(with-temp-matrix
     (gl-object-transform ,object)
     ,@(loop for clause in clauses
          unless
            (eq (first clause)
                :ignore)
          collect
            `(with-gl-primitives ,(first clause)
               ,@ (rest clause)))))


(declaim (inline gl-vertex-unit))
(defun gl-vertex-unit (radians &optional (radius 1.0))
  "Use gl:vertex with a point from the x-y plane unit circle"
  (gl:vertex (* radius (cos radians)) (* radius (sin radians)) 0))


(defun quad (row col width height)
  "Render a quad with the given row/col indexes and a given width/height"
  (render:with-gl-primitives :quads
    (gl:vertex (* col width) (* row height))
    (gl:vertex (* col width) (* (1+ row) height))
    (gl:vertex (* (1+ col) width) (* (1+ row) height))
    (gl:vertex (* (1+ col) width) (* row height))))
