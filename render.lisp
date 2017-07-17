
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


(defun quad (row col width height &optional texture)
  "Render a quad with the given row/col indexes and a given width/height"

  (if texture
      (progn
        (gl:enable :blend)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (texture:bind texture)
        (gl:enable :texture-2d))
      (gl:disable :texture-2d))
  
  ;;(gl:color 1 1 1)
  (render:with-gl-primitives :quads
    (gl:tex-coord 0 0)
    (gl:vertex (* col width) (* row height))

    (gl:tex-coord 0 1)
    (gl:vertex (* col width) (* (1+ row) height))

    (gl:tex-coord 1 1)
    (gl:vertex (* (1+ col) width) (* (1+ row) height))
    
    (gl:tex-coord 1 0)
    (gl:vertex (* (1+ col) width) (* row height)))
  
  (when texture
    (gl:disable :texture-2d)
    (gl:disable :blend)))

