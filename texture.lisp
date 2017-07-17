

(in-package :texture)

;; For loading a texture:
;;https://stackoverflow.com/questions/12518111/how-to-load-a-bmp-on-glut-to-use-it-as-a-texture


(defvar *textures* nil
  "alist of (texture-name . texture-id) ")


(defun read-png-from-file (file-path)
  "Load a png from a file"
  (with-open-file (input file-path
                         :element-type '(unsigned-byte 8))
    (png:decode input)))


(defun make-texture-from-png (png)
  "Create a new opengl texture from a loaded png"
  (let ((tex (first (gl:gen-textures 1))))
    (format t "Got texture ~a~%" tex)
    (unless tex
      (error "Couldn't generate texture"))

    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d tex)
    ;;(gl:tex-env :texture-env :texture-env-mode :modulate)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-image-2d :texture-2d 0 4 (png:image-width png) (png:image-height png) 0 :rgba :unsigned-int-8-8-8-8 (png-to-simple-array png))
    
    tex))


(defun png-to-simple-array (png)
  (unless (>= (png:image-channels png) 3)
    (error "Must have at least 3 channels"))

  (unless (eql (png:image-bit-depth png) 8)
    (warn "Bit depth is not 16. Might look weird"))
  
  (let* ((w (png:image-width png))
         (h (png:image-height png))
         (arr (make-array (* w h)
                          :element-type '(unsigned-byte 32)
                          :initial-element 0)))

    (loop for i upto (1- h) do
         (loop for j upto (1- w) do
              (let ((r (aref png i j 0))
                    (g (aref png i j 1))
                    (b (aref png i j 2)))

                (unless (and (equal r g)
                             (equal g b)
                             (equal b 255))
                  
                  (setf (aref arr (+ (* i w) j))
                        (logior (ash r 24)
                                (ash g 16)
                                (ash b 8)
                                255))))))
    
    arr))

(defun load-simple-array-from-png-file (path)
  "Load a png file from disk and convert it to an array of rgb bytes"
  (png-to-simple-array (read-png-from-file path)))


;(read-png-from-file "./bitmaps/WhiteBishop_64.png")
;(load-simple-array-from-file "./bitmaps/WhiteBishop_64.png")

#|
glGenTextures( 1, &texture );
glBindTexture( GL_TEXTURE_2D, texture );
glTexEnvf( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,GL_MODULATE );
glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST );


glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,GL_LINEAR );
glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,GL_REPEAT );
glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,GL_REPEAT );
gluBuild2DMipmaps( GL_TEXTURE_2D, 3, width, height,GL_RGB, GL_UNSIGNED_BYTE, data );
free( data );
|#

(defun load-texture-from-png-file (file-path &optional (name file-path))
  "Load a texture from a file"
  (let* ((png (read-png-from-file file-path))
         (tex (make-texture-from-png png))
         (pair (cons name tex)))
    
    (push pair *textures*)
    pair))

(defun get-texture-from-png-file (file-path &optional (name file-path))
  "Load a texture from a file"
  (let ((pair (assoc name *textures* :test #'string-equal)))
    (if pair
        pair
        (load-texture-from-png-file file-path name))))


(load-texture-from-png-file "./bitmaps/WhiteBishop_64.png" "WhiteBishop")


(defun bind (name)
  "Bind a texture by name"
  (let ((tex (assoc name *textures* :test #'string-equal)))
    (unless tex
      (error "No such texture ~a" name))

    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d (cdr tex))))
