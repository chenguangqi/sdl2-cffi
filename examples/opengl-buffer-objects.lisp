(in-package #:sdl2-cffi-examples)

(defvar *vertex-shader*
  "
attribute vec4 a_position;
attribute vec4 a_color;
varying vec4 v_color;
void main()
{
    gl_Position = a_position;
    v_color = a_color;
}
")

(defvar *fragment-shader*
  "
varying vec4 v_color;
void main()
{
    gl_FragColor = v_color;
}
")

(defclass triangle-mesh ()
  ((raw-verts
    :reader raw-verts
    :initform  #(0.0 0.5 0.0   ; vert 0 pos
		 1.0 0.0 0.0   ; vert 0 color
		 -0.5 -0.5 0.0 ; vert 1 pos
		 0.0 1.0 0.0   ; vert 1 color
		 0.5 -0.5 0.0  ; vert 2 pos
		 0.0 0.0 1.0)) ; vert 2 color
   (num-verts
    :reader num-verts
    :initform 3)
   (num-components-per-vert
    :reader num-components-per-vert
    :initform 3)
   (vert-stride
    :reader vert-stride
    :initform (* (cffi:foreign-type-size :float) 6))
   (buffer-object
    :accessor buffer-object
    :initform (car (gl:gen-buffers 1)))
   (vertex-array
    :accessor vertex-array)))

(defclass opengl-buffer-objects-example ()
  ((window
    :initarg :window
    :accessor window)
   (mesh
    :reader mesh
    :initform (make-instance 'triangle-mesh))
   (shader
    :accessor shader
    :initform (make-instance 'shader))))

(defmethod initialize-instance :after ((mesh triangle-mesh) &key)
  (setf (vertex-array mesh)
	(gl::make-gl-array-from-pointer (cffi:foreign-alloc :float
							    :initial-contents (raw-verts mesh))
					:float
					(length (raw-verts mesh))))
  (gl:bind-buffer :array-buffer (buffer-object mesh))
  (gl:buffer-data :array-buffer :static-draw (vertex-array mesh)))

(defmethod init ((example opengl-buffer-objects-example))
  (let* ((program-id (init-program *vertex-shader* *fragment-shader*))
	 (pos-idx (gl:get-attrib-location program-id "a_position"))
	 (color-idx (gl:get-attrib-location program-id "a_color"))
	 (tri-mesh (mesh example)))
    (gl:use-program program-id)
    (gl:bind-buffer :array-buffer (buffer-object tri-mesh))
    (gl:vertex-attrib-pointer pos-idx
			      (num-components-per-vert tri-mesh)
			      :float
			      nil
			      (vert-stride tri-mesh)
			      (make-pointer 0))
    (gl:enable-vertex-attrib-array pos-idx)
    (gl:vertex-attrib-pointer color-idx
			      (num-components-per-vert tri-mesh)
			      :float
			      nil
			      (vert-stride tri-mesh)
			      (make-pointer (* (num-components-per-vert tri-mesh) (cffi:foreign-type-size :float))))
    (gl:enable-vertex-attrib-array color-idx)
    (gl:clear-color 0.0 0.0 0.0 1.0)))

(defmethod draw ((example opengl-buffer-objects-example))
  (gl:clear :color-buffer-bit)
  (gl:draw-arrays :triangles 0 3))

(defmethod simulation-loop ((example opengl-buffer-objects-example))
  (loop
     (draw example)
     (sdl-gl-swap-window (window example))
     (with-foreign-object (event '(:union sdl-event))
       (sdl-poll-event event)
       (with-foreign-slots ((type) event (:union sdl-event))
       (cond
	 ((= type (foreign-enum-value 'sdl-event-type :sdl-quit))
	  (format t "Got SDL_QUIT!~%")
	  (return))
	 ((= type (foreign-enum-value 'sdl-event-type :sdl-keydown))
	  (format t "Got keypress. Quitting...~%")
	  (return))
	 (nil (format t "Event type = ~a~%" type)))))))

(defun run-buffer-objects-example ()
    (sdl-init +sdl-init-video+)
    (print-sdl-info)
    (sdl-gl-set-attribute :sdl-gl-context-major-version 2)
    (sdl-gl-set-attribute :sdl-gl-context-minor-version 0)
    (sdl-gl-set-attribute :sdl-gl-double-buffer 1)
    (sdl-gl-set-attribute :sdl-gl-depth-size 24)
    (sdl-gl-set-attribute :sdl-gl-context-flags (foreign-enum-value 'sdl-gl-context-flag :sdl-gl-context-debug-flag))
    (let* ((main-window (create-window "run-buffer-objects-example"))
	   (main-context (create-gl-context main-window))
	   (example (make-instance 'opengl-buffer-objects-example :window main-window)))
      (sdl-gl-set-swap-interval 1)
      (print-gl-info)
      (init example)
      (simulation-loop example)

      (sdl-gl-delete-context main-context)
      (sdl-destroy-window main-window))
    (sdl-quit))
      
