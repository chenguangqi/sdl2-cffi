(in-package #:sdl2-cffi-examples)

(defclass shader ()
  ((vertex-shader
    :initarg :vertex-shader
    :initform 0
    :accessor vertex-shader
    :documentation "Vertex shader handle")
   (fragment-shader
    :initarg :fragment-shader
    :initform 0
    :accessor fragment-shader
    :documentation "Fragment shader handle")
   (program
    :initarg :program
    :initform 0
    :accessor program
    :documentation "Program handle")))
		  
(defun create-shader (shader-type shader-string)
  (let ((shader-id (gl:create-shader shader-type)))
    (gl:shader-source shader-id shader-string)
    (gl:compile-shader shader-id)
    (when (not (gl:get-shader shader-id :compile-status))
      (format t "~a~%" (gl:get-shader-info-log shader-id)))
    shader-id))

(defun create-program (shader-lst)
  (let ((program-id (gl:create-program)))
    (dolist (shader-id shader-lst)
      (gl:attach-shader program-id shader-id))
    (gl:bind-attrib-location program-id 0 "position")
    (gl:link-program program-id)
    (when (not (gl:get-program program-id :link-status))
      (format t "~a~%" (gl:get-program-info-log program-id)))
    program-id))

(defun init-program (vertex-shader-src fragment-shader-src)
  (let* ((shader-lst (list (create-shader :vertex-shader vertex-shader-src)
			   (create-shader :fragment-shader fragment-shader-src)))
	 (program-id (create-program shader-lst)))
    (dolist (shader-id shader-lst)
      (gl:delete-shader shader-id))
    program-id))

(defun create-window (window-title &key (width 512) (height 512))
  (with-foreign-string (title window-title)
    (sdl-create-window title
		       +sdl-windowpos-centered+
		       +sdl-windowpos-centered+
		       width
		       height
		       (logior (foreign-enum-value 'sdl-window-flags :sdl-window-opengl)
			       (foreign-enum-value 'sdl-window-flags :sdl-window-shown)))))
		       
(defun print-sdl-info ()
  (with-foreign-object (version '(:struct sdl-version))
    (sdl-get-version version)
    (with-foreign-slots ((major minor patch) version (:struct sdl-version))
      (format t "Using SDL version ~a.~a.~a~%" major minor patch)
      (finish-output))))

(defun print-gl-info (&optional print-extensions-p)
  (format t "GL version: ~a~%" (gl:get* :version))
  (format t "GLSL version: ~a~%" (gl:get* :shading-language-version))
  (format t "GL vendor: ~a~%" (gl:get* :vendor))
  (format t "GL renderer: ~a~%" (gl:get* :renderer))
  (when print-extensions-p
    (format t "GL extensions:~%")
    (dolist (extension (sort (cl-utilities:split-sequence #\Space (gl:get* :extensions) #'string-lessp)))
      (format t "    ~a~%" extension)))
  (finish-output))

(defun gl-get-proc-address (procname)
   (with-foreign-string (c-procname procname)
     (sdl-gl-get-proc-address c-procname)))

(defun create-gl-context (main-window)
  (setf %gl:*gl-get-proc-address* #'gl-get-proc-address)
  (sdl-gl-create-context main-window))


