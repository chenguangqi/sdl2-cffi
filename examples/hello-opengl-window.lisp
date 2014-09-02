(in-package #:sdl2-cffi)

(defun create-window ()
  (with-foreign-string (title "Hello Lisp!")
    (sdl-create-window title
		       +sdl-windowpos-centered+
		       +sdl-windowpos-centered+
		       512
		       512
		       (logior (foreign-enum-value 'sdl-window-flags :sdl-window-opengl)
			       (foreign-enum-value 'sdl-window-flags :sdl-window-shown)))))
		       

(defun print-sdl-info ()
  (with-foreign-object (version '(:struct sdl-version))
    (sdl-get-version version)
    (with-foreign-slots ((major minor patch) version (:struct sdl-version))
      (format t "Using SDL version ~a.~a.~a~%" major minor patch))))

(defun print-gl-info ()
  (format t "GL version: ~a~%" (gl:get* :version))
  (format t "GLSL version: ~a~%" (gl:get* :shading-language-version))
  (format t "GL vendor: ~a~%" (gl:get* :vendor))
  (format t "GL renderer: ~a~%" (gl:get* :renderer))
  (format t "GL extensions: ~a~%" (gl:get* :extensions)))


(defun hello-opengl-window ()
    (sdl-init +sdl-init-video+)
    (print-sdl-info)
    (sdl-gl-set-attribute :sdl-gl-context-major-version 2)
    (sdl-gl-set-attribute :sdl-gl-context-minor-version 0)
    (sdl-gl-set-attribute :sdl-gl-double-buffer 1)
    (sdl-gl-set-attribute :sdl-gl-depth-size 24)
    (sdl-gl-set-attribute :sdl-gl-context-flags (foreign-enum-value 'sdl-gl-context-flag :sdl-gl-context-debug-flag))
    (let* ((main-window (create-window))
	   (main-context (sdl-gl-create-context main-window)))
      (sdl-gl-set-swap-interval 1)

      (print-gl-info)

      (gl:clear-color 1.0 0.0 0.0 1.0)
      (gl:clear :color-buffer-bit)
      (sdl-gl-swap-window main-window)
      (sleep 2)

      (gl:clear-color 0.0 1.0 0.0 1.0)
      (gl:clear :color-buffer-bit)
      (sdl-gl-swap-window main-window)
      (sleep 2)

      (gl:clear-color 0.0 0.0 1.0 1.0)
      (gl:clear :color-buffer-bit)
      (sdl-gl-swap-window main-window)
      (sleep 2)

      (sdl-gl-delete-context main-context)
      (sdl-destroy-window main-window))
    (sdl-quit))
      
