(in-package #:sdl2-cffi-examples)

(defun run-hello-opengl-window ()
    (sdl-init +sdl-init-video+)
    (print-sdl-info)
    (sdl-gl-set-attribute :sdl-gl-context-major-version 2)
    (sdl-gl-set-attribute :sdl-gl-context-minor-version 0)
    (sdl-gl-set-attribute :sdl-gl-double-buffer 1)
    (sdl-gl-set-attribute :sdl-gl-depth-size 24)
    (sdl-gl-set-attribute :sdl-gl-context-flags (foreign-enum-value 'sdl-gl-context-flag :sdl-gl-context-debug-flag))
    (let* ((main-window (create-window "hello-opengl-window"))
	   (main-context (create-gl-context main-window)))
      (sdl-gl-set-swap-interval 1)

      (print-gl-info t)

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
      
