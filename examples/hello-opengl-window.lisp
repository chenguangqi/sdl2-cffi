;;;; gltut01.lisp

(in-package #:gltut01)

(defvar *vertex-shader* "
#version 110
attribute vec4 position;
void main()
{
    gl_Position = position;
}
")

(defvar *fragment-shader* "
#version 110
void main()
{
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
}
")

(defvar *the-program*)
(defvar *position-buffer-object*)
(defvar *vertex-array-object*)

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

(defun init-program ()
  (let ((shader-lst (list (create-shader :vertex-shader *vertex-shader*)
			  (create-shader :fragment-shader *fragment-shader*))))
    (setf *the-program* (create-program shader-lst))
    (dolist (shader-id shader-lst)
      (gl:delete-shader shader-id))))

(defun init-vertex-buffer ()
  (setf *position-buffer-object* (elt (gl:gen-buffers 1) 0))
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (let ((arr (gl:alloc-gl-array :float 12))
	(verts #(0.75 0.75 0.0 1.0
		 0.75 -0.75 0.0 1.0
		 -0.75 -0.75 0.0 1.0)))
    (dotimes (i (length verts))
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (gl:bind-buffer :array-buffer 0))

(defun init (win gl-context)
  ;(sdl2:gl-make-current win gl-context)
  (init-program)
  (init-vertex-buffer)
  (setf *vertex-array-object* (gl:gen-vertex-array))
  (gl:bind-vertex-array *vertex-array-object*))


(defun display (win)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit)
  (gl:use-program *the-program*)
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 4 :float nil 0 (cffi:null-pointer))
  (gl:draw-arrays :triangles 0 3)
  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0))
  ;(sdl2:gl-swap-window win))

;(defun local-get-proc-address (proc-name)
;  (format t "proc-name = ~a~%" proc-name)
;  (sdl2-ffi.functions:sdl-gl-get-proc-address proc-name))

;; (defun hello-triangle ()
;;   (sdl2:with-init (:everything)
;;     (format t "Using SDL Library Version: ~D.~D.~D~%"
;; 	    sdl2-ffi:+sdl-major-version+
;; 	    sdl2-ffi:+sdl-minor-version+
;; 	    sdl2-ffi:+sdl-patchlevel+)
;;     (finish-output)

;;     (sdl2:with-window (win :flags '(:shown :opengl))
;;       ;(setf %gl:*gl-get-proc-address* #'local-get-proc-address)
;;       (sdl2:with-gl-context (gl-context win)
;; 	(init win gl-context)
;; 	(sdl2:with-event-loop (:method :poll)
;; 	  (:idle () (display win))
;; 	  (:keyup () (sdl2:push-event :quit))
;; 	  (:quit () t))))))

;;     SDL_Window *mainwindow;
;;     SDL_GLContext maincontext;

;;     (void) argc;
;;     (void) argv;
 
;;     if (SDL_Init(SDL_INIT_VIDEO) < 0) {
;;         sdl_print_error_and_exit("Unable to initialize SDL");
;;     }
  
;;     SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
;;     SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);
;;     SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
;;     SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
;; #if defined(RENDERER_OPENGLES2)
;;     SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
;; #endif
 
;;     mainwindow = SDL_CreateWindow("Yeah buddy",
;;                                   SDL_WINDOWPOS_CENTERED,
;;                                   SDL_WINDOWPOS_CENTERED,
;;                                   512,
;;                                   512,
;;                                   SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN);
;;     if (mainwindow == NULL) {
;;         sdl_print_error_and_exit("SDL_CreateWindow() failed!");
;;     }
;;     sdl_check_error();
 
;;     maincontext = SDL_GL_CreateContext(mainwindow);
;;     sdl_check_error();
 
;;     SDL_GL_SetSwapInterval(1); // 60hz?
 
;;     glClearColor ( 1.0, 0.0, 0.0, 1.0 );
;;     glClear ( GL_COLOR_BUFFER_BIT );
;;     SDL_GL_SwapWindow(mainwindow);
;;     SDL_Delay(2000);
 
;;     glClearColor ( 0.0, 1.0, 0.0, 1.0 );
;;     glClear ( GL_COLOR_BUFFER_BIT );
;;     SDL_GL_SwapWindow(mainwindow);
;;     SDL_Delay(2000);
 
;;     glClearColor ( 0.0, 0.0, 1.0, 1.0 );
;;     glClear ( GL_COLOR_BUFFER_BIT );
;;     SDL_GL_SwapWindow(mainwindow);
;;     SDL_Delay(2000);
 
;;     SDL_GL_DeleteContext(maincontext);
;;     SDL_DestroyWindow(mainwindow);
;;     SDL_Quit();
 
;;     return 0;

(defun create-window ()
  (with-foreign-string (title "Hello Lisp!")
    (sdl-create-window title
		       +sdl-windowpos-centered+
		       +sdl-windowpos-centered+
		       512
		       512
		       (logior (foreign-enum-value 'sdl-window-flags :sdl-window-opengl)
			       (foreign-enum-value 'sdl-window-flags :sdl-window-shown)))))
		       

(defun hello-opengl-window ()
    (sdl-init +sdl-init-video+)
    (with-foreign-object (version '(:struct sdl-version))
      (sdl-get-version version)
      (with-foreign-slots ((major minor patch) version (:struct sdl-version))
	(format t "Using SDL version ~a.~a.~a~%" major minor patch)))
    (sdl-gl-set-attribute :sdl-gl-context-major-version 2)
    (sdl-gl-set-attribute :sdl-gl-context-minor-version 0)
    (sdl-gl-set-attribute :sdl-gl-double-buffer 1)
    (sdl-gl-set-attribute :sdl-gl-depth-size 24)
    (sdl-gl-set-attribute :sdl-gl-context-flags (foreign-enum-value 'sdl-gl-context-flag :sdl-gl-context-debug-flag))
    (let* ((main-window (create-window))
	   (main-context (sdl-gl-create-context main-window)))
      (sdl-gl-set-swap-interval 1)

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
      
