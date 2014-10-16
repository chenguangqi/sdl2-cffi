(defpackage #:sdl2-cffi
  (:use #:cl
        #:cffi)
  (:export #:+sdl-init-video+

           ;;SDL.h
           #:sdl-init
           #:sdl-quit

	   ;; SDL_event.h
	   #:sdl-event-type
	   #:sdl-event
	   #:sdl-poll-event
	   #:type

           ;; SDL_version.h
           #:major
           #:minor
           #:patch
           #:sdl-version

           ;; SDL_video.h
           #:+sdl-windowpos-centered+
           #:sdl-create-window
           #:sdl-destroy-window
           #:sdl-get-version
           #:sdl-gl-context-debug-flag
           #:sdl-gl-context-flag
           #:sdl-gl-context-flags
           #:sdl-gl-context-major-version
           #:sdl-gl-context-minor-version
           #:sdl-gl-create-context
           #:sdl-gl-delete-context
           #:sdl-gl-depth-size
           #:sdl-gl-double-buffer
	   #:sdl-gl-get-proc-address
           #:sdl-gl-set-attribute
           #:sdl-gl-set-swap-interval
           #:sdl-gl-swap-window
           #:sdl-window
           #:sdl-window-flags
           #:sdl-window-opengl
           #:sdl-window-shown))
