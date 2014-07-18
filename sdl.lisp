(in-package #:sdl2-cffi)

(defconstant +sdl-init-video+                      #x00000020)

(defcfun ("SDL_Init" sdl-init) :int
  (flags :uint32))

(defcfun ("SDL_Quit" sdl-quit) :void)

;; TODO:
;; Functions:
;; SDL_GetError
;; SDL_ClearError
;; SDL_GL_SwapWindow
;; SDL_PollEvent
;; SDL_GL_SetAttribute
;; SDL_CreateWindow
;; SDL_GL_CreateContext
;; SDL_GL_SetSwapInterval
;; SDL_GL_DeleteContext
;; SDL_DestroyWindow

;; Types:
;; SDL_Window
;; SDL_Event
;; SDL_GLContext

;; Constants:
;; SDL_GL_CONTEXT_MAJOR_VERSION
;; SDL_QUIT
;; SDL_GL_CONTEXT_MINOR_VERSION
;; SDL_GL_DOUBLEBUFFER
;; SDL_GL_DOUBLEBUFFER
;; SDL_GL_DEPTH_SIZE
;; SDL_GL_CONTEXT_FLAGS
;; SDL_GL_CONTEXT_DEBUG_FLAG
;; SDL_GL_CONTEXT_PROFILE_MASK
;; SDL_GL_CONTEXT_PROFILE_ES
;; SDL_WINDOWPOS_CENTERED
;; SDL_WINDOW_OPENGL
;; SDL_WINDOW_SHOWN
;; SDL_INIT_VIDEO
