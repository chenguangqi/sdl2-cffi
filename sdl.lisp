(in-package #:sdl2-cffi)

(defconstant +sdl-init-video+                      #x00000020)

(defcfun ("SDL_Init" sdl-init) :int
  (flags :uint32))

(defcfun ("SDL_Quit" sdl-quit) :void)

