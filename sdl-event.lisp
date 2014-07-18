(in-package #:sdl2-cffi)

(defcenum sdl-event-type
    (:sdl-quit #x100))

(defcunion sdl-event
  (type :uint32)
  ;; Fill in the SDL event structs as needed
  (padding :uint8 :count 56))

(defcfun ("SDL_PollEvent" sdl-poll-event) :int
  (event :pointer))

