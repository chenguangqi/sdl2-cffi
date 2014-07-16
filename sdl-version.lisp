(in-package #:sdl2-cffi)

(defcstruct sdl-version
  (major :uint8)
  (minor :uint8)
  (patch :uint8))

(defcfun ("SDL_GetVersion" sdl-get-version) :void
  (state :pointer))

(defun get-version ()
  (with-foreign-object (version '(:struct sdl-version))
    (sdl-get-version version)
    (with-foreign-slots ((major minor patch) version (:struct sdl-version))
      (format t "major=~a minor=~a patch=~a~%" major minor patch))))
