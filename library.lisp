(in-package #:sdl2-cffi)

(defparameter *sdl2-cffi-loaded-p* nil)

(cffi:define-foreign-library sdl2-cffi
  (:windows "SDL2.dll"))

(defun load-sdl2-cffi-library ()
  (cffi:use-foreign-library sdl2-cffi)
  (setf *sdl2-cffi-loaded-p* t))

(eval-when (:load-toplevel :execute)
  (load-sdl2-cffi-library))

