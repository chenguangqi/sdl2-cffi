(asdf:defsystem #:sdl2-cffi
  :serial t
  :description "Incomplete SDL2 CFFI library"
  :author "Mick Beaver <m.charles.beaver@gmail.com>"
  :license "MIT License"
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "sdl")
               (:file "sdl-version")
	       (:file "library")))

