(asdf:defsystem #:sdl2-cffi-examples
  :serial t
  :description "Examples of using SDL2 CFFI library"
  :author "Mick Beaver <m.charles.beaver@gmail.com>"
  :license "MIT License"
  :depends-on (#:cffi #:sdl2-cffi #:cl-opengl)
  :components
  ((:module "examples"
	    :components ((:file "package")
			 (:file "common")
			 (:file "opengl-buffer-objects")
			 (:file "hello-opengl-window")))))

