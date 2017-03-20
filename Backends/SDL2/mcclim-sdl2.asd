
(defsystem #:mcclim-sdl2
  :depends-on (#:sdl2 #:sdl2-ttf #:uiop #:clim #:mcclim-full-mirrored-standard)
  :components
  ((:file "package")
   (:file "port")))
