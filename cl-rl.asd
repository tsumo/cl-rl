(asdf:defsystem #:cl-rl
  :description "Common Lisp roguelike"
  :author "tsumo"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:sdl2-image)
  :components ((:file "package")
               (:file "tiles")
               (:file "main")))

