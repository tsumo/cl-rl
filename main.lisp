(defpackage #:rl
  (:use :common-lisp)
  (:export :main))

(in-package :rl)

(require :sdl2)
(require :sdl2-image)

; Globals

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *font* (make-hash-table))
(defparameter *map* nil)
(defparameter *creatures* nil)

; Structures

(defclass texture ()
  ((renderer
     :initarg :renderer
     :initform (error "Must supply a renderer"))
   (width
     :accessor tex-width
     :initform 0)
   (height
     :accessor tex-height
     :initform 0)
   (texture
     :accessor tex-texture
     :initform nil)))

(defclass creature ()
  ((x
     :initarg :x
     :initform 0)
   (y
     :initarg :y
     :initform 0)))

; Initialization

(defun init-font ()
  (loop for ascii-code from 32 to 126
        for row from 0
        for col from 0
        do (setf (gethash (code-char ascii-code) *font*)
                 (sdl2:make-rect
                   (* (mod col 16) 7)
                   (* (floor row 16) 8)
                   7 8))))

(defun init-map ()
  (setf *map*
        '((* * * * * * * * * * * * *)
          (* - - - - * - - - - - - *)
          (* - - - - * - - - - - - *)
          (* * - * * * - - - - - - *)
          (* - - - - - - - - - - - *)
          (* - - - - - - - - - - - *)
          (* - - - - - * * - * * - *)
          (* - - - - - * - - - * - *)
          (* - - - - - * - - - * - *)
          (* - - - - - * * * * * - *)
          (* - - - - - - - - - - - *)
          (* * * * * * * * * * * * *))))

(defun init-creatures ()
  (setf *creatures*
        (list ':player (make-instance 'creature :x 3 :y 4)
              ':monster (make-instance 'creature :x 5 :y 5))))

; SDL2

(defmacro with-window-renderer ((window renderer) &body body)
  "Uses argument destructuring."
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                         :title "rl"
                         :w *screen-width*
                         :h *screen-height*
                         :flags '(:shown))
       (sdl2:with-renderer (,renderer
                             ,window
                             :index -1
                             :flags '(:accelerated))
         ,@body))))

(defun render (texture x y &key clip)
  (with-slots (renderer texture width height) texture
    (sdl2:render-copy
      renderer
      texture
      :source-rect clip
      :dest-rect (sdl2:make-rect
                   x
                   y
                   (if clip (sdl2:rect-width clip) width)
                   (if clip (sdl2:rect-height clip) height)))))

(defun load-texture-from-file (renderer filename)
  (let ((texture (make-instance 'texture :renderer renderer)))
    (with-slots (renderer texture width height) texture
      (let ((surface (sdl2-image:load-image filename)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key
          surface
          :true
          (sdl2:map-rgb (sdl2:surface-format surface)
                        0 130 130))
        (setf texture (sdl2:create-texture-from-surface
                        renderer
                        surface))))
    texture))

(defmacro with-viewport ((renderer x y w h) &body body)
  `(progn
     (sdl2:render-set-viewport
       ,renderer
       (sdl2:make-rect ,x ,y ,w ,h))
     ,@body
     (sdl2:render-set-viewport
       ,renderer
       (sdl2:make-rect 0 0 *screen-width* *screen-height*))))

; Game logic

(defun get-map-tile (x y)
  (nth x (nth y *map*)))

(defun move-creature-p (creature d-x d-y)
  (with-slots (x y) creature
    (if (eq '* (get-map-tile (+ x d-x) (+ y d-y)))
        nil
        t)))

(defun move-creature (creature d-x d-y)
  (if (move-creature-p creature d-x d-y)
      (with-slots (x y) creature
        (incf x d-x)
        (incf y d-y))))

; Rendering

(defun render-map (spritesheet-texture)
  (let ((floor-rect (sdl2:make-rect 0 0 16 16))
        (wall-rect (sdl2:make-rect 17 0 16 16)))
    (loop for row in *map* and i from 0
        do (loop for tile in row and j from 0
                 do (render spritesheet-texture
                            (* j 16) (* i 16)
                            :clip (cond ((eq tile '*)
                                         wall-rect)
                                        ((eq tile '-)
                                         floor-rect)))))))

(defun render-creatures (spritesheet-texture)
  (let ((player-rect (sdl2:make-rect 34 0 16 16))
        (monster-rect (sdl2:make-rect 0 17 16 16))
        (player (getf *creatures* :player))
        (monster (getf *creatures* :monster)))
    (render spritesheet-texture
            (* (slot-value player 'x) 16) (* (slot-value player 'y) 16)
            :clip player-rect)
    (render spritesheet-texture
            (* (slot-value monster 'x) 16) (* (slot-value monster 'y) 16)
            :clip monster-rect) ))

(defun render-text (font-texture text)
  (loop for char across text
        for pos from 0
        do (render font-texture
                   (* pos 8) 0
                   :clip (gethash char *font*))))

; Main game loop

(defun main ()
  (init-font)
  (init-map)
  (init-creatures)
  (with-window-renderer
    (window renderer)
    (sdl2-image:init '(:png))
    (sdl2:set-render-draw-color
      renderer 0 0 0 0)
    (let* ((spritesheet-texture (load-texture-from-file
                              renderer
                              "spritesheet.png"))
           (font-texture (load-texture-from-file
                           renderer
                           "font.png"))
           (player (getf *creatures* :player)))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown (:keysym keysym)
         (case (sdl2:scancode keysym)
           (:scancode-h (move-creature player -1 0))
           (:scancode-j (move-creature player 0 1))
           (:scancode-k (move-creature player 0 -1))
           (:scancode-l (move-creature player 1 0))
           (:scancode-q (sdl2:push-quit-event))))
        (:idle ()
         (sdl2:render-clear renderer)
         (with-viewport (renderer 0 0 450 350) ; Game area
           (render-map spritesheet-texture)
           (render-creatures spritesheet-texture))
         (with-viewport (renderer 450 0 190 480) ; Minimap, menus
           (render-map spritesheet-texture)
           (render-creatures spritesheet-texture))
         (with-viewport (renderer 0 350 640 130) ; Messages
           (render-text font-texture
                        "The quick brown fox jumped over the lazy dog"))
         (sdl2:render-present renderer)
         (sdl2:delay 100))))))
(main)
