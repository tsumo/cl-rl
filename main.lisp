(defpackage #:rl
  (:use :common-lisp)
  (:export :main))

(in-package :rl)

(require :sdl2)
(require :sdl2-image)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *map* nil)
(defparameter *creatures* nil)

(defclass creature ()
  ((x
     :initarg :x
     :initform 0)
   (y
     :initarg :y
     :initform 0)))

(defun init-game-objects ()
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
          (* * * * * * * * * * * * *)))
  (setf *creatures*
        (list ':player (make-instance 'creature :x 3 :y 4)
              ':monster (make-instance 'creature :x 5 :y 5))))

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

(defun render (tex x y &key clip)
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy
      renderer
      texture
      :source-rect clip
      :dest-rect (sdl2:make-rect
                   x
                   y
                   (if clip (sdl2:rect-width clip) width)
                   (if clip (sdl2:rect-height clip) height)))))

(defclass tex ()
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

(defun load-texture-from-file (renderer filename)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture width height) tex
      (let ((surface (sdl2-image:load-image filename)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key
          surface
          :true
          (sdl2:map-rgb (sdl2:surface-format surface)
                        0 0 0))
        (setf texture (sdl2:create-texture-from-surface
                        renderer
                        surface))))
    tex))

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

(defun render-map (spritesheet-tex)
  (let ((floor-rect (sdl2:make-rect 0 0 16 16))
        (wall-rect (sdl2:make-rect 17 0 16 16)))
    (loop for row in *map* and i from 0
        do (loop for tile in row and j from 0
                 do (render spritesheet-tex
                            (* j 16) (* i 16)
                            :clip (cond ((eq tile '*)
                                         wall-rect)
                                        ((eq tile '-)
                                         floor-rect)))))))

(defun render-creatures (spritesheet-tex)
  (let ((player-rect (sdl2:make-rect 34 0 16 16))
        (monster-rect (sdl2:make-rect 0 17 16 16))
        (player (getf *creatures* :player))
        (monster (getf *creatures* :monster)))
    (render spritesheet-tex
            (* (slot-value player 'x) 16) (* (slot-value player 'y) 16)
            :clip player-rect)
    (render spritesheet-tex
            (* (slot-value monster 'x) 16) (* (slot-value monster 'y) 16)
            :clip monster-rect) ))

(defun main ()
  (init-game-objects)
  (with-window-renderer
    (window renderer)
    (sdl2-image:init '(:png))
    (sdl2:set-render-draw-color
      renderer 0 0 0 0)
    (let* ((spritesheet-tex (load-texture-from-file
                              renderer
                              "spritesheet.png"))
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
         (render-map spritesheet-tex)
         (render-creatures spritesheet-tex)
         (sdl2:render-present renderer)
         (sdl2:delay 100))))))
(main)
