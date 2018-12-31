(defpackage #:rl
  (:use :common-lisp)
  (:export :main))

(in-package :rl)

(require :sdl2)
(require :sdl2-image)

; Globals

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *tile-size* 16)
(defparameter *main-viewport-width-in-tiles* 28)
(defparameter *main-viewport-height-in-tiles* 22)
(defparameter *main-viewport-width*
  (* *tile-size* *main-viewport-width-in-tiles*))
(defparameter *main-viewport-height*
  (* *tile-size* *main-viewport-height-in-tiles*))
(defparameter *main-viewport*
  (list 0
        0
        *main-viewport-width*
        *main-viewport-height*))
(defparameter *menu-viewport*
  (list *main-viewport-width*
        0
        (- *screen-width* *main-viewport-width*)
        *screen-height*))
(defparameter *message-viewport*
  (list 0
        *main-viewport-height*
        *screen-width*
        (- *screen-height* *main-viewport-height*)))
(defparameter *map* nil)
(defparameter *map-width* 0)
(defparameter *map-height* 0)
(defparameter *font* (make-hash-table))
(defparameter *creatures* nil)
(defparameter *redraw* t)
(defparameter *messages* nil)

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
  ((x :initarg :x :initform 0)
   (y :initarg :y :initform 0)
   (hp :initarg :hp :initform 0)))

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
  (setf *map* (make-hash-table :test #'equal))
  (with-open-file (file "map.txt")
    (loop for line = (read-line file nil)
          and y from 0
          while line
          do (loop for char across line
                   and x from 0
                   do (setf (gethash (list x y) *map*)
                            char)
                   finally (setf *map-width* x))
          finally (setf *map-height* y))))

(defun init-vars ()
  (setf *redraw* t)
  (setf *messages* nil))

(defun init-creatures ()
  (setf *creatures*
        (list ':player (make-instance 'creature :x 3 :y 3 :hp 100)
              ':monster (make-instance 'creature :x 5 :y 5 :hp 10))))

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

(defmacro with-viewport ((renderer viewport) &body body)
  `(destructuring-bind (x y w h) ,viewport
     (sdl2:render-set-viewport
       ,renderer
       (sdl2:make-rect x y w h))
     ,@body
     (sdl2:render-set-viewport
       ,renderer
       (sdl2:make-rect 0 0 *screen-width* *screen-height*))))

; Game logic

(defun push-message (message)
  (setf *messages*
        (cons message *messages*)))

(defun get-map-tile (x y)
  (gethash (list x y) *map*))

(defun move-creature-p (creature d-x d-y)
  (with-slots (x y) creature
    (if (eql #\# (get-map-tile (+ x d-x) (+ y d-y)))
        (progn
          (push-message (format nil "Can't go ~a ~a" d-x d-y))
          nil)
        t)))

(defun move-creature (creature d-x d-y)
  (when (move-creature-p creature d-x d-y)
    (with-slots (x y hp) creature
      (incf x d-x)
      (incf y d-y)
      (decf hp))))

; Rendering

(defun render-map (spritesheet-texture)
  (let ((player (getf *creatures* :player)))
    (with-slots (x y) player
      (let ((start-x (- x (/ *main-viewport-width-in-tiles* 2)))
            (start-y (- y (/ *main-viewport-height-in-tiles* 2)))
            (floor-rect (sdl2:make-rect 0 0 *tile-size* *tile-size*))
            (wall-rect (sdl2:make-rect 17 0 *tile-size* *tile-size*)))
        (loop for world-y from start-y to (1- *map-height*)
              and screen-y from 0 to *main-viewport-height-in-tiles*
              do (loop for world-x from start-x to *map-width*
                       and screen-x from 0 to *main-viewport-width-in-tiles*
                       do (when (and (>= world-x 0)
                                     (>= world-y 0))
                            (let ((tile (get-map-tile world-x world-y)))
                              (render spritesheet-texture
                                      (* screen-x *tile-size*) (* screen-y *tile-size*)
                                      :clip (cond ((eql tile #\#)
                                                   wall-rect)
                                                  ((eql tile #\.)
                                                   floor-rect)))))))))))

(defun render-creatures (spritesheet-texture)
  (let ((player-rect (sdl2:make-rect 34 0 *tile-size* *tile-size*))
        (monster-rect (sdl2:make-rect 0 17 *tile-size* *tile-size*))
        (player (getf *creatures* :player))
        (monster (getf *creatures* :monster)))
    (render spritesheet-texture
            (* (/ *main-viewport-width-in-tiles* 2) *tile-size*)
            (* (/ *main-viewport-height-in-tiles* 2) *tile-size*)
            :clip player-rect)
    (with-slots (x y) monster
      (render spritesheet-texture
              (* (+ (/ *main-viewport-width-in-tiles* 2)
                    (- x (slot-value player 'x)))
                 *tile-size*)
              (* (+ (/ *main-viewport-height-in-tiles* 2)
                    (- y (slot-value player 'y)))
                 *tile-size*)
              :clip monster-rect))))

(defun render-text (font-texture text x y)
  (loop for char across text
        for pos from 0
        do (render font-texture
                   (+ x (* pos 8)) y
                   :clip (gethash char *font*))))

(defun render-player-info (font-texture player)
  (with-slots (x y hp) player
    (render-text font-texture
                 (concatenate 'string "Player x "
                              (princ-to-string x))
                 5 5)
    (render-text font-texture
                 (concatenate 'string "Player y "
                              (princ-to-string y))
                 5 14)
    (render-text font-texture
                 (concatenate 'string "Player hp "
                              (princ-to-string hp))
                 5 23)))

(defun render-last-messages (font-texture)
  (loop for message in *messages*
        for i from 0 to 13
        do (render-text font-texture
                        message
                        5 (+ (* i 9) 2))))

; Main game loop

(defun main ()
  (init-font)
  (init-map)
  (init-vars)
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
         (setf *redraw* t) ; Redraw on keypress
         (case (sdl2:scancode keysym)
           (:scancode-h (move-creature player -1 0))
           (:scancode-j (move-creature player 0 1))
           (:scancode-k (move-creature player 0 -1))
           (:scancode-l (move-creature player 1 0))
           (:scancode-q (sdl2:push-quit-event))))
        (:idle ()
         (when *redraw*
           (sdl2:render-clear renderer)
           (with-viewport (renderer *main-viewport*)
             (render-map spritesheet-texture)
             (render-creatures spritesheet-texture))
           (with-viewport (renderer *menu-viewport*)
             (render-player-info font-texture player))
           (with-viewport (renderer *message-viewport*)
             (render-last-messages font-texture))
           (sdl2:render-present renderer)
           (setf *redraw* nil))
         (sdl2:delay 50))))))
(main)
