(defpackage #:rl
  (:use :common-lisp)
  (:export :main))

(in-package :rl)

(require :sdl2)
(require :sdl2-image)

; === Globals ===

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
(defparameter *camera* nil)
(defparameter *redraw* t)
(defparameter *messages* nil)
(defparameter *examine-messages* nil)
(defparameter *mode* :normal)

; === Structures ===

(defclass texture ()
  ((renderer
     :initarg :renderer
     :initform (error "Must supply a renderer"))
   (width :accessor tex-width :initform 0)
   (height :accessor tex-height :initform 0)
   (texture :accessor tex-texture :initform nil)))

(defclass tile ()
  ((description
     :initarg :description
     :initform "Map tile")
   (passable :initarg :passable :initform t)
   (rect :initarg :rect :initform nil)))

(defclass entity ()
  ((x :initarg :x :initform 0)
   (y :initarg :y :initform 0)))

(defclass creature (entity)
  ((hp :initarg :hp :initform 0)
   (rect :initarg :rect :initform nil)
   (description
     :initarg :description
     :initform "No description")))

; === Initialization ===

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
    (let ((floor-rect (sdl2:make-rect 0 0 *tile-size* *tile-size*))
          (wall-rect (sdl2:make-rect 17 0 *tile-size* *tile-size*)))
      (loop for line = (read-line file nil)
            and y from 0
            while line
            do (loop for char across line
                     and x from 0
                     do (setf (gethash (list x y) *map*)
                              (case char
                                (#\# (make-instance 'tile
                                                    :description "Wall"
                                                    :passable nil
                                                    :rect wall-rect))
                                (#\. (make-instance 'tile
                                                    :description "Floor"
                                                    :passable t
                                                    :rect floor-rect))))
                     finally (setf *map-width* x))
            finally (setf *map-height* (1- y))))))

(defun init-vars ()
  (setf *redraw* t)
  (setf *messages* nil)
  (setf *camera* (make-instance 'entity :x 3 :y 3))
  (setf *mode* :normal))

(defun init-creatures ()
  (setf *creatures* (make-hash-table))
  (setf (gethash :player *creatures*)
        (make-instance 'creature
                       :x 3 :y 3 :hp 100
                       :rect (sdl2:make-rect 34 0 *tile-size* *tile-size*)
                       :description "Player"))
  (setf (gethash :monster *creatures*)
        (make-instance 'creature
                       :x 5 :y 5 :hp 10
                       :rect (sdl2:make-rect 0 17 *tile-size* *tile-size*)
                       :description "Monster")))

; === SDL2 ===

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

; === Game logic ===

(defun camera-on-player ()
  (with-slots (x y) (gethash :player *creatures*)
    (setf (slot-value *camera* 'x) x)
    (setf (slot-value *camera* 'y) y)))

(defun set-game-mode (mode)
  (setf *mode* mode)
  (case mode
    (:normal
      (camera-on-player))
    (:examine)))

(defun get-map-tile (x y)
  (gethash (list x y) *map*))

(defun move-creature-p (creature d-x d-y)
  (with-slots (x y) creature
    (if (slot-value (get-map-tile (+ x d-x) (+ y d-y)) 'passable)
        t
        (progn
          (push (format nil "Can't go ~a ~a" d-x d-y) *messages*)
          nil))))

(defun move-creature (creature d-x d-y)
  (when (move-creature-p creature d-x d-y)
    (with-slots (x y) creature
      (incf x d-x)
      (incf y d-y))))

(defun inside-interval (value from to)
  (and (>= value from )
       (<= value to)))

(defun move-camera (d-x d-y)
  (with-slots (x y) *camera*
    (when (and (inside-interval (+ x d-x) 0 *map-width*)
               (inside-interval (+ y d-y) 0 *map-height*))
      (incf x d-x)
      (incf y d-y))))

(defun handle-keypresses (key)
  (case *mode*
    (:normal
      (let ((player (gethash :player *creatures*)))
        (case key
          (:scancode-q (sdl2:push-quit-event))
          (:scancode-h (move-creature player -1 0))
          (:scancode-j (move-creature player 0 1))
          (:scancode-k (move-creature player 0 -1))
          (:scancode-l (move-creature player 1 0))
          (:scancode-x (set-game-mode :examine)))
        (camera-on-player)))
    (:examine
      (case key
        (:scancode-q (sdl2:push-quit-event))
        (:scancode-escape (set-game-mode :normal))
        (:scancode-h (move-camera -1 0))
        (:scancode-j (move-camera 0 1))
        (:scancode-k (move-camera 0 -1))
        (:scancode-l (move-camera 1 0))))))

; === Rendering ===

(defun render-map (spritesheet-texture)
  (with-slots (x y) *camera*
    (let ((start-x (- x (/ *main-viewport-width-in-tiles* 2)))
          (start-y (- y (/ *main-viewport-height-in-tiles* 2))))
      (loop for world-y from start-y to *map-height*
            and screen-y from 0 to *main-viewport-height-in-tiles*
            do (loop for world-x from start-x to *map-width*
                     and screen-x from 0 to *main-viewport-width-in-tiles*
                     do (when (and (>= world-x 0)
                                   (>= world-y 0))
                          (let ((tile (get-map-tile world-x world-y)))
                            (render spritesheet-texture
                                    (* screen-x *tile-size*)
                                    (* screen-y *tile-size*)
                                    :clip (slot-value tile 'rect)))))))))

(defun render-creatures (spritesheet-texture)
  (maphash (lambda (k creature)
             (declare (ignore k))
             (with-slots (x y rect) creature
               (render spritesheet-texture
                       (* (+ (/ *main-viewport-width-in-tiles* 2)
                             (- x (slot-value *camera* 'x)))
                          *tile-size*)
                       (* (+ (/ *main-viewport-height-in-tiles* 2)
                             (- y (slot-value *camera* 'y)))
                          *tile-size*)
                       :clip rect)))
           *creatures*))

(defun render-cursor (spritesheet-texture)
  (let ((cursor-rect (sdl2:make-rect 51 0 *tile-size* *tile-size*)))
    (render spritesheet-texture
            (/ *main-viewport-width* 2)
            (/ *main-viewport-height* 2)
            :clip cursor-rect)))

(defun render-text (font-texture text x y)
  (loop for char across text
        for pos from 0
        do (render font-texture
                   (+ x (* pos 8)) y
                   :clip (gethash char *font*))))

(defun render-info (font-texture)
  (render-text font-texture (princ-to-string *mode*) 5 5)
  (render-text font-texture
               (format nil "Map width ~a" *map-width*)
               5 (+ 5 9))
  (render-text font-texture
               (format nil "Map height ~a" *map-height*)
               5 (+ 5 (* 9 2)))
  (with-slots (x y hp) (gethash :player *creatures*)
    (render-text font-texture
                 (format nil "Player hp ~a" hp)
                 5 (+ 5 (* 9 3)))
    (render-text font-texture
                 (format nil "Player x ~a" x)
                 5 (+ 5 (* 9 4)))
    (render-text font-texture
                 (format nil "Player y ~a" y)
                 5 (+ 5 (* 9 5))))
  (with-slots (x y) *camera*
    (render-text font-texture
                 (format nil "Camera x ~a" x)
                 5 (+ 5 (* 9 6)))
    (render-text font-texture
                 (format nil "Camera y ~a" y)
                 5 (+ 5 (* 9 7)))))

(defun render-messages (font-texture &optional (messages *messages*))
  (loop for message in messages
        for i from 0 to 13
        do (render-text font-texture
                        message
                        5 (+ (* i 9) 2))))

(defun render-examine-messages (font-texture)
  (setf *examine-messages* nil)
  (with-slots (x y) *camera*
    (push (slot-value (get-map-tile x y) 'description)
          *examine-messages*)
    (maphash (lambda (k creature)
               (declare (ignore k))
               (when (and (= x (slot-value creature 'x))
                          (= y (slot-value creature 'y)))
                 (push (slot-value creature 'description) *examine-messages*)))
             *creatures*))
  (render-messages font-texture *examine-messages*))

; === Main game loop ===

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
                           "font.png")))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown (:keysym keysym)
         (setf *redraw* t) ; Redraw on keypress
         (handle-keypresses (sdl2:scancode keysym)))
        (:idle ()
         (when *redraw*
           (sdl2:render-clear renderer)
           (with-viewport (renderer *main-viewport*)
             (render-map spritesheet-texture)
             (render-creatures spritesheet-texture)
             (when (eq *mode* :examine)
               (render-cursor spritesheet-texture)))
           (with-viewport (renderer *menu-viewport*)
             (render-info font-texture))
           (with-viewport (renderer *message-viewport*)
             (case *mode*
               (:normal (render-messages font-texture))
               (:examine (render-examine-messages font-texture))))
           (sdl2:render-present renderer)
           (setf *redraw* nil))
         (sdl2:delay 50))))))
(main)
