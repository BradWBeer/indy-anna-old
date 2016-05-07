 ;;;; indy-anna.lisp

(in-package #:indy-anna)

(defparameter +id-counter+ 0)
(defconstant +max-axis+ 32767)
(defparameter +axis-dead-zone+ .40)
(defparameter +joystick-1+ (v! 0 0))
(defparameter +joystick-2+ (v! 0 0))
(defparameter *hash* (make-hash-table))
(defparameter +vwidth+ (* 800 3))
(defparameter +vheight+ (* 600 3))

(defevent *on-window-resized* (window width height timestamp)

  (let ((m (max (float (/ +vwidth+ width))
		(float (/ +vheight+ height)))))
    (format t "w=~A h=~A min=~A~%" width height m)
    (setf *projection*
	  (make-orthogonal-transform (* width m) (* height m)
				     0 1000))))

(defun make-wall (e x y w h &optional parent)
  (let* ((tex  (make-instance 'texture :width w :height h))
	 (quad (make-quad-for-texture tex))
	 (node (make-instance 'node :parent parent :children (list quad))))
    (fast-draw (:texture tex)
      (let* ((lw 10)
	     (l/2 (/ lw 2)))
	(cairo:set-source-rgb 0 1 0)
	(clear-cairo-context 0 0 0 0)
	(cairo:set-line-width lw)
	(cairo:set-line-join :round)
	(cairo:move-to l/2 l/2)
	(cairo:line-to (- w l/2) l/2)
	(cairo:line-to (- w l/2) (- h l/2))
	(cairo:line-to l/2 (- h l/2))
	(cairo:close-path)
	(cairo:stroke)))

    (unless e (setf e (make-hash-table)))

    (setf (gethash :node e) node
	  (gethash :entity e) quad
	  (gethash :texture e) tex
	  (gethash :type e) :wall
	  (gethash :body e) (make-box x y w h :actor e))
    e))


(defun make-quad-and-node (path &key parent (center :center))
  (let* ((node (make-instance 'node :parent parent))
	 (entity (make-quad-for-image
		  (concatenate 'string 
			       (directory-namestring
				(asdf:system-relative-pathname :indy-anna "indy-anna.asd"))
			       path)
		  :parent node :center center)))

    (values node entity (uniform entity "t1"))))


(defun make-ring (r &optional e)
  (let* ((d (* r 2))
	 (n (make-instance 'node :parent *root*))
	 (tex (make-instance 'texture :width d :height d))
	 (quad (make-quad-for-texture tex :parent n)))

    (fast-draw (:texture tex)
      (clear-cairo-context 0 0 0 0)
      (cairo:set-source-rgba 0 0 1 1)
      (cairo:set-line-width (* 3/10 r))
      (cairo:arc r r (* r 4/10) 0 (* 2 pi))
      (cairo:stroke))

    (unless e (setf e (make-hash-table)))

    (setf (gethash :node e) n
	  (gethash :entity e) quad
	  (gethash :texture e) tex
	  (gethash :friction e) 2.5
	  (gethash :type e) :ring
	  (gethash :body e) (squirl:world-add-body +world+ (make-ball 0 0 r :actor e :mass 1.0d0)))
    e))

(defun make-bounding-walls (w h tk) 
  (let* ((right (/ w 2))
	 (top (/ h 2))
	 (t/2 (/ tk 2)))
    
    ;; left wall
    
    (update-body
     (setf (gethash (incf +id-counter+) *hash*)
	   (make-wall nil (- right t/2) 0 tk h *root*)))
    
    (update-body
     (setf (gethash (incf +id-counter+) *hash*)
	   (make-wall nil (- t/2 right) 0 tk h *root*)))

    (update-body 
     (setf (gethash (incf +id-counter+) *hash*)
	   (make-wall nil 0 (- top t/2) w tk *root*)))

    (update-body 
     (setf (gethash (incf +id-counter+) *hash*)
	   (make-wall nil 0 (- t/2 top) w tk *root*)))))


(defevent *next* ()

  (init-physics)

  (update-body
   (setf (gethash :ring *hash*)
	 (make-ring 25)))

  (let ((anna (make-hash-table)))
    (setf (gethash :anna *hash*) anna)

    (setf (gethash :type anna) :player
	  (gethash :node anna) (make-instance 'node :parent *root*)
	  (gethash :walk-speed anna) 300
	  (gethash :body anna) (squirl:world-add-body +world+ (make-ball 0 0 20 :actor anna :mass 5.0d0)))
    
    (multiple-value-bind (node entity texture)   
	(make-quad-and-node "/assets/img/girl01.png" :parent (entity-value :anna :node))
      
      
      (setf (gethash :entity-node anna) node
	    (gethash :entity anna) entity
	    (gethash :texture anna) texture)
      (setf (entity-value :anna :entity-node) node)
      (setf (entity-value :anna :entity) entity)
      (setf (entity-value :anna :texture) texture)
      (!s (entity-value :anna :entity-node) 1/4 1/4 1/4)))

  (let ((guy (make-hash-table)))
    (setf (gethash :guy *hash*) guy)

    (setf (gethash :type guy) :enemy
	  (gethash :node guy) (make-instance 'node :parent *root*)
	  (gethash :walk-speed guy) 500
	  (gethash :body guy) (squirl:world-add-body +world+ (make-ball 0 0 20 :actor guy :mass 20.0d0)))
    
    (multiple-value-bind (node entity texture)   
	(make-quad-and-node "/assets/img/shooting.png" :parent (entity-value :guy :node))
      
      
      (setf (gethash :entity-node guy) node
	    (gethash :entity guy) entity
	    (gethash :texture guy) texture)
      (setf (entity-value :guy :entity-node) node)
      (setf (entity-value :guy :entity) entity)
      (setf (entity-value :guy :texture) texture)
      (!s (entity-value :guy :entity-node) 1/4 1/4 1/4)))

  


  (make-bounding-walls +vwidth+ +vheight+ 50))
;; (setf (gethash (incf +id-counter+) *hash*)
;; 	(make-wall (acons :type :wall nil) 50 50 250 50 *root*))





;; 2 is right-Y and 3 is right-X
(defevent *on-controller-axis-move* (controller-id axis-id position timestamp)

  (when (zerop controller-id)
    (let ((pos (float (/ position +max-axis+))))
      (case axis-id

	(5 nil)
	(4 nil)
	(3 (setf (aref +joystick-1+ 1) (- pos)))
	(2 (setf (aref +joystick-1+ 0) pos))
	(1 (setf (aref +joystick-2+ 1) (- pos)))
	(0 (setf (aref +joystick-2+ 0) pos))
	(t (format t "axis: ~A ~A~%" axis-id pos))))))

(defevent *on-controller-button-down* (controller-id button ts)
  (when (= button 10)
    (let ((anna (entity-value :anna :body))
	  (ring (entity-value :ring :body)))
      (setf (squirl:body-position ring) (squirl:body-position anna))
      (setf (squirl:body-velocity ring)
	    (+
	     (squirl:body-velocity (entity-value :anna :body))
	     (* 1500 (v->complex (v2:normalize (entity-value :anna :direction)))))))))


(defevent *on-idle* ()
  (declare (optimize (speed 3)))
  
  (if (not +last-frame-time+)
      (setf +last-frame-time+ 0.0)
      (multiple-value-bind (count rem) (floor (+ (/ *delta-ticks* 1000) +last-frame-time+) +physics-updates+)
	(setf +last-frame-time+ rem)
	;;(unless (= count 1) (format t "c: ~A t: ~A :l ~A~%" count rem +last-frame-time+))
	(dotimes (x count)

	  (when (> (v2:length +joystick-1+) (coerce +axis-dead-zone+ 'single-float))
	    (setf (entity-value :anna :direction) (v2:normalize +joystick-1+))
	    (setf (rotation (entity-value :anna :entity-node)) (joystick-to-rotation +joystick-1+))
	    
	     (let* ((b (entity-value :ring :body))
		    (v (complex->v (squirl:body-velocity b)))
		    (vn (v2:normalize v))
		    (s (v2:length v))
		    (js (v2:*s +joystick-1+ (* s .03)))
		    (dir (v2:*s
			  (v2:normalize
			   (v2:+ js v))
			  s)))
		    
	       (setf (squirl:body-velocity b) (v->complex  dir))))
	  

	  (let* ((len (v2:length +joystick-2+))
		 (unit (v2:normalize +joystick-2+))
		 (speed (if (< len +axis-dead-zone+)
			    0.0
			    (min 1.0
				 (/ (- (abs len) +axis-dead-zone+)
				    (- 1 +axis-dead-zone+))))))
	    
	    (setf (squirl:body-velocity (entity-value :anna :body))
		  (squirl:vec (* (aref unit 0) speed (entity-value :anna :walk-speed))
			      (* (aref unit 1) speed (entity-value :anna :walk-speed)))))
	  
	  ;; do the controls here
	  (let ((step (/ anna::+physics-updates+ 10)))
	    (dotimes (i 10)
	      (squirl:world-step +world+ step))))))
  
  (update-bodies)
  
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (render *root* :projection *projection*)
  (when *entity*
    (render *entity* :projection :*ortho-projection*)))



(defun start ()
  (init))


(let ((vs t))
  (defun toggle-vsync ()
    (clinch:! (if (setf vs (not vs))
		  (sdl2:gl-set-swap-interval 1)
		  (sdl2:gl-set-swap-interval 0)))))


