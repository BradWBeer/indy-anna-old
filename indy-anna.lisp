 ;;;; indy-anna.lisp

(in-package #:indy-anna)

(defparameter +id-counter+ 0)
(defconstant +max-axis+ 32767)
(defparameter +axis-dead-zone+ .40)
(defparameter +joystick-1+ (v! 0 0))
(defparameter +joystick-2+ (v! 0 0))
(defparameter *hash* (make-hash-table))

(defevent *on-window-resized* (window width height timestamp)
  (setf *projection* *ortho-projection*))

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

    (setf e (acons :node node 
		   (acons :body (make-box x y w h)
			  (acons :entity quad
				 (acons :texture tex
					(acons :type :wall e))))))))


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

    (acons :node n
	   (acons :body (squirl:world-add-body +world+ (make-ball 0 0 r ))
		  (acons :entity quad
			 (acons :texture tex
				(acons :type :ring e)))))))



(defevent *next* ()

  (init-physics)

  (set-entity-value :anna :type :player)
  (set-entity-value :anna :node (make-instance 'node :parent *root*))
  (set-entity-value :anna :body (squirl:world-add-body +world+ (make-ball 0 0 20)))
  (set-entity-value :anna :walk-speed 500)
  (multiple-value-bind (node entity texture)   
      (make-quad-and-node "/assets/img/girl01.png" :parent (get-entity-value :anna :node))
    (set-entity-value :anna :entity-node node)
    (set-entity-value :anna :entity entity)
    (set-entity-value :anna :texture texture))
  (!s (anna::get-entity-value :anna :entity-node) 1/4 1/4 1/4)

  (update-body
   (setf (gethash :ring *hash*)
	 (make-ring 25)))
  (make-bounding-walls (width *viewport*) (height *viewport*) 50)

  ;; (setf (gethash (incf +id-counter+) *hash*)
  ;; 	(make-wall (acons :type :wall nil) 50 50 250 50 *root*))


  )
(defun make-bounding-walls (w h tk) 
  (let* ((right (/ w 2))
	 (top (/ h 2))
	 (t/2 (/ tk 2)))
   
    ;; left wall
    
    (update-body
     (setf (gethash (incf +id-counter+) *hash*)
	   (make-wall (acons :type :wall nil) (- right t/2) 0 tk h *root*)))
    
    (update-body
     (setf (gethash (incf +id-counter+) *hash*)
	   (make-wall (acons :type :wall nil) (- t/2 right) 0 tk h *root*)))

    (update-body 
     (setf (gethash (incf +id-counter+) *hash*)
	   (make-wall (acons :type :wall nil) 0 (- top t/2) w tk *root*)))

    (update-body 
     (setf (gethash (incf +id-counter+) *hash*)
	   (make-wall (acons :type :wall nil) 0 (- t/2 top) w tk *root*)))))


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
    (let ((anna (get-entity-value :anna :body))
	  (ring (get-entity-value :ring :body)))
      (setf (squirl:body-position ring) (squirl:body-position anna))
      (setf (squirl:body-velocity ring)
	    (+ (squirl:body-velocity anna)
	       (* 500 (v->complex (get-entity-value :anna :direction))))))))
	  

(defevent *on-idle* ()
  (declare (optimize (speed 3)))
  
  (if (not +last-frame-time+)
      (setf +last-frame-time+ 0.0)
      (multiple-value-bind (count rem) (floor (+ (/ *delta-ticks* 1000) +last-frame-time+) +physics-updates+)
	(setf +last-frame-time+ rem)
	;;(unless (= count 1) (format t "c: ~A t: ~A :l ~A~%" count rem +last-frame-time+))
	(dotimes (x count)

	  (when (> (v2:length +joystick-1+) (coerce +axis-dead-zone+ 'single-float))
	    (set-entity-value :anna :direction +joystick-1+)
	    (setf (rotation (get-entity-value :anna :entity-node)) (joystick-to-rotation +joystick-1+))
	    
	    (let ((b (get-entity-value :ring :body)))

	      (setf (squirl:body-velocity b)
		    (+ (squirl:body-velocity b) (v->complex (v2:*s +joystick-1+ 10.0))))))
	    
	    

	  (let* ((len (v2:length +joystick-2+))
		 (unit (v2:normalize +joystick-2+))
		 (speed (if (< len +axis-dead-zone+)
			     0.0
			     (min 1.0
				  (/ (- (abs len) +axis-dead-zone+)
				     (- 1 +axis-dead-zone+))))))
	    
	    ;; (squirl:vec (* (aref unit 0) speed (get-entity-value :anna :walk-speed))
	    ;; 		(* (aref unit 1) speed (get-entity-value :anna :walk-speed)))
	    
	    
	    (setf (squirl:body-velocity (get-entity-value :anna :body))
		  (squirl:vec (* (aref unit 0) speed (get-entity-value :anna :walk-speed))
			      (* (aref unit 1) speed (get-entity-value :anna :walk-speed)))))
	  
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
