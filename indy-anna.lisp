 ;;;; indy-anna.lisp

(in-package #:indy-anna)

(defconstant +max-axis+ 32767)
(defparameter +axis-dead-zone+ .40)
(defparameter +joystick-1+ (v! 0 0))
(defparameter +joystick-2+ (v! 0 0))
(defparameter +character-node+ nil)
(defparameter +girl-node+ nil)
(defparameter +girl-entity+ nil)
(defparameter +girl-velocity+ (v! 0 0 0))
(defparameter +girl-walk-speed+ 500)
(defparameter +blue-arrow-node+ nil)
(defparameter +blue-arrow-entity+ nil)
(defparameter +orange-arrow-node+ nil)
(defparameter +orange-arrow-entity+ nil)

(defparameter +magic-circle-blue-node+ nil)
(defparameter +magic-circle-blue-entity+ nil)
(defparameter +magic-circle-green-node+ nil)
(defparameter +magic-circle-green-entity+ nil)

(defparameter +w1-node+ nil)
(defparameter +w1-entity+ nil)



(defevent *on-window-resized* (window width height timestamp)
 (setf *projection* *ortho-projection*))

(defmethod sort-children ((node clinch:node))
  (sort (loop for o in (children node)
	   if (typep o 'node)
	   collect o)
	(lambda (a b)
	  (< (aref (translation a) 2)
	     (aref (translation b) 2)))))

(defun make-wall (w h &optional parent)
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
    (values node quad tex)))
	 	 
(defun make-quad-and-node (path &optional parent &key (center :center))
  (let* ((node (make-instance 'node))
	 (entity (make-quad-for-image
		  (concatenate 'string 
			       (directory-namestring
				(asdf:system-relative-pathname :indy-anna "indy-anna.asd"))
			       path)
		  :parent node :center center)))
    (add-child parent node)
    (values node entity)))
    

(defevent *next* ()
  
  (setf +character-node+
	(make-instance 'node))
  (add-child *root* +character-node+)

  (multiple-value-setq (+magic-circle-blue-node+ +magic-circle-blue-entity+)
    (anna::make-quad-and-node "/assets/img/magic_circle_blue.jpg" +character-node+))
  (multiple-value-setq (+magic-circle-green-node+ +magic-circle-green-entity+)
    (anna::make-quad-and-node "/assets/img/magic_circle_green.png" +character-node+)) 

  (multiple-value-setq (+orange-arrow-node+ +orange-arrow-entity+)
    (make-quad-and-node "/assets/img/arrow-orange.png" +character-node+ :center :bottom-center))
  (multiple-value-setq (+blue-arrow-node+ +blue-arrow-entity+)
    (make-quad-and-node "/assets/img/arrow-blue.png" +character-node+ :center :bottom-center))

  (multiple-value-setq (+girl-node+ +girl-entity+)
    (make-quad-and-node "/assets/img/girl01.png" +character-node+))
  
  (multiple-value-setq (+w1-node+ +w1-entity+) (make-wall 250 50))
  (add-child *root* +w1-node+)
    
  
  (!s +girl-node+ .25 .25 .25)
  (!s +orange-arrow-node+ .2 .4 .2)
  (!s +blue-arrow-node+ .2 .4 .2)
  (!s +magic-circle-blue-node+ 0 0 1)
  (!s +magic-circle-green-node+ 0 0 1)

  (!t +girl-node+ 0 0 10)
  (!t +orange-arrow-node+ 0 0 2)
  (!t +blue-arrow-node+ 0 0 1)
  (!t +magic-circle-blue-node+ 0 0 -2)
  (!t +magic-circle-green-node+ 0 0 -1)
  (!t +w1-node+ 0 0 5)
  
  (setf (children +character-node+)
	(sort-children +character-node+))

  (init-physics)
  
  (setf +last-frame-time+ (/ (sdl2:get-ticks) 1000)))

  
;; 2 is right-Y and 3 is right-X
(defevent *on-controller-axis-move* (controller-id axis-id position timestamp)
  (when (zerop controller-id)
    (let ((pos (float (/ position +max-axis+))))
      (case axis-id
	(5 (!s +magic-circle-blue-node+ pos pos 1 t))
	(4 (!s +magic-circle-green-node+ pos pos 1 t))
	(3 (setf (aref +joystick-1+ 1) (- pos)))
	(2 (setf (aref +joystick-1+ 0) pos))
	(1 (setf (aref +joystick-2+ 1) (- pos)))
	(0 (setf (aref +joystick-2+ 0) pos))
	(t (format t "axis: ~A ~A~%" axis-id pos))))))

(defevent *on-idle* ()
  (declare (optimize (speed 3)))
  
   (multiple-value-bind (count remainder) (floor (+ (/ *delta-ticks* 1000) +last-frame-time+) +physics-timestep+)
     ;;(print (list (float (/ *delta-ticks* 1000)) count remainder))
     (setf +last-frame-time+ remainder)
     (when (> *delta-ticks* 19) (format t "Overtime: ~A Steps: ~A~%" *delta-ticks* count))
     (dotimes (i (max 0 count))
       (time (squirl:world-step +world+ +physics-timestep+))))
  
  (let ((vec (v2:normalize +joystick-1+)))
    (setf (rotation +blue-arrow-node+)
  	  (setf (rotation +girl-node+)
			  (rtg-math.quaternions:from-fixed-angles 0 0 (- (atan (aref vec 0) (aref vec 1)))))))

  (let* ((len (v2:length +joystick-2+))
	 (unit (v2:normalize +joystick-2+))
	 (speed (if (< len +axis-dead-zone+)
		    0.0
		    (min 1.0
			 (/ (- (abs len) +axis-dead-zone+)
			    (- 1 +axis-dead-zone+))))))
    (setf (squirl:body-velocity +girl-body+) (squirl:vec (* (aref unit 0) speed +girl-walk-speed+) (* (aref unit 1) speed +girl-walk-speed+) ))
    ;; (when (> speed .1)
    ;;   (print (squirl:body-velocity +girl-body+)))
    (setf (rotation +orange-arrow-node+)
	  (rtg-math.quaternions:from-fixed-angles 0 0 (- (atan (aref +joystick-2+ 0) (aref +joystick-2+ 1)))))
    (!s +orange-arrow-node+ .2 (* .4 speed) .2 t)
    (setf (enabled +orange-arrow-node+)
	  (> (v2:length +joystick-2+) +axis-dead-zone+))
       

    ;;(!t +character-node+ (* (aref unit 0) speed +girl-walk-speed+) (* (aref unit 1) speed +girl-walk-speed+) 0.0)
    )

  ;;(print (squirl:body-rotation anna::+girl-body+))

  (!t +character-node+ (realpart (squirl:body-position +girl-body+)) (imagpart (squirl:body-position +girl-body+)) 0 t)

  (!t +w1-node+ (realpart (squirl:body-position +w1-body+)) (imagpart (squirl:body-position +w1-body+)) 0 t)
  (setf (rotation +w1-node+)
	(rtg-math.quaternions:from-fixed-angles
	 0 0
	 (coerce (-
		  (atan (realpart (squirl:body-rotation anna::+w1-body+)) 
			(imagpart (squirl:body-rotation anna::+w1-body+))))
		  'single-float)))
  
  (rotate +magic-circle-green-node+ 
   		 (q:from-fixed-angles 0 0
   				      (degrees->radians -2)))

  (rotate +magic-circle-blue-node+ 
   		 (q:from-fixed-angles 0 0
   				      (degrees->radians 2)))


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
