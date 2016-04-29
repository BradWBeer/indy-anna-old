;;;; indy-anna.lisp

(in-package #:indy-anna)

(defconstant +max-axis+ 32767)
(defparameter +axis-dead-zone+ .40)
(defparameter +joystick-1+ (v! 0 0))
(defparameter +joystick-2+ (v! 0 0))
(defparameter +girl-node+ nil)
(defparameter +girl-entity+ nil)
(defparameter +blue-arrow-node+ nil)
(defparameter +blue-arrow-entity+ nil)
(defparameter +orange-arrow-node+ nil)
(defparameter +orange-arrow-entity+ nil)

;;; "indy-anna" goes here. Hacks and glory await!


(defevent *on-window-resized* (window width height timestamp)
 (setf *projection* clinch::*ortho-projection*))

(defun make-quad-and-node (path &optional parent &key (center :center))
  (let* ((node (make-instance 'node))
	 (entity (clinch:make-quad-for-image
		  (concatenate 'string 
			       (directory-namestring
				(asdf:system-relative-pathname :indy-anna "indy-anna.asd"))
			       path)
		  :parent node :center center)))
    (add-child parent node)
    (values node entity)))
    

(defevent *next* ()
  (multiple-value-setq (+blue-arrow-node+ +blue-arrow-entity+)
    (make-quad-and-node "/assets/img/arrow-blue.png" *root* :center :bottom-center))
  (multiple-value-setq (+orange-arrow-node+ +orange-arrow-entity+)
    (make-quad-and-node "/assets/img/arrow-orange.png" *root* :center :bottom-center))
  (multiple-value-setq (+girl-node+ +girl-entity+)
    (make-quad-and-node "/assets/img/girl01.png" *root*))
  
  (scale +girl-node+ (v! .25 .25 .25))
  (scale +orange-arrow-node+ (v! .2 .4 .2))
  (scale +blue-arrow-node+ (v! -.2 -.4 .2)))
  
		       

;; 2 is right-Y and 3 is right-X
(defevent *on-controller-axis-move* (controller-id axis-id position timestamp)
  (when (zerop controller-id)
    (let ((pos (float (/ position +max-axis+))))
      (when (> (abs pos) +axis-dead-zone+)
	(case axis-id
	  (3 (setf (aref +joystick-1+ 0) pos))
	  (2 (setf (aref +joystick-1+ 1) pos))
	  (1 (setf (aref +joystick-2+ 0) (- pos)))
	  (0 (setf (aref +joystick-2+ 1) (- pos))))))))



(defevent *on-idle* ()
  
  ;;(setf (rotation +girl-node+) (q:from-fixed-angles 0 0
  ;;(clinch:degrees->radians 0)))

  ;; 

  (let ((vec (v2:normalize +joystick-1+)))
    (setf (rotation +blue-arrow-node+)
	  (setf (rotation +girl-node+)
		(rtg-math.quaternions:from-fixed-angles 0 0 (atan (aref vec 1) (aref vec 0))))))

  (let ((vec (v2:normalize +joystick-2+)))
    (setf (rotation +orange-arrow-node+)
	  (rtg-math.quaternions:from-fixed-angles 0 0 (atan (aref vec 1) (aref vec 0)))))

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render *root* :projection *projection*)
  (when *entity*
    (clinch:render *entity* :projection clinch::*ortho-projection*)))



(defun start ()
  (init))
