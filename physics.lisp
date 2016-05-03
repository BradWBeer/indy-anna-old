 ;;;; physics.lisp

(in-package #:indy-anna)

(defparameter +world+ nil)
(defparameter +last-frame-time+ nil)
(defparameter +physics-updates+ (coerce 1/60 'double-float))
(defparameter +physics-timestep+ (coerce 1/600 'double-float))
(defparameter +girl-body+ nil)
(defparameter +w1-body+ nil)

(defclass player () ())
(defclass wall () ())

(squirl:defcollision ((a player) (b wall) contacts)
	      ;;(format t "Collision between player and wall ~A~%" contacts)
	      t)

(defun reset-body (body)
  (setf (squirl:body-position body) (squirl:vec 0 0)
	(squirl:body-velocity body) (squirl:vec 0 0)
	(squirl::body-angular-velocity body) 0.0d0
	(squirl:body-rotation body) (squirl:vec 1 0)))
		 

(defun make-ball (x y r &optional (bounce 1.0d0))
  (squirl:make-body :mass 1.0 :inertia 0.0d0 :calculate-inertia-p nil :position (squirl:vec x y) :shapes (list (squirl:make-circle r :restitution bounce))))

(defun make-box (x y w h &optional (bounce 1.0d0))
  (let ((verts (list (squirl:vec (* -1/2 w) (* -1/2 h))
		     (squirl:vec (* -1/2 w) (*  1/2 h))
		     (squirl:vec (*  1/2 w) (*  1/2 h))
		     (squirl:vec (*  1/2 w) (* -1/2 h)))))
    (squirl:world-add-body
     +world+
     (squirl:make-body
      :position (squirl:vec x y)
      :inertia 0.0d0
      :shapes (list (squirl:make-poly verts :friction 0.8 :restitution bounce))))))

(defun init-physics ()
  
  (setf +world+ (squirl:make-world :iterations 1))
  (squirl:resize-world-active-hash +world+ 20 100)
  (squirl:resize-world-static-hash +world+ 20 100)

  ;; (setf +girl-body+ (squirl:world-add-body +world+ (make-ball 0 0))
  ;; 	+w1-body+   (make-box 50 250))


  ;; (setf (squirl:body-position +girl-body+) (squirl:vec 0 0)
  ;; 	(squirl:body-velocity +girl-body+) (squirl:vec 0 0)
  ;; 	(squirl::body-angular-velocity +girl-body+) 0.0d0
  ;; 	(squirl:body-rotation +girl-body+) (squirl:vec 1 0))


  )

(defun update-physics ()

  )

(defun update-body (x)
  (let ((body (cdr (assoc :body x)))
	(node (cdr (assoc :node x))))
    (when (and body node)
      (let ((p (squirl:body-position body)))
	(!t node (realpart p) (imagpart p) 0 t)))))


(defun update-bodies ()
  (alexandria:maphash-values #'update-body *hash*))
