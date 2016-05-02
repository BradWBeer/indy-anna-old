 ;;;; physics.lisp

(in-package #:indy-anna)

(defparameter +world+ nil)
(defparameter +last-frame-time+ nil)
(defconstant +physics-timestep+ (coerce (/ (* 60 10)) 'double-float))
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
		 

(defun make-ball (x y)
  (squirl:make-body :mass 1.0 :inertia 0.0d0 :calculate-inertia-p nil :position (squirl:vec x y) :shapes (list (squirl:make-circle 20.0))))

(defun make-box (w h)
  (let ((verts (list (squirl:vec (* -1/2 w) (* -1/2 h))
		     (squirl:vec (* -1/2 w) (*  1/2 h))
		     (squirl:vec (*  1/2 w) (*  1/2 h))
		     (squirl:vec (*  1/2 w) (* -1/2 h)))))
    (squirl:world-add-body
     +world+
     (squirl:make-body
      :inertia 0.0d0
      :shapes (list (squirl:make-poly verts :friction 0.8))))))

(defun init-physics ()
  
  (setf +world+ (squirl:make-world :iterations 5))
  (squirl:resize-world-active-hash +world+ 20 100)
  (squirl:resize-world-static-hash +world+ 20 100)

  (setf +girl-body+ (squirl:world-add-body +world+ (make-ball 0 0))
	+w1-body+   (make-box 50 250))


  (setf (squirl:body-position +girl-body+) (squirl:vec 0 0)
	(squirl:body-velocity +girl-body+) (squirl:vec 0 0)
	(squirl::body-angular-velocity +girl-body+) 0.0d0
	(squirl:body-rotation +girl-body+) (squirl:vec 1 0))


  )

(defun update-physics () )

