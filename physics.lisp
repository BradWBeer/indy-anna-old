 ;;;; physics.lisp

(in-package #:indy-anna)

(defparameter +world+ nil)
(defparameter +last-frame-time+ nil)
(defparameter +physics-updates+ (coerce 1/60 'double-float))
(defparameter +physics-timestep+ (coerce 1/600 'double-float))
(defparameter +friction+ 1.0)

(defclass player () ())
(defclass wall () ())

(squirl:defcollision ((a hash-table) (b hash-table) contacts)
		
		     (let ((type-a (gethash :type a))
			   (type-b (gethash :type b)))

		       (cond ((or (and (eq type-a :player) (eql type-b :ring))
				  (and (eql type-a :player) (eql type-b :ring))) nil)
			     (t t))))

(squirl:defcollision (a b contacts)
		     (format t "unknown Collision between ~A and ~B~%" a b)
		     t)

(defun reset-body (body)
  (setf (squirl:body-position body) (squirl:vec 0 0)
	(squirl:body-velocity body) (squirl:vec 0 0)
	(squirl::body-angular-velocity body) 0.0d0
	(squirl:body-rotation body) (squirl:vec 1 0)))
		 

(defun make-ball (x y r &key (bounce 1.0d0) (mass 1.0) actor)
  (squirl:make-body :mass mass
		    :inertia 0.0d0 
		    :calculate-inertia-p nil 
		    :position (squirl:vec x y)
		    :actor actor
		    :shapes (list
			     (squirl:make-circle r :restitution bounce))))

(defun make-box (x y w h &key (bounce 1.0d0) actor)
  (let ((verts (list (squirl:vec (* -1/2 w) (* -1/2 h))
		     (squirl:vec (* -1/2 w) (*  1/2 h))
		     (squirl:vec (*  1/2 w) (*  1/2 h))
		     (squirl:vec (*  1/2 w) (* -1/2 h)))))
    (squirl:world-add-body
     +world+
     (squirl:make-body
      :position (squirl:vec x y)
      :inertia 0.0d0
      :actor actor
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
  (let ((body (gethash :body x))
	(node (gethash :node x)))
    (when (and body node)
      (let ((p (squirl:body-position body))
	    (friction (gethash :friction x)))
	(when friction
	  (let ((v (complex->v (squirl:body-velocity body))))
	    (setf (squirl:body-velocity body)
		  (if (> (v2:length v) +friction+)
		      (v->complex (v2:- v (v2:*s (v2:normalize v) +friction+)))
		      (complex 0d0 0d0)))))
	    
	(!t node (realpart p) (imagpart p) 0 t)))))


(defun update-bodies ()
  (alexandria:maphash-values #'update-body *hash*))
