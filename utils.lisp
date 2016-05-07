 ;;;; utils.lisp

(in-package #:indy-anna)

(defmacro set-entity-value (key name value)
  (let ((k (gensym))
	(n (gensym))
	(v (gensym))
	(lst (gensym))
	(entry (gensym)))
    `(let* ((,k ,key)
	    (,n ,name)
	    (,v ,value)
	    (,lst (gethash ,k *hash*))
	    (,entry (assoc ,n ,lst)))
       (if ,entry
	   (setf (cdr ,entry) ,v)
	   (setf (gethash ,k *hash*)
		 (acons ,n ,v ,lst)))
       ,v)))

(defmacro entity-value (key name)
  `(gethash ,name (gethash ,key *hash*)))

(defun joystick-to-rotation (js)
  (let ((vec (v2:normalize js)))
    (rtg-math.quaternions:from-fixed-angles
     0 0 (- (atan (aref vec 0)
		  (aref vec 1))))))

(defun v->complex (v)
  (complex (* (aref v 0) 1d0) (* (aref v 1) 1d0)))

(defun complex->v (c)
  (v2:make (coerce (realpart c) 'single-float) (coerce (imagpart c) 'single-float)))


(defun map-true (f lst)
  (when lst
    (let ((result (funcall f (car lst))))
      (if result 
	  (cons (car lst) (map-true f (cdr lst)))
	  (map-true f (cdr lst))))))

(defun find-matches (cell lst)
  (map-true (lambda (l)
	      (when (eql (cdr cell) (cdr (assoc (car cell) l))) l))
	    lst))

(defun assoc-acons (key value lst &key (test #'eql))
  (if value 
      (let ((result (assoc key lst :test test)))
	(if result
	    (progn
	      (setf (cdr result) value)
	      lst)
	    (acons key value lst)))
      (loop for a in lst
	 unless (funcall test key (car a))
	 collect a)))
	     
(defmethod sort-children ((node clinch:node))
  (sort (loop for o in (children node)
	   if (typep o 'node)
	   collect o)
	(lambda (a b)
	  (< (aref (translation a) 2)
	     (aref (translation b) 2)))))
