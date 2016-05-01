;;;; package.lisp

(defpackage #:indy-anna
  (:use #:cl #:clinch)
  (:nicknames :anna)
  (:export
   start
   +MAX-AXIS+ +AXIS-DEAD-ZONE+ +JOYSTICK-1+ +JOYSTICK-2+ +GIRL-NODE+
   +GIRL-ENTITY+ +BLUE-ARROW-NODE+ +BLUE-ARROW-ENTITY+ +ORANGE-ARROW-NODE+
   +CHARACTER-NODE+
   +ORANGE-ARROW-ENTITY+))

