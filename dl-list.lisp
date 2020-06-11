;;;; dl-list.lisp

(in-package #:dl-list)

;; good enough
(declaim (optimize (speed 3) (compilation-speed 0)))


;; maybe should make public
(declaim (inline %node))
(defstruct (%node
            (:constructor %node (val left right)))
  (val nil :type t)
  (left nil :type (or null %node))
  (right nil :type (or null %node)))

;; FIXME this is a bad way
(defmethod print-object ((obj %node) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "-~a-" (%node-val obj))))

(declaim (ftype (function () (values %node &optional)) %make-origin))
(defun %make-origin ()
  (let ((%origin (%node nil nil nil)))
    (setf (%node-left %origin) %origin
          (%node-right %origin) %origin)))


;; #+sbcl inherit from sequence?
(defclass dl-list ()
  ((origin
    :type %node
    :initform (%make-origin))
   (length
    :type (integer 0 4611686018427387901)
    :initform 0)))

(defmethod print-object ((obj dl-list) stream)
  (print-unreadable-object (obj stream :type t)
    (loop :with origin := (slot-value obj 'origin)
          :for this := (%node-left origin) :then (%node-left this)
          :until (eq this origin)
          :do (format stream "~a" this))))


;; Constructor
(declaim (ftype (function (&key (:type symbol)) (values dl-list &optional)) make-dl-list))
(defun make-dl-list (&key type)
  (declare (ignore type))
  (make-instance 'dl-list))


(defun front (dllist)
  (%node-val (%node-left (slot-value dllist 'origin))))

(defun back (dllist)
  (%node-val (%node-right (slot-value dllist 'origin))))

;; Push
(declaim (ftype (function (%node t) (values &optional)) %insert-left-after))
(defun %insert-left-after (node value)
  (with-slots (left) node
    (with-slots (right) left
      (setf right (%node value left node)
            left right)))
  (values))

(declaim (ftype (function (%node t) (values &optional)) %insert-right-after))
(defun %insert-right-after (node value)
  (with-slots (right) node
    (with-slots (left) right
      (setf left (%node value node right)
            right left)))
  (values))

(declaim (ftype (function (t dl-list) (values t &optional)) push-front))
(defun push-front (value dllist)
  (with-slots (origin length) dllist
    (%insert-left-after origin value)
    (incf length)
    value))

(declaim (ftype (function (t dl-list) (values t &optional)) push-back))
(defun push-back (value dllist)
  (with-slots (origin length) dllist
    (%insert-right-after origin value)
    (incf length)
    value))

;; Pop (goes here)


(defun pop-back (dllist)
  (if (zerop (slot-value dllist 'length))
      (error 'simple-error :format-control "There is nothing left to pop")
      (prog1
          (back dllist)
        (with-slots (origin length) dllist
          (with-slots (left) (%node-right origin)
            (setf left origin
                  (%node-right origin) left)
            (decf length))))))


;; Utility (emptyp? something else)
(declaim (ftype (function (dl-list) (values (integer 0 4611686018427387901) &optional)) size))
(defun size (dllist)
  (slot-value dllist 'length))
