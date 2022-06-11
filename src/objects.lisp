(in-package :lincl)

(defclass vec ()
  ((arr :initarg :arr :accessor arr)
   (length :accessor vec-length)
   (initial-element :initform 0 :initarg :initial-element :accessor vec-initial-element)))

(defclass mat (vec)
  ((dimensions
    :initarg :dimensions
    :initform (error "Dimensions required for matrix initialization.")
    :accessor dimensions)
   (rows :initarg :rows :accessor rows)
   (cols :initarg :cols :accessor cols)))

(defmethod vec-init ((v vec))
  (setf (vec-length v) (length (arr v))))

(defmethod initialize-instance :after ((m mat) &key dimensions)
  (with-slots (length arr rows cols)
      m
    (setf (slot-value m 'dimensions) dimensions)
    (setf rows (car dimensions) cols (cadr dimensions))
    (setf length (* (car dimensions)
                    (cadr dimensions)))))

(defmethod row ((m mat) num)
  (let ((row (make-array (cols m)))
        (offset (* (cols m) num)))
    (dotimes (n (cols m))
      (setf (aref row n) (aref (arr m) (+ offset n))))
    row))

(defmethod col ((m mat) num)
  (let ((col (make-array (rows m))))
    (loop for i from 0 below (rows m)
          do (setf (aref col i) (aref (arr m) (+ (* (cols m) i) num))))
    col))

(defmethod print-object ((m mat) stream)
  (let ((print-box-top (concatenate 'string "┌~" (write-to-string (+ (* (cols m) 5) 3)) "a┐~%"))
        (print-box-bottom (concatenate 'string "└~" (write-to-string (+ (* (cols m) 5) 3)) "a┘~%")))
    (format stream print-box-top #\Space)
    (dotimes (n (rows m))
      (format stream "│~{~5@A~^~}   │~%" (coerce (row m n) 'list)))
    (format stream print-box-bottom #\Space)))

(defmacro v (&rest vals)
  `(vector ,@vals))

(defun mat (dim &key (random nil) (fill nil) (transpose nil) (direct nil))
  (let ((len (* (car dim) (cadr dim))))
    (make-instance 'mat :dimensions dim :arr
                   (cond
                     (random
                      (coerce (loop for i from 0 below len collect
                                                           (if (evenp (random random))
                                                               (random random)
                                                               (* (random 10) -1))) 'vector))
                     (fill
                      (coerce (loop for i from 0 below len collect fill) 'vector))
                     (direct
                      (unless (eq len (length direct))
                        (error "Specified dimensions do not equal length of supplied array."))
                      direct)
                     (transpose
                      (if (not (eq (car dim) (cadr dim)))
                          (error "Transpose matrix requires square matrix.")
                          (let ((z-mat (coerce (loop for i from 0 below len collect 0) 'vector)))
                            (do ((count 0 (+ (cadr dim) count))
                                 (row 0 (1+ row)))
                                ((eq row (car dim)) z-mat)
                              (setf (aref z-mat (+ count row)) 1)))))
                     (t
                      (coerce (loop for i from 0 below len collect 0) 'vector))))))
