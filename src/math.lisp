(in-package #:lincl)

(defclass linear-system ()
  ((equation-lst :initform :equation-lst :initarg :equation-lst :accessor equation-lst)
   (start-matrix :initform :start-matrix :initarg :start-matrix :accessor start-matrix)
   (rows :initform :rows :initarg :rows :accessor rows)
   (cols :initform :cols :initarg :cols :accessor cols)))

(defclass matrix-op ()
  ((matrix :initform :matrix :initarg :matrix :accessor matrix)
   (matrix-post :initform nil :initarg :matrix-post :accessor matrix-post)
   (op-list :initform '() :initarg :op-list :accessor op-list)
   (rows :initform :rows :initarg :rows :accessor rows)))

(defclass matrix ()
  ((mat-vec :initform :mat-vec :initarg :mat-vec :accessor mat-vec)
   (diff-rows :initform nil :initarg :diff-rows :accessor diff-rows)
   (rows :initform :rows :initarg :rows :accessor rows)
   (cols :initform :cols :initarg :cols :accessor cols)))


(defmethod initialize-instance :after ((op matrix) &rest initargs)
  (with-slots (mat-vec rows cols) op
    (setf rows (car (array-dimensions mat-vec)))
    (setf cols (cadr (array-dimensions mat-vec)))))

(defmethod initialize-instance :after ((op linear-system) &rest initargs)
  (with-slots (equation-lst start-matrix rows cols) op
    (setf start-matrix (build-matrix (parse-equations equation-lst)))
    (setf rows (car (array-dimensions start-matrix)))
    (setf cols (cadr (array-dimensions start-matrix)))))

(defmacro key (lst)
  (let ((item (gensym)))
    `(case (car ,lst)
       (for (setf item 'FOR)))
    (print item)))

(defmacro foo (&rest ds)
  (let ((args (gensym)))
    `(lambda (&rest ,args)
       (case (car ,args)
         ,@(mapcar (lambda (d)
                     `(,(if (eq t (car d))
                            t
                            (list (car d)))
                       (apply (lambda ,@(cdr d))
                              ,(if (eq t (car d))
                                   args
                                   `(cdr ,args)))))
            ds)))))

(defun forward-solve (matrix &key (row 0) (col 0))
  (let* ((mat (mdup matrix))
         (rows (car (array-dimensions matrix)))
         (cols (cadr (array-dimensions matrix)))
         (steplist '()))
      (if (eq row rows)
          (values mat (reverse steplist))
          (labels ((one-and-clear (mat)
                     (let ((col-one-p (remove row (col-contains-1 mat col) :test #'>)))
                       (if col-one-p
                           (if (eq row (car col-one-p))
                               (do ((nth-row (+ row 1) (incf nth-row)))
                                   ((eq nth-row rows))
                                 (doc-do (mult-and-add mat nth-row (v mat row) (v-gen (* (aref mat nth-row col) -1) cols))))
                               (one-and-clear (doc-do (swap-rows mat (car col-one-p) row))))
                           (let* ((target-row (car (em-whole? (mi-n mat #'/ col :start-row row) :start-row row))))
                             (one-and-clear (doc-do (ei-n mat #'/ target-row col))))))))
            (one-and-clear mat)))
    (values mat (reverse steplist))))

(defun solve (mat)
  (let ((rref-lst (list (make-rref-operation :matrix mat)))
        (endpoint nil))
    ;; Forward solve the augmented matrix
    (do* ((row 0 (1+ row))
          (col 0 (1+ col)))
         ((eq row (car (array-dimensions mat))) (setf endpoint (list (- row 1) (- col 1))))
      (multiple-value-bind (matrix steplist)
          (forward-solve mat :row row :col col)
        (push (make-rref-operation :matrix matrix
                                   :steplist steplist
                                   :diff (m-diff matrix (rref-operation-matrix (car rref-lst)))) rref-lst)
        (setf mat matrix)))
    ;; Backward solve the augmented matrix
    (do* ((row (car endpoint) (1- row))
          (col (cadr endpoint) (1- col)))
         ((eq row 0))
      (multiple-value-bind (matrix steplist)
          (backward-solve mat :row row :col col)
        (push (make-rref-operation :matrix matrix
                                   :steplist steplist
                                   :diff (m-diff matrix (rref-operation-matrix (car rref-lst)))) rref-lst)
        (setf mat matrix)))
    (reverse rref-lst)))

(defun trim-whitespace (str)
  (let ((len (length str)))
    (if (eq #\Space (char str (- len 1)))
        (trim-whitespace (subseq str 0 (- len 1)))
        str)))

(defun split-string (str)
  (let ((groups nil))
    (with-input-from-string (in str)
      (do* ((len (length str))
            (pos 0)
            (tok ""))
           ((eq pos len) (unless (eq tok "")
                           (push tok groups)))
        (let ((c (read-char in)))
          (if (eq c #\Space)
              (and (> (length tok) 0)
                   (progn (push tok groups)
                          (setf tok "")))
              (setf tok (concatenate 'string tok (string c)))))
        (setf pos (incf pos))))
    (reverse groups)))

(defun parse-token (str)
  (let* ((contains (map 'list #'(lambda (x) (if (alpha-char-p x) 'alpha 'digit)) str))
         (len (length contains))
         (variable-p (member 'alpha contains))
         (number-p (member 'digit contains)))
    (if (string-equal str "=")
        nil                     ;; FIXME: we need to track the equals column
        (if variable-p
            (if number-p
                (cons (read-from-string (subseq str (- len (length variable-p)) len))
                      (read-from-string (subseq str 0 (- len (length variable-p)))))
                (cons (read-from-string (subseq str (- len (length variable-p)) len))
                      1))
            (cons 'COEFFICIENT (read-from-string str))))))

(defun parse-string (str)
  (remove nil (map 'list #'parse-token (split-string str))))

(defun parse-equations (str-list)
  (mapcar #'parse-string str-list))

(defun longest-sublist-n (lst)
  (let ((longest 0)
        (lengths (mapcar #'length lst)))
    (dotimes (n (length lengths))
      (and (> (nth n lengths) (nth longest lengths))
           (setf longest n)))
    longest))

(defun column-positions (lst)
  (let ((final '()))
    (dotimes (j (length lst))
      (push (cons (car (nth j lst)) j) final))
    final))

(defun build-matrix (grouplist)
  (let* ((rows (length grouplist))
         (longest (nth (longest-sublist-n grouplist) grouplist))
         (cols (length longest))
         (positions (column-positions longest))
         (mat (make-array (list rows cols))))
    (loop for i from 0 below rows
          do (loop for j in (nth i grouplist)
                   do (setf (aref mat i (cdr (assoc (car j) positions))) (cdr j))))
    mat))


(defun print-matrix (mat)
  (let ((dimensions (array-dimensions mat)))
    (terpri)
    (dotimes (row (car dimensions))
      (dotimes (col (cadr dimensions))
        (format t "~8A" (aref mat row col)))
      (format t "~%~%"))))

(defun print-vector (vec)
  (let ((cols (car (array-dimensions vec))))
    (dotimes (col cols)
      (format t "~A" (aref vec col)))))

(defun swap-rows (mat from to)
  (let ((from-v (v mat from))
        (to-v (v mat to)))
    (loop for j from 0 below (length from-v)
          do (setf (aref mat to j) (aref from-v j)))
    (loop for j from 0 below (length to-v)
          do (setf (aref mat from j) (aref to-v j))))
  mat)

(defun ii-product (v1 v2)
  (do* ((cols (car (array-dimensions v1)))
        (cur 0 (incf cur))
        (result-vec (make-array cols)))
       ((eq cur cols) result-vec)
    (setf (aref result-vec cur) (funcall #'* (aref v1 cur) (aref v2 cur)))))

(defun ij (fn v1 j)
  (do* ((cols (car (array-dimensions v1)))
        (cur 0 (incf cur))
        (result-vec (make-array cols)))
       ((eq cur cols) result-vec)
    (setf (aref result-vec cur) (funcall fn (aref v1 cur) j))))

(defun col-contains-1 (mat col &key (start-row 0))
  (remove nil (loop for i from start-row below (car (array-dimensions mat))
                    collect (and (eq 1 (aref mat i col))
                                 i))))

(defun veq (v0 v1)
  (not (member nil (map 'list #'eq v0 v1))))

;; Return row ROW of matrix MAT
(defun v (mat row)
  (do* ((cols (cadr (array-dimensions mat)))
        (vec (make-array cols))
        (pos 0 (incf pos)))
       ((eq pos cols) vec)
    (setf (aref vec pos) (aref mat row pos))))

;; Return column N of matrix MAT
(defun c (mat n)
  (do* ((rows (car (array-dimensions mat)))
        (col (make-array (list rows 1)))
        (pos 0 (incf pos)))
       ((eq pos rows) col)
    (setf (aref col pos 0) (aref mat pos n))))

(defun mdup (mat)
  (loop for i from 0 below (car (array-dimensions mat))
        with new = (make-array (array-dimensions mat))
        do (loop for j from 0 below (cadr (array-dimensions mat))
                 do (setf (aref new i j) (aref mat i j)))
        finally (return new)))

(defun v-gen (int n)
  (make-array n :initial-element int))

(defun m-col (mat col)
  (do* ((rows (car (array-dimensions mat)))
        (new (make-array rows))
        (row 0 (1+ row)))
       ((eq row rows) new)
    (setf (aref new row) (aref mat row col))))

(defmacro vlist (mat &rest rows)
  `(let ((r (list ,@rows)))
     (mapcar #'(lambda (x) (v ,mat x)) r)))

(defun ei-n (mat fn row col)
  (let ((rows (car (array-dimensions mat)))
        (cols (cadr (array-dimensions mat))))
    (loop for i below rows
          do (loop for j below cols
                   with val = (aref mat i col)
                   do (if (eq row i)
                          (setf (aref mat i j) (funcall fn (aref mat i j) val))
                          (setf (aref mat i j) (aref mat i j)))))
    mat))

(defmethod mn/ ((m mat) n)
  (map 'vector #'/ (arr m) (make-sequence 'vector (length (arr m)) :initial-element n)))

(defmethod mn* ((m mat) n)
  (map 'vector #'* (arr m) (make-sequence 'vector (length (arr m)) :initial-element n)))

(defmethod mn+ ((m mat) n)
  (map 'vector #'+ (arr m) (make-sequence 'vector (length (arr m)) :initial-element n)))

(defmethod mn- ((m mat) n)
  (map 'vector #'- (arr m) (make-sequence 'vector (length (arr m)) :initial-element n)))

;; When dividing each column in row matrix M by column number COL, which row gives us the greatest number of whole numbers?
(defmethod most-whole-numbers ((m mat) col &key (start-row 0))
  (let ((total '())
        (row-lst (mat-rows m :start-row start-row)))
    (dotimes (n (rows m))
      (let ((cur (nth n row-lst)))
        (unless (zerop (elt cur col))
          (push (cons n (map-into cur #'/ cur (vgen (length cur) (elt cur col)))) total))))
    (sort (mapcar #'(lambda (x)
                (cons (car x) (loop for i across (map 'vector #'integerp (cdr x)) count i)))
            total)
          #'> :key #'cdr)))

(defun m-diff (m0 m1)
  (and (equal (array-dimensions m0) (array-dimensions m1))
       (do ((row-diffs '())
            (cur-row 0 (incf cur-row)))
           ((eq cur-row (car (array-dimensions m0))) row-diffs)
         (unless (veq (v m0 cur-row) (v m1 cur-row))
           (push cur-row row-diffs)))))

;; Find the summation of the product of multiplying two vectors
(defun row-col-product (vec col)
  (and (eq (length vec) (length col))
       (apply #'+ (map 'list #'* vec col))))

;; Are the matrices m0 and m1 equal?
(defun mm-equal (m0 m1)
  (let ((d0 (array-dimensions m0))
        (d1 (array-dimensions m1)))
    (and (equal d0 d1)
         (labels ((compare (row)    ;; Using recursion instead of a loop so it can immediately stop. Check performance against a loop
                    (if (eq row (- (car d0) 1))
                        t
                        (if (not (veq (v m0 row) (v m1 row)))
                            nil
                            (compare (1+ row))))))
           (compare 0)))))

(defun row-do (row fn col)
  (let* ((len (length row))
         (colrow (vgen len col)))
    (map-into row fn row colrow)))

;; Apply function FN to rows starting with START-ROW in matrix M from ROW
(defmacro mmap (m fn row &key (start-row 0))
  `(and (eq (cols ,m) (length ,row))
        (mapcar #'(lambda (x) (map-into x ,fn x ,row)) (nrows ,m))))

;; find the product of matrix M0 and matrix M1
(defmethod mm* ((m0 mat) (m1 mat))
  (let ((new-dimensions (conformable? m0 m1)))
    (do ((i 0 (1+ i))
          (new (mat new-dimensions)))
         ((eq i (rows m0)) new)
      (do ((j 0 (1+ j)))
          ((eq j (cols m1)))
        (setf (aref (arr new) (+ (* i (cols m1)) j)) (reduce #'+ (map 'list #'* (row m0 i) (col m1 j))))))))

;; Find the product of matrix M and integer N
(defmethod nm* ((m mat) n)
  (let ((new (mat (dimensions m) :direct (arr m))))
    (loop for i from 0 below (vec-length new)
          do (setf (aref (arr new) i) (* (aref (arr new) i) n)))
    new))

;; Transpose matrix M
(defmethod transpose ((m mat))
  (do* ((n 0 (1+ n))
        (new (make-array (length (arr m)) :initial-element 99)))
       ((eq n (cols m)) (mat (list (cadr (dimensions m)) (car (dimensions m))) :direct new))
    (let ((c (col m n)))
      (dotimes (j (length c))
        (setf (aref new (+ (* (rows m) n) j)) (aref c j))))))

(defmacro concatenate-vectors (cols)
  `(concatenate 'vector ,@cols))

;; Are the matrix dimensions D0 and D1 CONFORMABLE for matrix multiplication?
(defmethod conformable? ((m0 mat) (m1 mat))
  (let ((d0 (dimensions m0))
        (d1 (dimensions m1)))
  (if (eq d0 d1)
      d0
      (if (eq (car d0) (cadr d1))
          (list (car d0) (cadr d1))
          (error "Matrix dimensions M0: ~A and M1: ~A are not conformable." d0 d1)))))

(defmethod  )

;; Loop through all rows in matrix MAT starting with START-ROW and return a cons of the row with the most whole numbers.
(defun em-whole? (mat &key (start-row 0))
  (do ((rows (car (array-dimensions mat)))
       (cols (cadr (array-dimensions mat)))
       (top nil)
       (counter 0 0)
       (row start-row (incf row)))
      ((eq row rows) top)
    (dotimes (j cols)
      (and (integerp (aref mat row j))
           (setf counter (incf counter))))
    (if top
        (and (> counter (cdr top))
             (setf top (cons row counter)))
        (setf top (cons row counter)))))

; Multiply TARGET-VEC by the vector MULTIPLIER-VEC and add to DST-ROW-N in 2d array MAT
(defun mult-and-add (mat dst-row-n target-vec multiplier-vec)
  (let* ((len (cadr (array-dimensions mat)))
         (dst (make-array len
                          :displaced-to mat
                          :displaced-index-offset (* dst-row-n len))))
    (map-into dst #'+ (v mat dst-row-n) (map-into (make-array len) #'* target-vec multiplier-vec))))

(defmacro doc-do (&body body)
  `(progn (push (format nil "~A~%" (cond ((eq (car ',@body) 'swap-rows) "Swapping rows ? and ?")
                                         ((eq (car ',@body) 'ei-n) "Dividing each column in row to bring leading 1")
                                         ((eq (car ',@body) 'mult-and-add) "Leading 1 found, multiplying lower rows by column"))) steplist)
          ,@body))



(defun backward-solve (matrix &key (row 0) (col 0))
  (let* ((mat (mdup matrix))
         (cols (cadr (array-dimensions matrix))))
    (if (eq row 0)
        mat
        (labels ((column-clear (mat)
                   (do ((cur (- row 1) (1- cur)))
                       ((< cur 0))
                     (mult-and-add mat cur (v mat row) (v-gen (* (aref mat cur col) -1) cols)))))
          (column-clear mat)))
    mat))

(defun forward-solve (matrix &key (row 0) (col 0))
  (let* ((mat (mdup matrix))
         (rows (car (array-dimensions matrix)))
         (cols (cadr (array-dimensions matrix)))
         (steplist '()))
      (if (eq row rows)
          (values mat (reverse steplist))
          (labels ((one-and-clear (mat)
                     (let ((col-one-p (remove row (col-contains-1 mat col) :test #'>)))
                       (if col-one-p
                           (if (eq row (car col-one-p))
                               (do ((nth-row (+ row 1) (incf nth-row)))
                                   ((eq nth-row rows))
                                 (doc-do (mult-and-add mat nth-row (v mat row) (v-gen (* (aref mat nth-row col) -1) cols))))
                               (one-and-clear (doc-do (swap-rows mat (car col-one-p) row))))
                           (let* ((target-row (car (em-whole? (mi-n mat #'/ col :start-row row) :start-row row))))
                             (one-and-clear (doc-do (ei-n mat #'/ target-row col))))))))
            (one-and-clear mat)))
    (values mat (reverse steplist))))

(defun rref (str-list)
  (if (< (length str-list) 2)
      (print "More than one equation required.")
      (let ((atoms (build-matrix (mapcar #'parse-string str-list))))
        (backward-solve (forward-solve atoms) 2 2))))
