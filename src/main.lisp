;; McClim UI for playing around with linear algebra tests

(in-package #:lincl)

(defvar *rref-tests* (make-hash-table :test 'equal))
(defvar *two-unknowns* '("x y = 3"
                       "2x 3y = 1"))
(defvar *three-unknowns* '("x 4y e = 5"
                         "3x 3y 5e = 1"
                         "4x y 3e = 3"))
(defvar *four-unknowns* '("3x 2y -1z 5e = 1"
                        "2x -4y 3z e = 7"
                        "x 8y 2z 3e = 3"
                        "4x 3y 5z -2e = 7"))

(defstruct rref-operation
  matrix steplist diff)

(defparameter *dummy* (make-rref-operation :matrix (make-array '(2 2) :initial-element #\-)))

(define-presentation-type matrix-op ())

(defmacro color-diff-row (options diff-p &body body)
  `(if ,diff-p
       (surrounding-output-with-border ,options ,@body)
       (progn ,@body)))

(defun draw-matrix (mat stream &key (diff-rows nil))
    (surrounding-output-with-border (stream :shape :rounded
                                            :background +beige+
                                            :shadow +grey+
                                            :line-thickness 3)
      (formatting-table (stream :x-spacing 20 :y-spacing 10)
        (dotimes (i (car (array-dimensions mat)))
          (color-diff-row (stream :shape :rounded
                                  :background +light-goldenrod+
                                  :outline-ink +dark-blue+
                                  :line-dashes t)
              (member i diff-rows)
            (formatting-row (stream)
              (dotimes (j (cadr (array-dimensions mat)))
                (formatting-cell (stream)
                  (with-text-style (stream (make-text-style :sans-serif :roman 28))
                    (format stream "~5A" (aref mat i j))))))))))
  (fresh-line stream))

(define-application-frame lincl ()
  ((two-unknowns :initform (solve (build-matrix (parse-equations *two-unknowns*))) :accessor two-unknowns)
   (three-unknowns :initform (solve (build-matrix (parse-equations *three-unknowns*))) :accessor three-unknowns)
   (four-unknowns :initform (solve (build-matrix (parse-equations *four-unknowns*))) :accessor four-unknowns)
   (selected :initform (solve (build-matrix (parse-equations *four-unknowns*))) :accessor selected)
   (counter :initform 1 :accessor counter)
   (dummy-lst :initform (loop for i from 0 below 20 collect *dummy*) :accessor dummy-lst))
  (:menu-bar menubar-command-table)
  (:pointer-documentation t)
  (:panes
   (output :application
           :scroll-bars nil
           ;:incremental-redisplay t
           ;:display-time t
           :display-function #'display)
   (int :interactor))
  (:layouts
   (:default
    (vertically ()
      (7/9 output)
      int)))
  (:default-initargs :width 1200 :height 1600))

(define-lincl-command (com-select-four :name t) ()
  (setf (selected *application-frame*) (four-unknowns *application-frame*))
  (setf (counter *application-frame*) 1)
  (setf (dummy-lst *application-frame*) (loop for i from 0 below 20 collect *dummy*)))

(define-lincl-command (com-select-three :name t) ()
  (setf (selected *application-frame*) (three-unknowns *application-frame*))
  (setf (counter *application-frame*) 1)
  (setf (dummy-lst *application-frame*) (loop for i from 0 below 20 collect *dummy*)))

(define-lincl-command (com-select-two :name t) ()
  (setf (selected *application-frame*) (two-unknowns *application-frame*))
  (setf (counter *application-frame*) 1)
  (setf (dummy-lst *application-frame*) (loop for i from 0 below 20 collect *dummy*)))

(define-lincl-command (com-custom-sequence :name t) ()
  (setf (selected *application-frame*)
        (solve (build-matrix (parse-equations (multiple-value-list (custom-sequence))))))
  (setf (counter *application-frame*) 1)
  (setf (dummy-lst *application-frame*) (loop for i from 0 below 20 collect *dummy*))
  (format nil "Result: ~S~%" (selected *application-frame*)))

(define-lincl-command (com-step :name t) ()
  (with-application-frame (frame)
    (setf (nth (counter *application-frame*) (dummy-lst *application-frame*))
          (nth (counter *application-frame*) (selected *application-frame*)))
    (incf (counter *application-frame*))))

(defun custom-sequence (&key (stream *query-io*))
  (let ((eq1)
        (eq2)
        (eq3))
    (restart-case
        (progn
          (accepting-values (stream)
            (setq eq1 (accept 'string :stream stream :default "2x -3y d = 7" :prompt "Equation 1"))
            (terpri stream)
            (setq eq2 (accept 'string :stream stream :default "x 2y -2d = 3" :prompt "Equation 2"))
            (terpri stream)
            (setq eq3 (accept 'string :stream stream :default "6x -1y 5d = 5" :prompt "Equation 3"))
            (terpri stream))))
    (values eq1 eq2 eq3)))

(make-command-table 'select-equation-command-table
                    :errorp nil
                    :menu '(("Four unknowns" :command com-select-four)
                            ("Three unknowns" :command com-select-three)
                            ("Two unknowns" :command com-select-two)))

(define-command-table menubar-command-table
    :menu (("Select equation" :menu select-equation-command-table)
           ("Finish" :command com-finish-selection)
           ("Custom" :command com-custom-sequence)
           ("Step" :command com-step)))

(defun display (frame pane)
  (let* ((len (length (selected frame)))
         (record
           (updating-output (pane)
             (do* ((elements (dummy-lst frame) (cdr elements))
                   (count 0 (1+ count))
                   (element (first (selected frame)) (first elements)))
                  ((eq count len))
               (updating-output (pane
                                 :unique-id count
                                 :id-test #'eq
                                 :cache-value element
                                 :cache-test #'eql)
                 (formatting-table (pane)
                   (formatting-row (pane)
                     (formatting-cell (pane)
                       (draw-matrix (rref-operation-matrix element) pane :diff-rows (rref-operation-diff element)))
                     (formatting-cell (pane)
                       (with-text-style (pane (make-text-style :sans-serif :roman 24))
                         (format pane "~{~A~}~%" (rref-operation-steplist element))))))
                 (fresh-line pane))))))
    (force-output pane)
    (redisplay record pane)))

(defmacro doc-do (&body body)
  `(progn (push (format nil "~A~%" (cond ((eq (car ',@body) 'swap-rows) "Swapping rows ? and ?")
                                         ((eq (car ',@body) 'ei-n) "Dividing each column in row to bring leading 1")
                                         ((eq (car ',@body) 'mult-and-add) "Leading 1 found, multiplying lower rows by column"))) steplist)
          ,@body))

(defun lincl-ui ()
  (run-frame-top-level (make-application-frame 'lincl)))
