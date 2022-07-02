;; McClim UI for playing around with linear algebra tests
;;
;; TODO: Use av-test to find examples for menu selection
;;
(in-package #:lincl)

(defclass matrices-view (gadget-view) ())

(define-presentation-type base-row ())
(define-presentation-type modifier-row ())

(defclass operation-state ()
  ((m0 :initarg :m0 :accessor m0)
   (m1 :initarg :m1 :accessor m1)
   (result :initarg :result :accessor result))
  (:default-initargs :m0 (mat '(4 4) :random 10)
                     :m1 (mat '(4 4) :random 10)
                     :result (mat '(4 4) :random 10)))

(define-application-frame lincl-ui (standard-application-frame operation-state)
  ((base-size-selection :initform :base-size :accessor base-size-selection)
   (modifier-size-selection :initform :modifier-size :accessor modifier-size-selection))
  (:panes
   (base :application
         :display-function 'display-base
         :display-time :command-loop
         :text-style (make-text-style :fix nil :huge)
         :default-view (make-instance 'matrices-view))
   (modifier :application
             :display-function 'display-modifier
             :display-time :command-loop
             :text-style (make-text-style :fix nil :huge)
             :default-view (make-instance 'matrices-view))
   (base-options
    (vertically ()
      (horizontally ()
        (labelling (:label "Rows")
          (with-radio-box (:orientation :vertical
                           :value-changed-callback
                           #'(lambda (this-gadget selected-gadget)
                               (declare (ignore this-gadget))
                               (set-dimensions (digit-char-p (char (gadget-label selected-gadget) 0))
                                               'row
                                               :base (m0 *application-frame*))))
            (radio-box-current-selection "4") "2" "3" "4" "5" "6"))
        (labelling (:label "Columns")
          (with-radio-box (:orientation :vertical
                           :value-changed-callback
                           #'(lambda (this-gadget selected-gadget)
                               (declare (ignore this-gadget))
                               (set-dimensions (digit-char-p (char (gadget-label selected-gadget) 0))
                                               'row
                                               :base (m0 *application-frame*))))
            (radio-box-current-selection "4") "2" "3" "4" "5" "6")))))
   (modifier-options
    (vertically ()
      (horizontally ()
        (labelling (:label "Rows")
          (with-radio-box (:orientation :vertical
                           :value-changed-callback
                           #'(lambda (this-gadget selected-gadget)
                               (declare (ignore this-gadget))
                               (set-dimensions (digit-char-p (char (gadget-label selected-gadget) 0))
                                               'row
                                               :modifier (m1 *application-frame*))))
            (radio-box-current-selection "4") "2" "3" "4" "5" "6"))
        (labelling (:label "Columns")
          (with-radio-box (:orientation :vertical
                           :value-changed-callback
                           #'(lambda (this-gadget selected-gadget)
                               (declare (ignore this-gadget))
                               (set-dimensions (digit-char-p (char (gadget-label selected-gadget) 0))
                                               'row
                                               :modifier (m1 *application-frame*))))
            (radio-box-current-selection "4") "2" "3" "4" "5" "6")))))
   (result :application
           :display-function 'display-result
           :display-time :command-loop
           :text-style (make-text-style :fix nil :huge)
           :default-view (make-instance 'matrices-view))
   (interactor :interactor))
  (:layouts
   (default
    (horizontally ()
      (vertically ()
        (horizontally ()
          (labelling (:label "Input Matrix") base)
          (1/7 base-options))
        (horizontally ()
          (labelling (:label "Modifier Matrix") modifier)
          (1/7 modifier-options))
        (horizontally ()
          (labelling (:label "Result Matrix") result)
          (vertically ()
            (make-pane 'push-button :label "Product" :activate-callback #'%execute)
            (make-pane 'push-button :label "Divide" :activate-callback #'%execute)
            (make-pane 'push-button :label "Add" :activate-callback #'%execute)
            (make-pane 'push-button :label "Subtract" :activate-callback #'%execute)
            (make-pane 'push-button :label "Foo" :activate-callback #'%execute)
            (make-pane 'push-button :label "Bar" :activate-callback #'%execute))))
      (labelling (:label "Interactor") interactor))))
  (:pointer-documentation t))


(define-presentation-method present
    ((n integer) (type base-row) stream (view matrices-view) &key)
  (let ((row (row (m0 *application-frame*) n)))
    (formatting-row (stream)
      (dotimes (j (length row))
        (formatting-cell (stream :min-width 20 :align-x :right)
          (format stream "~A" (aref row j)))))))

(define-presentation-method present
    ((n integer) (type modifier-row) stream (view matrices-view) &key)
  (let ((row (row (m1 *application-frame*) n)))
    (formatting-row (stream)
      (dotimes (j (length row))
        (formatting-cell (stream :min-width 20 :align-x :right)
          (format stream "~A" (aref row j)))))))

(define-lincl-ui-command (com-select-modifier-row :menu t :name t)
    ((object 'modifier-row :gesture (:select :tester test-row)))
  (accept-new-row (m1 *application-frame*) object)
  (finish-output *standard-output*))

(define-lincl-ui-command (com-select-base-row :menu t :name t)
    ((object 'base-row :gesture (:select :tester test-row)))
  (accept-new-row (m0 *application-frame*) object)
  (finish-output *standard-output*))

(define-lincl-ui-command (com-set-base-matrix :name t) ()
  (setf (m0 *application-frame*)
        (mat (base-size-selection *application-frame*) :random 10)))

(define-lincl-ui-command (com-set-modifier-matrix :name t) ()
  (setf (m1 *application-frame*)
        (mat (modifier-size-selection *application-frame*) :random 10)))

(define-lincl-ui-command (com-product :name t) ()
  (setf (result *application-frame*)
        (mm* (m0 *application-frame*) (m1 *application-frame*))))

(define-lincl-ui-command (com-divide :name t) ()
  (setf (result *application-frame*)
        (mm* (m0 *application-frame*) (m1 *application-frame*))))

(define-lincl-ui-command (com-add :name t) ()
  (setf (result *application-frame*)
        (mm+ (m0 *application-frame*) (m1 *application-frame*))))

(define-lincl-ui-command (com-subtract :name t) ()
  (setf (result *application-frame*)
        (mm- (m0 *application-frame*) (m1 *application-frame*))))

(define-lincl-ui-command (com-product :name t) ()
  (setf (result *application-frame*)
        (mm* (m0 *application-frame*) (m1 *application-frame*))))

(defun %execute (this-gadget)
  (cond ((string= (gadget-label this-gadget) "Product")
         (execute-frame-command *application-frame* '(com-product)))
        ((string= (gadget-label this-gadget) "Divide")
         (execute-frame-command *application-frame* '(com-divide)))
        ((string= (gadget-label this-gadget) "Add")
         (execute-frame-command *application-frame* '(com-add)))
        ((string= (gadget-label this-gadget) "Subtract")
         (execute-frame-command *application-frame* '(com-subtract)))
        (t nil)))

;; This is clunky but I could not figure out a clean way to get the gadget values along with the target matrix
;; without a bunch of code repitition.
(defun set-dimensions (num row-or-col &key (base nil) (modifier nil))
  (cond (base
         (with-slots (dimensions base-size-selection)
             *application-frame*
           (let ((new-dimensions
                   (if (eq row-or-col 'row)
                       (list num (cadr (dimensions (m0 *application-frame*))))
                       (list (car (dimensions (m0 *application-frame*))) num))))
             (setf base-size-selection new-dimensions)
             (execute-frame-command *application-frame* '(com-set-base-matrix)))))
        (modifier
         (with-slots (m1 modifier-size-selection)
             *application-frame*
           (let ((new-dimensions
                   (if (eq row-or-col 'col)
                       (list num (cadr (dimensions (m1 *application-frame*))))
                       (list (car (dimensions (m1 *application-frame*))) num))))
             (setf modifier-size-selection new-dimensions)
             (execute-frame-command *application-frame* '(com-set-modifier-matrix)))))))

(defun %new-matrix-type (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (let* ((selection (gadget-label selected-gadget))
         (dim (digit-char-p (char selection 0))))
    (with-slots (size-selection)
        *application-frame*
      (setf (size-selection *application-frame*) (list dim dim))
      (execute-frame-command *application-frame* '(com-set-matrix)))))

(defmethod accept-new-row ((mat mat) row)
  (let ((new-values (accept-row)))
    (and (eq (length new-values) (cols mat))
         (dotimes (n (length new-values))
           (setf (mref mat row n) (nth n new-values))))))

(defmethod display-base ((frame lincl-ui) pane)
  (with-slots (cols rows)
      (m0 *application-frame*)
    (setf (stream-cursor-position pane) (values 40 40))
    (with-text-family (pane :fix)
      (surrounding-output-with-border (pane :line-thickness 2
                                            :ink +red+)
        (formatting-table (pane :x-spacing (max 8 (- 60 (* 8 cols)))
                                :y-spacing (max 8 (- 60 (* 8 rows))))
                                (dotimes (i (rows (m0 *application-frame*)))
                                  (present i 'base-row :stream pane :single-box t))))))
  (fresh-line pane))

(defmethod display-modifier ((frame lincl-ui) pane)
    (setf (stream-cursor-position pane) (values 40 40))
    (with-text-family (pane :fix)
      (surrounding-output-with-border (pane :line-thickness 2
                                            :ink +red+)
          (with-slots (cols rows)
              (m1 *application-frame*)
        (formatting-table (pane :x-spacing (max 8 (- 60 (* 8 cols)))
                                :y-spacing (max 8 (- 60 (* 8 rows))))
                                (dotimes (i (rows (m1 *application-frame*)))
                                  (present i 'modifier-row :stream pane :single-box t)))))))

(defun display-result (frame pane)
  (declare (ignore frame))
  (with-slots (cols rows)
      (result *application-frame*)
    (setf (stream-cursor-position pane) (values 40 40))
    (with-text-family (pane :fix)
      (surrounding-output-with-border (pane :line-thickness 2
                                            :ink +green+)
        (formatting-table (pane :x-spacing (max 8 (- 60 (* 8 cols)))
                                :y-spacing (max 8 (- 60 (* 8 rows))))
          (dolist (row (mat-rows (result *application-frame*)))
            (formatting-row (pane)
              (dotimes (j (length row))
                (formatting-cell (pane :min-width 20 :align-x :right)
                  (format pane "~A" (aref row j)))))))))))


(defun string-values (str)
  (let ((len (length str))
        (vals '()))
    (labels ((token (str pos)
               (multiple-value-bind (tok end)
                   (read-from-string str :eof-error-p t :start pos)
                 (if (not (numberp tok))
                     (error "Please provide a list of comma delineated numbers.")
                     (push tok vals))
                 (unless (eq end len)
                   (token str end)))))
      (token str 0))
    (reverse vals)))

(defun accept-row (&key (stream *query-io*))
  (let (new-values)
    (accepting-values (stream :resynchronize-every-pass t)
      (fresh-line stream)
      (setq new-values (accept 'string :default () :stream stream)))
    (string-values new-values)))

(defun test-row (dst-row &rest args)
  (declare (ignore args))
  t)

(defun matrix-handler ()
  (let ((m (mat '(2 2) :direct #(.7 .3 .6 .4))))
    (matrix-lambda
     (:print () (print m))
     (:eq () (mm= m m))
     (:mult (foo) (mm* m (mat '(2 1) :direct foo))))))

(defun lincl-ui ()
  (run-frame-top-level (make-application-frame 'lincl-ui :width 1500 :height 1400)))
