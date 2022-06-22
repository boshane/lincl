;; McClim UI for playing around with linear algebra tests

(in-package #:lincl)

(defun repl ()
  (catch :exit
    (loop
      (clim-debugger:with-debugger ()
        (with-simple-restart (abort "Return to CLIM's top level.")
          (rep))))))

(define-presentation-type result () :inherit-from t)


(defun rep ()
  (multiple-value-bind (command-or-form ptype)
      (accept 'command-or-form :prompt (package-name *package*))
    (when (presentation-subtypep ptype 'command)
      (with-application-frame (frame)
        (return-from rep (execute-frame-command frame command-or-form))))
    (shiftf +++ ++ + - command-or-form)
    (with-room-for-graphics (t :first-quadrant nil)
      (shiftf /// // / (multiple-value-list (eval -))))
    (shiftf *** ** * (first /))
    (format-textual-list / (lambda (object stream)
                             (format stream " ")
                             (present object 'result :stream stream))
                         :separator #\newline)
    (terpri)))

(define-application-frame lincl-ui ()
  ((matrix :initform (mat '(4 4) :random 10) :accessor matrix))
  (:pane :interactor :text-style (make-text-style :fix nil :very-large)))

(define-lincl-ui-command com-inspect-result ((result 'result :gesture :select))
  (clouseau:inspect result))

(define-lincl-ui-command (com-new :name "new")
  ((size '(completion (("4x4" 4x4) ("2x2" 2x2))
           :value-key cadr)
           :prompt "size"
           :default '4x4
           :display-default t))
  (case size
        (4x4 (setf (matrix *application-frame*) (mat '(4 4) :random 10)))
        (2x2 (setf (matrix *application-frame*) (mat '(2 2) :random 10))))
  (print (matrix *application-frame*)))


(defmethod run-frame-top-level ((frame lincl-ui) &rest args)
  (declare (ignore args))
  (let ((*standard-input* (frame-standard-input frame))
        (*standard-output* (frame-standard-output frame))
        (*error-output* (frame-error-output frame))
        (*query-io* (frame-query-io frame))
        (*command-dispatchers* '(#\,)))
    (unwind-protect (repl)
      (frame-exit frame))))

;;(find-application-frame 'lincl-ui :width 800 :height 600)
;;
(defun lincl-ui ()
  (run-frame-top-level (make-application-frame 'lincl-ui :width 800 :height 600)))
