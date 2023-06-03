;;;
;;; Fork-join style concurrency.
;;;
;;; This package implements a simple fork-join concurrency on top of
;;; bordeaux-threads.
;;;
;;; The simplest usage example is:
;;;
;;; (FJ:FORK
;;;   (FOO)
;;;   (BAR X)
;;;   (BAZ))
;;;
;;; This computes FOO, BAR and BAZ in 3 separate threads and waits until all of them complete.
;;;
;;; Wait can be separate:
;;; 
;;; (LET ((GROUP (FJ:FORK-LAUNCH T ; T means 'propagate conditions to the waiter'.
;;;                (FOO)
;;;                (BAR X)
;;;                (BAZ))))
;;;   (COMPUTE-SOMETHING)          ; Compute concurrently with the forked functions.
;;;   (FJ:WAIT GROUP))             ; Wait for FOO, BAR and BAZ completion.
;;;
;;; Conditions, errors, signals.
;;;
;;; If a fork-group is created with PROP flag set to true (default for FORK
;;; macro), any condition signalled by a forked function within the group is
;;; propagated to the group. Propagated conditions are picked by WAIT and
;;; re-signalled in the waiting thread.
;;;
;;; Implementation is very simple: a separate thread is used for each forked
;;; function.

;;(load "/Users/517711/quicklisp/setup.lisp")
(ql:quickload :bordeaux-threads)

(defpackage :fork-join
  (:nicknames :fj)
  (:use common-lisp bordeaux-threads)
  (:export make-group fork-launch wait fork))

(in-package :fork-join)

(defclass fork ()             ; Class of forked functions.
  ((group  :initarg :group)   ; Fork-group to which this function belongs.
   (thread :initform nil)))   ; Underlying thread.

(defclass group ()
   ; This lock protects all fields and serialises forked thread creation and
   ; termination.
  ((lock  :initform (make-lock))
   ; Condition variable used to wait for thread termination.
   (wait  :initform (make-condition-variable))
   (forks :initform ())    ; List of forked functions.
   ; If true, conditions signalled by the forked threads and propagated to the group.
   (prop  :initarg  :prop)
   ; List of propagated conditions, not yet re-signalled.
   (sigs  :initform ())
   ; Optional call-back invoked on each forked thread termination.
   (next  :initform (constantly nil))
   ; Optional call-back invoked on the group termination.
   (done  :initform (constantly nil))))

(defun fork-name (group function) ; Underlying thread name.
  (format nil "fork-~a-~a" group function))

(defun done-fork (group fork sig)
  (with-slots (lock wait forks prop sigs next done) group
    (with-lock-held (lock)
      (setf forks (remove fork forks :test #'eq))
      (if sig (if prop (push sig sigs) ; Propagate ...
		  (signal sig)))       ; or re-signal immediately.
      (funcall next group fork)
      (unless forks (funcall done group))
      (if (or sigs (not forks)) (condition-notify wait)))))

(defun fork-function (group fork func) ; Startup function of a forked thread.
  #'(lambda ()
      (let (sig)
	(unwind-protect (handler-case (funcall func)
			  (condition (s) (setf sig s)))
	  (done-fork group fork sig)))))

(defun make-fork (group function) ; Create and add a forked thread to the group
  (with-slots (lock forks) group
    (with-lock-held (lock)
      (let ((f (make-instance 'fork :group group)))
	(setf (slot-value f 'thread)
	      (make-thread (fork-function group f function)
			   :name (fork-name group function)))
	(push f forks)))))

(defun make-group (prop)
  (make-instance 'group :prop prop))

(defun handle (group) ; Handle propagated conditions.
  (with-slots (prop sigs) group
    (and prop sigs (not (signal (pop sigs))) (handle group))))
  
(defun wait (group) ; Wait until all forked threads terminate.
  (with-slots (lock wait forks sigs) group
    (with-lock-held (lock)
      (loop while (or forks sigs) do
	(progn
	  (handle group)
	  (condition-wait wait lock))))))

; Create and return a group of forked functions.
(defmacro fork-launch (prop &body body) 
  (let ((g (gensym)))
    `(let ((,g (make-group ,prop)))
       ,@(loop for f in body collect `(make-fork ,g #'(lambda () ,f)))
       ,g)))

(defmacro fork (&body body) ; Fork and wait.
  `(wait (fork-launch t ,@body)))


