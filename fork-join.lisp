;;;
;;; Fork-join style concurrency.
;;;
;;; This package implements a simple fork-join concurrency on top of
;;; bordeaux-threads.
;;;
;;; The simplest usage is:
;;;
;;; (FORK
;;;   (FOO)
;;;   (BAR X)
;;;   (BAZ))
;;;
;;; This computes FOO, BAR and BAZ in 3 separate threads and waits until all of
;;; them complete.
;;;
;;; A group of concurrent computations that can be manipulated as a whole is
;;; called "a fork". A particular computation in the fork is called "a tine".
;;;
;;; (tine, noun: a prong or sharp point, such as that on a fork or antler.)
;;;
;;; For a fuller control than in the example above, create a fork explicitly,
;;; then add tines to it. When a tine is added to a fork, the tine function
;;; starts concurrent execution immediately:
;;;
;;; (LET ((FORK (MAKE-FORK T))) ; T means 'propagate conditions to the waiter'.
;;;   (FORK-WITH FORK
;;;     (FOO)
;;;     (BAR X)
;;;     (BAZ))))
;;;   (COMPUTE-SOMETHING)       ; Compute concurrently with the forked functions.
;;;   (WAIT GROUP))             ; Wait for FOO, BAR and BAZ completion.
;;;
;;; Tines can be created and added explicitly (again, this immediately starts
;;; tine computation in a concurrent thread):
;;;
;;; (ADD-TINE FORK (MAKE-TINE #'FUNCTION))
;;;
;;; Instead of a synchronous join, call-backs can be specified for a fork. NEXT
;;; call-back is called with 2 parameters (the fork and the tine), when a forked
;;; thread completes in the fork and DONE call-back is called with the fork as
;;; the parameter when all tines complete:
;;;
;;; (FORK-WITH (MAKE-FORK T :NEXT #'NEXT :DONE #'DONE)
;;;   (FOO)
;;;   (BAR X)
;;;   (BAZ)))))
;;;
;;; (KILL FORK) terminates all forked threads in FORK. If a thread in FORK is
;;; waiting in (WAIT SUBFORK), the SUBFORK is terminated recursively. KILL
;;; uses BORDEAUX-THREADS:INTERRUPT-THREAD (c.f.) so all the appropriate caveats
;;; apply.
;;;
;;; Timeout:
;;;
;;; (LET ((FORK (MAKE-FORK T)))
;;;   (FORK-TIMEOUT FORK 3600.0) ; After 1 hour, terminate the fork.
;;;   (FORK-WITH FORK            ; Start FOO, BAR and BAZ in FORK.
;;;     (FOO)
;;;     (BAR X)
;;;     (BAZ)))
;;;
;;; If a fork is created with PROP flag set to true (default for FORK
;;; macro), any condition signalled by a forked function within the fork is
;;; propagated to the fork. Propagated conditions are picked by WAIT and
;;; re-signalled in the waiting thread.
;;;
;;; Implementation is very simple: a separate thread is used for each forked
;;; function.

(defpackage :fork-join
  (:documentation "Fork-join concurrency")
  (:nicknames :fj)
  (:use #:common-lisp #:bordeaux-threads)
  (:export #:tine
           #:fork
           #:make-fork
           #:make-tine
           #:add-tine
           #:kill
           #:wait
           #:fork-with
           #:fork-timeout
           #:active-tines))

(in-package :fork-join)

(defclass tine ()             ; A function computed as part of a fork.
  ((fork   :initarg :fork)    ; Fork to which this function belongs.
   (func   :initarg :func)    ; The function executed in this tine.
   (thread :initform nil)))   ; Underlying thread.

(defclass fork () ; A group of tines.
   ; This lock protects all fields and serialises forked thread creation and
   ; termination.
  ((lock  :initform (make-lock))
   ; Condition variable used to wait for thread termination.
   (wait  :initform (make-condition-variable))
   (tines :initform ()) ; List of forked functions.
   ; If true, conditions signalled by the tines are propagated to the fork.
   (prop  :initarg :prop)
   ; List of propagated conditions, not yet re-signalled.
   (sigs  :initform ())
   ; Optional call-back invoked on each tine termination.
   (next  :initform nil :initarg :next)
   ; Optional call-back invoked on the fork termination.
   (done  :initform nil :initarg :done)))

(defun make-fork (prop &key next done)
  (make-instance 'fork :prop prop :next next :done done))

(defun make-tine (func)
  (make-instance 'tine :func func))

(defmacro with-fork-lock (fork &body body)
  `(with-slots (lock) ,fork (with-lock-held (lock) ,@body)))

(defun done-tine (fork tine sig)
  (with-fork-lock fork
    (with-slots (wait tines prop sigs next done) fork
      (setf tines (remove tine tines :test #'eq))
      (if sig (if prop (push sig sigs) ; Propagate ...
                  (signal sig)))       ; or re-signal immediately.
      (when next (funcall next fork tine))
      (unless tines (when done (funcall done fork)))
      ;; XXX There is no BORDEAUX-THREADS:CONDITION-BROADCAST?
      (if (or sigs (not tines)) (condition-notify wait)))))

(define-condition tine-exit (condition) ())

(defun tine-function (fork tine) ; Startup function of a forked thread.
  #'(lambda ()
      (let (sig)
        (unwind-protect (handler-case (funcall (slot-value tine 'func))
                          (tine-exit () nil) ; Ignore exit.
                          (condition (s) (setf sig s)))
          (done-tine fork tine sig)))))

(defun add-tine (fork tine) ; Add a tine to the fork and start the thread.
  (with-fork-lock fork
    (with-slots (tines) fork
      (push tine tines)
      (setf (slot-value tine 'fork) fork
            (slot-value tine 'thread) (make-thread (tine-function fork tine))))))

(defun handle (fork) ; Handle propagated conditions.
  (with-slots (prop sigs) fork
    (and prop sigs (progn (signal (pop sigs)) (handle fork)))))

(defun kill (fork)
  (with-fork-lock fork
    (with-slots (tines) fork
      (let (self) ; Handle the case when current thread is part of the fork.
        (loop for tine in tines do
             (let ((thread (slot-value tine 'thread)))
               (if (eq thread (current-thread))
                   (setf self t)
                   (interrupt-thread thread #'signal 'tine-exit))))
        (if self (signal 'tine-exit))))))

(defun wait (fork) ; Wait until all forked threads terminate.
  (handler-case
      (progn
        (with-fork-lock fork
          (with-slots (lock wait tines sigs) fork
            (loop while (or tines sigs) do
              (progn (handle fork)
                     (condition-wait wait lock))))))
    (tine-exit (e) (kill fork) (signal e)))) ; Parent is killed, infanticide.

(defmacro fork-with (fork &body body) ; Add tines to an existing fork.
  `(progn ,@(loop for form in body collect
            `(add-tine ,fork (make-tine #'(lambda () ,form))))))

(defmacro fork (&body body) ; Fork and wait.
  (let* ((f (gensym)))
    `(let ((,f (make-fork t)))
       ,@(loop for form in body collect
            `(add-tine ,f (make-tine #'(lambda () ,form))))
       (wait ,f))))

;;
;; Additional functionality on top of basic fork-join.
;;

(defun fork-timeout (fork timeout)
  (make-thread #'(lambda () (sleep timeout) (kill fork))))

(defun active-tines (fork)
  (with-fork-lock fork (with-slots (tines) fork (length tines))))
