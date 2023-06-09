;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(defpackage :fork-join/tests
  (:documentation "Fork-join unit tests")
  (:nicknames :fj/t)
  (:use #:parachute #:fork-join #:common-lisp #:bordeaux-threads))

(in-package #:fork-join/tests)

(define-test all)

(define-test fork/base
  :parent all)

(define-test fork/simple
  :parent fork/base
  (true (progn (fork) t))
  (true (progn (fork 0) t))
  (true (progn (fork 0 1) t))
  (true (progn (fork 0 1 2) t)))

(define-test fork/time
  :parent fork/base
  :depends-on (fork/simple)
  :time-limit 1.2
  (true (progn (fork
                 (sleep 1)
                 (sleep 1)
                 (sleep 1))
               t)))

(define-test fork/let
  :parent fork/base
  :depends-on (fork/time)
  (is equal '(1 2 nil)
      (fork-let ((x 1) (y 2) z)
        (list x y z))))

(define-test fork/order
  :parent fork/base
  :depends-on (fork/let)
  (is equal (let (out) (fork
                         (progn (sleep 1) (push 1 out))
                         (progn (sleep 2) (push 2 out))
                         (progn (sleep 3) (push 3 out)))
              out)
      (reverse '(1 2 3))))
  
(define-test fork/mutex
  :parent fork/base
  :depends-on (fork/order)
  (is equal (sort (let (out
                        (guard (bt:make-lock)))
                    (fork
                      (bt:with-lock-held (guard) (push 1 out))
                      (bt:with-lock-held (guard) (push 2 out))
                      (bt:with-lock-held (guard) (push 3 out)))
                    out)
                  #'<)
      '(1 2 3)))

(define-test fork/tine
  :parent fork/base
  :depends-on (fork/mutex)
  (is equal (sort (let (out
                        (guard (bt:make-lock))
                        (fork (make-fork t)))
                    (loop for i in '(1 2 3 4 5 6 7 8 9) do
                      (let ((i i))
                        (add-tine fork
                                     (make-tine
                                      #'(lambda ()
                                          (progn
                                            (sleep (random 2.0))
                                            (bt:with-lock-held (guard)
                                              (push i out))))))))
                    (wait fork)
                    out)
                  #'<)
      '(1 2 3 4 5 6 7 8 9)))

(define-test fork/next-done
  :parent fork/base
  :depends-on (fork/tine)
  (is = (let* ((guard (bt:make-lock))
               (c 0)
               (cb #'(lambda (&rest args)
                       (declare (ignore args))
                       (bt:with-lock-held (guard) (incf c))))
               (fork (make-fork t :next cb :done cb)))
          (fork-with fork 1 2 3 4)
          (wait fork)
          c)
      5))

(define-test fork/cond
  :parent all
  :depends-on (fork/base))

(define-test fork/propagate
  :parent fork/cond
  (true (handler-case (fork
                        0
                        (signal 'error)
                        2)
          (error () t))))

(define-test fork/propagate-through
  :parent fork/cond
  (true (handler-case (fork
                        0
                        (fork
                          1.1
                          (signal 'error)
                          1.2)
                        2)
          (error () t))))

(define-test fork/no-propagate
  :parent fork/cond
  (is equal t (let ((fork (make-fork nil)))
                (fork-with fork
                  (sleep 0.1)
                  (signal 'error))
                t)))

(define-test fork/terminate
  :parent all
  :depends-on (fork/cond))

(define-test fork/kill
  :parent fork/terminate
  :time-limit 0.5
  (true (let ((fork (make-fork t)))
          (fork-with fork
            (loop (sleep 1))
            (kill fork))
          t)))

(define-test fork/kill-up
  :parent fork/terminate
  :time-limit 0.5
  (true (let ((fork (make-fork t)))
          (fork-with fork
            (loop (sleep 1))
            (let ((subfork (make-fork t)))
              (fork-with subfork
                (loop (sleep 1))
                (kill subfork))))
          t)))

(define-test fork/kill-down
  :parent fork/terminate
  :time-limit 0.5
  (true (let ((fork (make-fork t)))
          (fork-with fork
            (loop (sleep 1))
            (let ((subfork (make-fork t)))
              (fork-with subfork
                (loop (sleep 1))
                (kill fork))))
          t)))

(define-test fork/timeout
  :parent fork/terminate
  :time-limit 1.0
  (true (let ((fork (make-fork t)))
          (fork-timeout fork 0.5)
          (fork-with fork
            (loop (sleep 1)))
          t)))

(define-test fork/container
  :parent all
  :depends-on (fork/terminate))

(define-test fork/map
  :parent fork/container
  (is equalp #(1.1 3.2 5.3 7.4)
      (progn (let* ((fork (make-fork t))
                    (out (fork-map fork '(1.1 2.2 3.3 4.4)
                                   #'(lambda (idx val) (+ idx val)))))
               (wait fork)
               out))))

(define-test fork/serialise
  :parent fork/container
  (is = 3 (progn (let ((*factory* (make-factory 1))
                       (x 0))
                   (fork
                     (when (= x 0) (sleep 1) (setf x 1))
                     (when (= x 1) (sleep 1) (setf x 2))
                     (when (= x 2) (sleep 1) (setf x 3)))
                   x))))
  
(test 'all)
