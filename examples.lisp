;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(defpackage :fork-join/examples
  (:documentation "Fork-join examples")
  (:nicknames :fj/e)
  (:use #:fork-join #:common-lisp)
  (:export #:merge-sort
	   #:merge-sort-fork
	   #:quick-sort
	   #:quick-sort-fork
	   #:sort-time))

(in-package #:fork-join/examples)

(defun split-at (lst index)
  (cons (subseq lst 0 index) (subseq lst index)))

(defun merge-sort (list pred)
  (let ((len (length list)))
    (cond ((<= len 1) list)
          (t (let* ((mid (floor len 2))
                    (split (split-at list mid)))
               (merge 'list
		      (merge-sort (car split) pred)
                      (merge-sort (cdr split) pred) pred))))))

(defun merge-sort-fork (list pred)
  (let ((len (length list)))
    (cond ((<= len 1) list)
          (t (let* ((split (split-at list (floor len 2))))
	       (fj:fork-let ((left  (merge-sort-fork (car split) pred))
			     (right (merge-sort-fork (cdr split) pred)))
		 (merge 'list left right pred)))))))

(defun quick-sort (list pred)
  (if (null list) list
      (let* ((pivot (car list))
             (rest  (cdr list))
             (lt (remove-if #'(lambda (x) (not (funcall pred x pivot))) rest))
             (gt (remove-if #'(lambda (x)      (funcall pred x pivot))  rest)))
	(append (quick-sort lt pred) (list pivot) (quick-sort gt pred)))))

(defun quick-sort-fork (list pred)
  (if (null list) list
      (let ((pivot (car list))
            (rest  (cdr list)))
	(fj:fork-let
          ((lt (quick-sort-fork	(remove-if #'(lambda (x) (not (funcall pred x pivot))) rest) pred))
           (gt (quick-sort-fork	(remove-if #'(lambda (x)      (funcall pred x pivot))  rest) pred)))
	(append lt (list pivot) gt)))))

(defmacro eval-secs (&body body)
  `(- (/ (- (get-internal-real-time)
	    (progn ,@body (get-internal-real-time)))
	 internal-time-units-per-second)))

(defun average (list)
  (/ (reduce #'+ list) (length list)))

(defmacro eval-rep (r &body body)
  `(let* ((l (loop repeat ,r collect (eval-secs ,@body)))
	  (avg (average l))
	  (min (reduce #'min l))
	  (max (reduce #'max l))
	  (dev (sqrt (average (mapcar #'(lambda (x) (expt (- x avg) 2)) l)))))
     (list avg min max dev)))

(defun sort-time (fseq fpar limit r thread-max)
  (let ((fj:*factory* (fj:make-factory thread-max)))
    (let ((h '("avg" "min" "max" "dev")))
      (format t "~10a: ~{~10<~a~>~} ~{~10<~a~>~}~%"
	      "length" '("seq" "" "" "") '("par" "" "" ""))
      (format t "~10<~a~>  ~{~10<~a~>~} ~{~10<~a~>~}~%" "" h h))
    (loop for i from 0 below limit do
      (let* ((n (expt 2 i))
	     (list (loop repeat n collect (random n)))
	     (seq (eval-rep r (funcall fseq list #'<)))
	     (par (eval-rep r (funcall fpar list #'<))))
	(format t "~10a: ~{~10,3f~} ~{~10,3f~}~%" n seq par)))))

