;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(asdf:defsystem "fork-join"
  :description "Fork-join style concurrency."
  :author "Nikita Danilov <danilov@gmail.com>"
  :maintainer "Nikita Danilov <danilov@gmail.com>"
  :homepage "https://github.com/nikitadanilov/forkjoin"
  :bug-tracker "https://github.com/nikitadanilov/forkjoin/issues"
  :source-control (:git "https://github.com/nikitadanilov/forkjoin.git")
  :license "GPL2"
  :version "0.0.1"
  :serial t
  :depends-on (#:bordeaux-threads)
  :components ((:file "fork-join")))

(asdf:defsystem "fork-join/tests"
  :description "Unit tests for fork-join."
  :author "Nikita Danilov <danilov@gmail.com>"
  :maintainer "Nikita Danilov <danilov@gmail.com>"
  :homepage "https://github.com/nikitadanilov/forkjoin"
  :bug-tracker "https://github.com/nikitadanilov/forkjoin/issues"
  :source-control (:git "https://github.com/nikitadanilov/forkjoin.git")
  :license "GPL2"
  :version "0.0.1"
  :serial t
  :depends-on (#:fork-join #:parachute)
  :components ((:file "tests")))

(asdf:defsystem "fork-join/examples"
  :description "Examples for fork-join."
  :author "Nikita Danilov <danilov@gmail.com>"
  :maintainer "Nikita Danilov <danilov@gmail.com>"
  :homepage "https://github.com/nikitadanilov/forkjoin"
  :bug-tracker "https://github.com/nikitadanilov/forkjoin/issues"
  :source-control (:git "https://github.com/nikitadanilov/forkjoin.git")
  :license "GPL2"
  :version "0.0.1"
  :serial t
  :depends-on (#:fork-join)
  :components ((:file "examples")))
