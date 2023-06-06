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
