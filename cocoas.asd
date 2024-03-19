(asdf:defsystem cocoas
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A toolkit library to help deal with CoreFoundation, Cocoa, and objc"
  :homepage "https://shinmera.github.io/cocoas/"
  :bug-tracker "https://github.com/shinmera/cocoas/issues"
  :source-control (:git "https://github.com/shinmera/cocoas.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "types")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:float-features
               :trivial-main-thread))
