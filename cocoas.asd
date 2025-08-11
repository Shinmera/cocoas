(asdf:defsystem cocoas
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A toolkit library to help deal with CoreFoundation, Cocoa, and objc"
  :homepage "https://shinmera.com/docs/cocoas/"
  :bug-tracker "https://shinmera.com/project/cocoas/issues"
  :source-control (:git "https://shinmera.com/project/cocoas.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "types")
               (:file "classes")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:cffi
               :float-features
               :trivial-main-thread))
