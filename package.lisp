(defpackage #:org.shirakumo.cocoas.cffi
  (:use #:cl)
  (:export))

(defpackage #:org.shirakumo.cocoas
  (:use #:cl)
  (:local-nicknames
   (#:objc #:org.shirakumo.cocoas.cffi))
  (:export))
