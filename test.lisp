(defpackage #:org.shirakumo.cocoas.test
  (:use #:cl)
  (:local-nicknames
   (#:cocoas #:org.shirakumo.cocoas))
  (:export))

(in-package #:org.shirakumo.cocoas.test)

(defun test-roundtrip (type value &optional (value-out value))
  (cffi:with-foreign-objects ((var type))
    (setf (cffi:mem-ref var type) value)
    (assert (equal value-out (cffi:mem-ref var type)))))

(defun test-types ()
  (test-roundtrip 'cocoas:nsstring "Test")
  (test-roundtrip 'cocoas:cfstring "Test")
  (test-roundtrip 'cocoas:cfnumber 12.3d0)
  (test-roundtrip '(cocoas:cfnumber :int) #xDEADBEEF)
  (test-roundtrip '(cocoas:cfset :string) '("A" "b"))
  (test-roundtrip '(cocoas:cfset :string) #("A" "b") '("A" "b"))
  (test-roundtrip '(cocoas:cfarray :string) #("A" "b"))
  (test-roundtrip '(cocoas:cfarray :string) '("A" "b") #("A" "b"))
  (test-roundtrip '(cocoas:cfdictionary :string (cocoas:cfnumber :int))
                  (let ((tbl (make-hash-table :test 'equal)))
                    (setf (gethash "K" tbl) 1)
                    (setf (gethash "0" tbl) 2)
                    tbl)))
