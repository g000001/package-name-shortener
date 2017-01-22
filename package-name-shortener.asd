;;;; package-name-shortener.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :package-name-shortener
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "package-name-shortener")
               #+sbcl (:file "sbcl")
               #+ccl (:file "ccl")
               #+lispworks (:file "lispworks")
               (:file "test")))


(defmethod perform ((o test-op) (c (eql (find-system :package-name-shortener))))
  (load-system :package-name-shortener)
  (or (flet (($ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let ((result (funcall ($ :fiveam :run) ($ :package-name-shortener.internal :package-name-shortener))))
          (funcall ($ :fiveam :explain!) result)
          (funcall ($ :fiveam :results-status) result)))
      (error "test-op failed") ))


;;; *EOF*
