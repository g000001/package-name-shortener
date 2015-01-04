(Cl:In-Package :ccl)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (Defun redefine-kernel-function-continue (c)
    (Let ((cstring (Simple-Condition-Format-Control c)))
      (If  (String= "The function ~S is predefined in Clozure CL."
                    cstring)
           (Continue c)
           (Cerror cstring c)))))


(Handler-Bind ((Simple-Error #'redefine-kernel-function-continue))
  (defun write-a-symbol (symbol stream)
    (declare (type symbol symbol) (type stream stream))
    (let ((case (get-*print-case*))
          (name (symbol-name symbol))
          (package (symbol-package symbol)))
      (declare (type simple-string name))
      (when (or *print-readably* *print-escape*)
        (cond ((keywordp symbol)
               (stream-write-char stream #\:))
              ((null package)
               (when (or *print-readably* *print-gensym*)
                 (stream-write-char stream #\#)
                 (stream-write-char stream #\:)))
              (t
               (multiple-value-bind (s flag)
                                    (find-symbol name *package*)
                 (unless (and flag (eq s symbol))
                   (multiple-value-setq (s flag)
                                        (find-symbol name package))
                   (unless (and flag (eq s symbol))
                     (%write-string "#|symbol not found in home package!!|#"
                                    stream))
                   ;; (write-pname (package-name package) case stream)
                   (write-pname
                    (PACKAGE-NAME-SHORTENER::SHORTEST-PACKAGE-NAME
                     (package-name package))
                    case stream)
                   (stream-write-char stream #\:)
                   (unless (eq flag ':external)
                     (stream-write-char stream #\:)))))))
      (write-pname name case stream))))


;;; *EOF*
