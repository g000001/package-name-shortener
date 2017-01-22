;;;; package-name-shortener.lisp -*- Mode: Lisp;-*- 

(Cl:In-Package :package-name-shortener)


(defvar *keyword-package* (Find-Package :Keyword))


(Defun sane-package ()
  #+(or :ccl lispworks) *package*
  #+sbcl (sb-int:sane-package))


(Defun find-external-symbol (String Package)
  ;; bug?
  (declare (ftype (function (simple-string package)
                            (values T T))
                  system::find-external-symbol))
  (system::find-external-symbol String Package))


(Defun package-%local-nicknames (pkg)
  . #+lispworks ((Declare (Ignore pkg)) nil)
    #+sbcl ((sb-impl::package-%local-nicknames pkg)))


(Defun output-symbol-name (name Stream &Optional maybe-quote)
  #+lispworks (Declare (Ignore maybe-quote))
  #+sbcl (sb-kernel::output-symbol-name name stream maybe-quote)
  #+lispworks
  (let ((sys::*output-stream* Stream))
    (sys::output-cased-symbol (string name))))


(defun output-symbol (object Stream)
  (if (or *print-escape* *print-readably*)
      (let ((package (symbol-package object))
            (name (symbol-name object))
            (current (sane-package)))
        (cond
         ;; The ANSI spec "22.1.3.3.1 Package Prefixes for Symbols"
         ;; requires that keywords be printed with preceding colons
         ;; always, regardless of the value of *PACKAGE*.
         ((eq package *keyword-package*)
          (write-char #\: stream))
         ;; Otherwise, if the symbol's home package is the current
         ;; one, then a prefix is never necessary.
         ((eq package current))
         ;; Uninterned symbols print with a leading #:.
         ((null package)
          (when (or *print-gensym* *print-readably*)
            (write-string "#:" stream)))
         (t
          (multiple-value-bind (symbol accessible)
                               (find-symbol name current)
            ;; If we can find the symbol by looking it up, it need not
            ;; be qualified. This can happen if the symbol has been
            ;; inherited from a package other than its home package.
            ;;
            ;; To preserve print-read consistency, use the local nickname if
            ;; one exists.
            (unless (and accessible (eq symbol object))
              (let ((prefix (or (car (rassoc package (package-%local-nicknames current)))
                                (PACKAGE-NAME-SHORTENER::SHORTEST-PACKAGE-NAME package))))
                (output-symbol-name prefix stream))
              (multiple-value-bind (symbol externalp)
                                   (find-external-symbol name package)
                (declare (ignore symbol))
                (if externalp
                    (write-char #\: stream)
                    (write-string "::" stream)))))))
        (output-symbol-name name stream))
      (output-symbol-name (symbol-name object) stream nil)))


(defun shorten ()
  (lw:defadvice (system::output-symbol short :around) (object)
    (output-symbol object sys::*output-stream*)))


(defun unshorten ()
  (hcl:delete-advice system::output-symbol short))


(shorten)

;;; *eof*


