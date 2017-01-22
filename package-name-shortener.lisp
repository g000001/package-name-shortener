;;;; package-name-shortener.lisp -*- Mode: Lisp;-*- 

(Cl:In-Package :package-name-shortener)


(Deftype string-or-null ()
  '(Or String null))


(Defun shortest-package-name (Package)
  (Declare (Type Package Package))
  (Let ((shortest (Package-Name Package)))
    (Declare (type string-or-null shortest))
    (Dolist (nick (Package-Nicknames package) shortest)
      (Declare (Type string-or-null nick))
      (When (< (If nick (Length nick) 0)
               (If nick (Length shortest) 0))
        (Setq shortest nick)))))


#||||||||||||||||
;=>  (KMRCL:AIF KMRCL:ACOND)


#+lispworks '(Defmethod Print-Object :around ((object Symbol) out-stream)
  (Call-Next-Method))

;(SYSTEM::OUTPUT-SYMBOL-OR-NAME 'kl:aif t)

;(SYSTEM::OUTPUT-SYMBOL 'kl:aif)

;(SYSTEM::OUTPUT-SYMBOL 'if)


(defvar *keyword-package* (Find-Package :Keyword))


(Defun sane-package ()
  #+(or :ccl lispworks) *package*
  #+sbcl (sb-int:sane-package))


(Defun find-external-symbol (String Package)
  #+lispworks (system::find-external-symbol String Package)
  #+sbcl (sb-impl::find-external-symbol String package))


(Defun package-%local-nicknames (pkg)
  . #+lispworks ((Declare (Ignore pkg)) nil)
    #+sbcl ((sb-impl::package-%local-nicknames pkg)))


(Defun output-symbol-name (name Stream &Optional maybe-quote)
  #+lispworks (Declare (Ignore maybe-quote))
  #+sbcl (sb-kernel::output-symbol-name name stream maybe-quote)
  #+lispworks
  (If (Every #'Upper-Case-P (Remove-If-Not #'Alpha-Char-P name ))
      (Write-String (case *print-case*
                      (:downcase (string-downcase name))
                      (otherwise name))
                    Stream)
      (Progn
        (Write-String "|" Stream)
        (Write-String name Stream)
        (Write-String "|" Stream))))


;; (sb-kernel::output-symbol-name  "foo" t)
;=> |foo|

'(Let ((*Print-Gensym* nil))
 (With-Output-To-String (*standard-output*)
   #|(system::output-symbol 'foo)|#
   #|(system::output-symbol '#:foo)|#
   (system::output-symbol 'kl:aif)))

;; (List 'kl:aif 'kl:aif 'kl:aif 'kl:aif )

(defun output-symbol
       (object &optional (Stream 
                          #-lispworks *Standard-Output*
                          #+lispworks sys::*output-stream*))
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


(lw:defadvice (system::output-symbol short :around) (object)
  (output-symbol object))

(hcl:delete-advice system::output-symbol short)


(system::output-symbol )
;;; *eof*

(with-output-to-string (sys::*output-stream*)
  (system::output-symbol 'foo))

(with-output-to-string (*standard-output*)
  (system::output-symbol 'foo))


(Print 'foo)
||||||||||||||||#
