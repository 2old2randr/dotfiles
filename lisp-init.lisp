;;;; Generic Lisp startup file that works across SBCL and Clozure

(require :asdf)	; Trying to find ASDF is more trouble than it is worth
;; Load and compile asdf
;; #-:asdf
;; (handler-case
;;     (when *asdf-pathname*
;;       (load #+:lispworks (or (compile-file-if-needed *asdf-pathname*)
;; 		*asdf-pathname*)
;; 	    #-lispworks *asdf-pathname*))
;;   (#+:lispworks conditions:fasl-error
;;    #+:allegro excl:file-incompatible-fasl-error
;;    #+:sbcl sb-ext:invalid-fasl
;;    #-(or :lispworks :allegro :sbcl) error ()
;;    (load (compile-file *asdf-pathname*))))

;;;;--------------------------------------------------------------------------------
;;;; Setup useful directories and search paths
;;;;--------------------------------------------------------------------------------

(in-package :cl-user)

(let* ((home-dir (#+:clozure ccl:getenv
		  #+:sbcl posix-getenv
			     "HOME"))
       (lib-dir (concatenate 'string home-dir "/lib/lisp/"))
       (app-path (concatenate 'string home-dir "/src/cl/app/**/*.*"))
       (lib-path (concatenate 'string lib-dir "**/*.*")))
  #+:clozure
  (pushnew "ccl:tools;asdf-install;" asdf:*central-registry* :test #'equal)

  (setf (logical-pathname-translations "app") (list (list "**;*.*" app-path))
	(logical-pathname-translations "lib") (list (list "**;*.*" lib-path)))

  ;; So that 'require module' will do 'asdf oos load-op' in CCL
  #+:clozure
  (defun module-provide-asdf (name)
    (let* ((name (string-downcase name))
	   (system (asdf:find-system name nil)))
      (when system
	(asdf:oos 'asdf:load-op name)
	t)))
  #+:clozure
  (pushnew 'module-provide-asdf *module-provider-functions*)

  (defun walk-asdf-package-dirs (asdf-dir)
    "Loops through all sub-directories and adds those containing
system definitions to ASDF's central registry."
    (let ((path (make-pathname :name :wild :type :wild :defaults asdf-dir)))
      (dolist (dir-candidate #+(or :lispworks :sbcl) (directory path)
			     #+:clozure (directory path :directories t))
	(when #+:lispworks (lw:file-directory-p dir-candidate)
	      #+:clozure (directory-pathname-p dir-candidate)
	      #+:sbcl (and (not (pathname-name dir-candidate))
			   (not (pathname-type dir-candidate)))
	      (let ((asd-candidate (merge-pathnames "*.asd" dir-candidate)))
		(when (directory asd-candidate)
		  (pushnew dir-candidate asdf:*central-registry*
			   :test #'equal)))))))
  (walk-asdf-package-dirs (concatenate 'string lib-dir "pkgs/")))


;;;;----------------------------------------------------------------------------------
;;;; Define a couple of functions in ASDF to handle stale FASLs and reloading systems
;;;;----------------------------------------------------------------------------------

(in-package :asdf)
(export 'reload)

;; Automatically recompile out-of-date FASLs when loading through asdf
(defmethod perform :around ((o load-op) (c cl-source-file))
  "When trying to load a Lisp file with a stale FASL version, auto recompile the file."
  ;; from Cliki
  (handler-case
      (call-next-method o c)
    (#+:lispworks conditions:fasl-error
      #+:allegro excl:file-incompatible-fasl-error
      #+:sbcl sb-ext:invalid-fasl
      #-(or :lispworks :allegro :sbcl) error ()
      (format t "FASL load error for ~A (recompiling)~%" c)
      (perform (make-instance 'compile-op) c)
      (call-next-method))))

(defun reload (system)
  (let ((name (coerce-name system)))
    (remhash name *defined-systems*)
    (oos 'load-op name)))

;;;;--------------------------------------------------------------------------------
;;;; Load useful libraries by default
;;;;--------------------------------------------------------------------------------

(in-package :cl-user)

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :sutils)
(require :cl-ppcre)
