;; * package

(defpackage :spear-to-midi
  (:use :common-lisp)
  (:export 
   #:spear-to-midi))

(in-package :spear-to-midi)

(defun get-pathname-dir (path &optional file)
  (format nil "~a~a"
          (namestring
           (make-pathname :directory (pathname-directory path)
                          :device (pathname-device path)))
          (if file file "")))

(defun path-from-same-dir (&optional file)
  (let ((load-name (or *load-truename* *compile-file-truename*
                       *default-pathname-defaults*)))
    (get-pathname-dir load-name file)))

;; get path to local files with #'spear-to-midi-path
(let ((spear-to-midi-src-dir (path-from-same-dir)))
  (defun spear-to-midi-path (name)
    (format nil "~a~a" spear-to-midi-src-dir name)))

;; EOF package.lsp
