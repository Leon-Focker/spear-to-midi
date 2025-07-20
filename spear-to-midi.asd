(asdf:defsystem #:spear-to-midi
  :version "1.0"
  :default-component-class "cl-source-file.lsp"
  :description "convert spear text - partial files to midi"
  :author "Leon Focker"
  :license "GNU General Public License v3.0"
  :depends-on ("cm"
	       "cl-ppcre")
  :serial t
  :components ((:file "package")
	       (:file "utilities")
	       (:file "main")))

;; EOF spear-to-midi.asd
