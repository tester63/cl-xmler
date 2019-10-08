

(defpackage #:xmler
  (:use #:common-lisp)
  (:import-from #:uiop
		#:split-string
		#:strcat)
  (:export #:elem
	   #:xml))
