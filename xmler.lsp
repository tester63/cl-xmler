
(in-package :xmler)
(require :uiop)


(defconstant +space+ #\u0020)

(defun elemp (list)
  "Is list a valid element?"
  (let ((head (first list))
	(tail (first (last list))))
    (and
     ;; An element list should have (head content tail)
     (= (list-length list) 3)
     ;; Head and tail should be strings (tags)
     (and (stringp head) (stringp tail))
     (string= (first (uiop:split-string (string-trim "</>" head)))
	      (string-trim "</>" tail)))))

(defun assign-attr (attr-cell)
  (let ((name (car attr-cell))
	(type (cdr attr-cell)))
    (uiop:strcat name "=\"" type "\"")))


(defun opening-tag (name &key
			   empty
			   (attrs-alist '())
			   (open-mark "<")
			   (close-mark ">")
			   (empty-close-mark "/>"))
  "Returns the <elem attrs...> string."
  (uiop:strcat open-mark name
	       ;; Join strings
	       (if attrs-alist
		   (format nil " ~{~A~^ ~}" (mapcar #'assign-attr attrs-alist))
		   "")
	       (if empty empty-close-mark close-mark)))

(defun closing-tag (name)
  "Returns the closing </elem> tag string."
  (uiop:strcat "</" name ">"))

(defun elem (name &key attr cont
		    (open-mark "<")
		    (close-mark ">")
		    (empty-close-mark "/>"))
  "Returns the element."
  (cond
    ;; +attrs +conts
    ;; If cont is just a string, return an appended-string, otherwise a list.
    ((and attr cont)
     (if (stringp cont)
	 (uiop:strcat
	  (opening-tag name
		       :attrs-alist attr
		       :open-mark open-mark
		       :close-mark close-mark)
	  cont
	  (closing-tag name))
	 (list (opening-tag name
			    :attrs-alist attr
			    :open-mark open-mark
			    :close-mark close-mark)
	       cont
	       (closing-tag name))))
    ;; -attrs +conts
    (cont
     (if (stringp cont)
	 (uiop:strcat
	  (opening-tag name
		       :open-mark open-mark
		       :close-mark close-mark)
	  cont
	  (closing-tag name))
	 (list (opening-tag name
			    :open-mark open-mark
			    :close-mark close-mark)
	       cont
	       (closing-tag name))))
    ;; +attrs -conts (empty element)
    (attr (opening-tag name
		       :attrs-alist attr
		       :empty t
		       :open-mark open-mark
		       :empty-close-mark empty-close-mark))
    ;; -attrs -conts (also empty)
    (t (opening-tag name
		    :empty t
		    :open-mark open-mark
		    :empty-close-mark empty-close-mark))))


(defun indent (elems depth)
    (cond
     ((null elems) nil)
     ;; It can be a string like (e.g. "4", "<.../>")
     ((stringp elems) (list (cons depth elems)))
     ;; A element type is like (element-form="<onset>" element-form=content element-form="</termination>")
     ((elemp elems) (append (list (cons depth (first elems)))
			    (indent (second elems) (1+ depth))
			    (list (cons depth (third elems)))))
     ;; Else its a list of elements and/or strings
     (t (apply #'append (mapcar #'(lambda (e) (indent e depth)) elems)))))

(defun lines (indented-elems)
  (mapcar #'(lambda (elem-cell)
	      (uiop:strcat (make-string (car elem-cell)
					:initial-element +space+)
			   (cdr elem-cell)))
	  indented-elems))

(defun xml (elems &key (path "/tmp/etude.xml") (depth 0))
  (let ((indented-lines (lines (indent elems depth))))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (loop for line in indented-lines
	 do (format stream (uiop:strcat line "~%"))))))
