;; Group Layer Lock - Jonathan Handojo
;; Locks all layers within a selection set.
;; Issue the command GRLAYLCK, select objects, and the layers of every
;; object selected will be locked.

(defun c:grlaylck ( / acadobj adoc i lay lck objl ss)
    (setq acadobj (vlax-get-acad-object)
	  adoc (vla-get-ActiveDocument acadobj)
	  )
    (vla-StartUndoMark adoc)
    (and
	(eq (type (setq ss (vl-catch-all-apply 'ssget))) 'pickset)
	(repeat (setq i (sslength ss))
	    (if
		(and
		    (setq objl (cdr (assoc 8 (entget (ssname ss (setq i (1- i))))))
			  lay (tblobjname "layer" objl)
			  lck (cdr (assoc 70 (entget lay)))
			  )
		    (= 0 (logand 4 lck))
		    )
		(entmod
		    (subst
			(cons 70 (+ lck 4))
			(assoc 70 (entget lay))
			(entget lay)
			)
		    )
		)
	    )
	)
    
    (vla-Regen adoc acActiveViewport)
    (vla-EndUndoMark adoc)
    (princ)
    )
