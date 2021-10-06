;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                     PrefixSuffixText.lsp                                     ;;;
;;;                                 Created by Jonathan Handojo                                  ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;  This LISP routine allows the user to add a common prefix and suffix to a selected group     ;;;
;;;  of texts in unlocked layers. Upon issuing the command PRESUF, the user is prompted a        ;;;
;;;  selection of texts and mtexts, and then prompted for a prefix and suffix text to append to  ;;;
;;;  all the selected texts.                                                                     ;;;
;;;                                                                                              ;;;
;;;  How to use the PRESUF command:                                                              ;;;
;;;                                                                                              ;;;
;;;  1. Select a group of texts you wish to modify                                               ;;;
;;;  2. Specify the prefix you want to append to the selected texts                              ;;;
;;;  3. Specify the suffix you want to append to the selected texts                              ;;;
;;;  4. Done                                                                                     ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:presuf (/ *error* acadobj activeundo adoc ent i msp prf ss suf txt)
    (defun *error* ( msg )
	(vla-EndUndoMark adoc)
	(if (not (wcmatch (strcase msg T) "*break*,*cancel*,*exit*"))
	    (princ (strcat "Error: " msg))
	    )
	)
    (setq acadobj (vlax-get-acad-object)
	  adoc (vla-get-ActiveDocument acadobj)
	  msp (vla-get-ModelSpace adoc)
	  activeundo nil)
    (if (= 0 (logand 8 (getvar "UNDOCTL"))) (vla-StartUndoMark adoc) (setq activeundo T))

    (if (setq ss (ssget "_:L" '((0 . "TEXT,MTEXT"))))
	(progn
	    (setq prf (getstring t "\nSpecify prefix to add: ")
		  suf (getstring t "\nSpecify suffix to add: ")
		  )
	    (if (not (and (eq prf "") (eq suf "")))
		(repeat (setq i (sslength ss))
		    (setq ent (ssname ss (setq i (1- i)))
			  txt (assoc 1 (entget ent))
			  )
		    (entmod (subst (cons 1 (strcat prf (cdr txt) suf)) txt (entget ent)))
		    )
		)
	    )
	)
    
    (if activeundo nil (vla-EndUndoMark adoc))
    (princ)
    )

(vl-load-com)
