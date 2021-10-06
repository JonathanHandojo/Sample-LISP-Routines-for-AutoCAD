;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                        Block Overkill                                        ;;;
;;;                                 Created by Jonathan Handojo                                  ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;  This program allows the user to either delete blocks that may have been accidentally        ;;;
;;;  placed on top of one another (similar to the OVERKILL command), or move them to a user-     ;;;
;;;  specified layer. The command will proceed to process blocks that shares the same insertion  ;;;
;;;  point effective name, and effective scale. This is then followed by a circle of a desired   ;;;
;;;  radius drawn in the insertion points of the deleted block in the "BOVERKILL-Duplicates"     ;;;
;;;  layer, and a report of how many blocks are deleted in how many unique locations is printed  ;;;
;;;  into the command line. If your block is dynamic with modified properties, the original      ;;;
;;;  OVERKILL command won't be able to catch the blocks as it does not have a defined block      ;;;
;;;  name. As such, this command will catch blocks through effective name of the block.          ;;;
;;;                                                                                              ;;;
;;;  This program proceeds to process blocks sharing the same insertion point through a          ;;;
;;;  specific fuzz, and effective name, and scale. So, beware that blocks that shares the        ;;;
;;;  criteria states but is rotated differently will still be deleted. The reason this was not   ;;;
;;;  accounted for is because objects when an object is mirrored using the MIRROR command,       ;;;
;;;  rotation values are adjusted, and it's not possible to determined if an object is mirrored  ;;;
;;;  (at least from my one-year experience of AutoLISP coding).                                  ;;;
;;;                                                                                              ;;;
;;;  This inspiration comes from using Lee Mac's Block Counter code reporting incorrect values   ;;;
;;;  due to blocks being duplicated on top of another. With thousands of blocks present in the   ;;;
;;;  drawing, it's almost impossible to check for block duplicates, thus I programmed this       ;;;
;;;  piece of LISP to help me with take-off values. Type BOVERKILL to invoke command.            ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                     Versions and Updates                                     ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;  Version 1.0 (15/04/20) – First release                                                      ;;;
;;;                                                                                              ;;;
;;;  ------------------------------------                                                        ;;;
;;;                                                                                              ;;;
;;;  Version 1.1 (16/04/2020)                                                                    ;;;
;;;                                                                                              ;;;
;;;  - Added an option to move the duplicated blocks to a layer of a specified choice            ;;;
;;;                                                                                              ;;;
;;;  ------------------------------------                                                        ;;;
;;;                                                                                              ;;;
;;;  Version 1.2 (25/04/2020)                                                                    ;;;
;;;                                                                                              ;;;
;;;  - Bug fix due to variable setting errors.                                                   ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:boverkill ( / *error* acadobj activeundo adoc blk blklist blks deleted duplay dups lay msp opt rad rtn tolerance vn x y)
    (defun *error* ( msg )
	(vla-EndUndoMark adoc)
	(if (not (wcmatch (strcase msg T) "*break*,*cancel*,*exit*"))
	    (princ (strcat "Error: " msg))
	    )
	)
    
    (setq acadobj (vlax-get-acad-object)
	  adoc (vla-get-ActiveDocument acadobj)
	  blk (vla-get-blocks adoc)
	  msp (vla-get-ModelSpace adoc)
	  activeundo nil)
    (if (= 0 (logand 8 (getvar "UNDOCTL"))) (vla-StartUndoMark adoc) (setq activeundo T))
    
    (setq rad 60	; <--- Circle radius
	  blks (ssget '((0 . "INSERT")))
	  deleted (ssadd))
    
    (if blks
	(progn
	    (initget 6)
	    (setq tolerance (cond ((progn (initget 6) (getreal "\nSpecify overkill tolerance <0.00001>: "))) (1e-5)))
	    (setq blklist (mapcar '(lambda (x / vn)
				       (list x (cdr (assoc 10 (entget x)))
					     (vla-get-EffectiveName (setq vn (vlax-ename->vla-object x)))
					     (list
						 (vla-get-XEffectiveScaleFactor vn)
						 (vla-get-YEffectiveScaleFactor vn)
						 (vla-get-ZEffectiveScaleFactor vn)
						 )
					     )
				       )
				  (JH:selset-to-list blks)
				  )
		  dups ((lambda (x / rtn)
			    (mapcar
				'(lambda (y)
				     (if
					 (vl-some
					     '(lambda (z)
						  (and
						      (equal (cadr y) (cadr z) tolerance)
						      (eq (caddr y) (caddr z))
						      (equal (last y) (last z) tolerance)
						      )
						  )
					     (cdr x)
					     )
					 (setq rtn (cons y rtn))
					 )
				     (setq x (cdr x))
				     )
				x
				)
			    (reverse rtn)
			    )
			   blklist
			   )
		  )
	    (if dups
		(progn
		    (setq opt (cond
				  ((progn (initget "Delete Move")
				       (getkword (strcat "\n" (itoa (length dups)) " duplicates found. Delete or move to another layer? [Delete/Move] <Delete>: "))
				       )
				   )
				  ("Delete")
				  )
			  )
		    
		    (if (eq opt "Move")
			(while
			    (cond
				((null (setq lay (progn (initget 1 "Current") (entsel "\nSelect destination layer or [Current]: ")))) (princ "\nNothing selected"))
				((eq lay "Current") (null (setq lay (getvar "CLAYER"))))
				((null (setq lay (cdr (assoc 8 (entget (car lay)))))))
				)
			    )
			)
		    
		    (foreach x dups
			(setq deleted
				 (ssadd
				     (entmakex
					 (list
					     '(0 . "CIRCLE")
					     '(100 . "AcDbEntity")
					     '(67 . 0)
					     '(410 . "Model")
					     '(8 . "BOVERKILL-Duplicates")
					     '(100 . "AcDbCircle")
					     (cons 10 (cadr x))
					     (cons 40 rad)
					     )
					 )
				     deleted
				     )
			      )
			(if (eq opt "Move") (entmod (subst (cons 8 lay) (assoc 8 (entget (car x))) (entget (car x)))) (entdel (car x)))
			)
		    (princ (strcat "\n" (itoa (length dups)) " duplicates "
				   (if (eq opt "Delete") "deleted" (strcat "moved to layer \"" lay "\""))
				   " in " (itoa (length (LM:UniqueFuzz (mapcar 'cadr dups) tolerance))) " locations, refer selected circles."))
		    (sssetfirst nil deleted)
		    )
		(princ "\nNo duplicates found")
		)
	    (if (setq duplay (tblobjname "LAYER" "BOVERKILL-Duplicates")) (entmod (subst '(62 . 1) (assoc 62 (entget duplay)) (entget duplay))))
	    )
	)
    
    (if activeundo nil (vla-EndUndoMark adoc))
    (princ)
    )


(defun JH:selset-to-list (selset / lst iter)	; Returns all entities within a selection set into a list.
    (setq iter 0)
    (repeat (sslength selset)
	(setq lst (cons (ssname selset iter) lst)
	      iter (1+ iter))
	)
    (reverse lst)
    )

;;; --------------------------------- ONLINE REFERENCES --------------------------------- ;;;

;; Unique with Fuzz  -  Lee Mac
;; Returns a list with all elements considered duplicate to
;; a given tolerance removed.

(defun LM:UniqueFuzz ( l f / x r )
    (while l
        (setq x (car l)
              l (vl-remove-if (function (lambda ( y ) (equal x y f))) (cdr l))
              r (cons x r)
        )
    )
    (reverse r)
)
