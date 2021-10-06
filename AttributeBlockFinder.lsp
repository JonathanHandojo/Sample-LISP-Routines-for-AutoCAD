;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                   AttributeBlockFinder.lsp                                   ;;;
;;;                                 Created by Jonathan Handojo                                  ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;  This LISP routine offers the user multiple ways to find attributed blocks within your       ;;;
;;;  current drawing. This routine simulates the behaviour to the Quick Select feature but       ;;;
;;;  enhanced for use with attributed blocks.                                                    ;;;
;;;                                                                                              ;;;
;;;  The user specifies a tag and its value, and adds to the list of search criteria. Blank      ;;;
;;;  tags are not allowed. However, blank values are allowed. The program offers two selection   ;;;
;;;  criteria: the block to be selected must satisfy                                             ;;;
;;;                                                                                              ;;;
;;;  1. All the criteria that is added into the list, or                                         ;;;
;;;  2. At least one of the criterion within the list.                                           ;;;
;;;                                                                                              ;;;
;;;  The program also includes a search criterion whether the tags, values, or both should be    ;;;
;;;  case-sensitive or case-insensitive. In addition, the user can specify to either use a       ;;;
;;;  wildcard match for all the search tags/values, or search for those whose tags/values:       ;;;
;;;                                                                                              ;;;
;;;  1. Equals                                                                                   ;;;
;;;  2. Starts with                                                                              ;;;
;;;  3. Ends with, or                                                                            ;;;
;;;  4. Contains                                                                                 ;;;
;;;                                                                                              ;;;
;;;  the values within the added list.                                                           ;;;
;;;                                                                                              ;;;
;;;  Type ABF to invoke command.                                                                 ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                     Version and Updates                                      ;;;
;;;                                                                                              ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;  Version 1.0 (08/12/2020) – First Release                                                    ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:abf (/ ats dcf dcld dcop det dl ent fil fn i rtn ss tagl tg ts vall vl)
    (if (ssget "_A" (setq fil (list '(0 . "INSERT") '(66 . 1) (cons 410 (getvar 'ctab)))))
	(progn
	    (setq dcf (vl-filename-mktemp nil nil ".dcl")
		  dcop (open dcf "w")
		  )
	    (foreach x
		     '(
		       " spc : spacer { width = 1; } "
		       " edb : edit_box { edit_width = 10; } "
		       " lb : list_box { width = 20; } "
		       " dlg : dialog { label = \"Attribute Block Finder\"; "
		       "	: spacer { height = 0.5; } "
		       "	: row { "
		       "		: column { "
		       "			: row { fixed_width = true; alignment = centered; "
		       "				: popup_list { label = \"Apply to: \"; key = \"sel\"; width = 35; } "
		       "				: image_button { key = \"cur\"; height = 0.5; width = 3.6; } "
		       "				} "
		       "			: row { fixed_width = true; alignment = centered; "
		       "				: edb { label = \"Tag: \"; key = \"tag\"; } "
		       "				spc; "
		       "				: edb { label = \"Value: \"; key = \"val\"; } "
		       "				spc; "
		       "				: button { label = \"Add\"; key = \"add\"; width = 11; } "
		       "				} "
		       "			: boxed_radio_column { label = \"Selection criteria\"; "
		       "				: radio_button { label = \"Block must satisfy all criteria in the list\" ; key = \"sel1\"; } "
		       "				: radio_button { label = \"Block must satisfy at least one criterion in the list\" ; key = \"sel2\"; } "
		       "				} "
		       "			: boxed_column { label = \"Search criteria\"; "
		       "				: row { "
		       "					: boxed_column { label = \"Tags\"; "
		       "						: toggle { label = \"Case-insensitive\"; key = \"tagcas\"; } "
		       "						: toggle { label = \"Use wildcard match\"; key = \"tagwil\"; } "
		       "						: text { label = \"Select blocks whose tags \"; key = \"tagtx1\"; } "
		       "						: popup_list { width = 6; key = \"tagmat\"; } "
		       "						: text { label = \"added items in the list\"; key = \"tagtx2\"; } "
		       "						} "
		       "					: boxed_column { label = \"Values\"; "
		       "						: toggle { label = \"Case-insensitive\"; key = \"valcas\"; value = \"1\"; } "
		       "						: toggle { label = \"Use wildcard match\"; key = \"valwil\"; } "
		       "						: text { label = \"Select blocks whose values \"; key = \"valtx1\"; } "
		       "						: popup_list { width = 6; key = \"valmat\"; } "
		       "						: text { label = \"added items in the list\"; key = \"valtx2\"; } "
		       "						} "
		       "					} "
		       "				} "
		       "			} "
		       "		spc; "
		       "		: lb { label = \"Tags\"; key = \"taglst\"; } "
		       "		: lb { label = \"Values\"; key = \"vallst\"; } "
		       "		spc; "
		       "		} "
		       "	ok_cancel; "
		       "	errtile; "
		       "	} "
		       )
		(write-line x dcop)
		)
	    (close dcop)
	    (setq dcld (load_dialog dcf)
		  det
		     (cons (list "sel" (if (or ss (setq ss (ssget "_I" fil))) "1" "0"))
			   '(
			     ("sel" "0")
			     ("tag" "")
			     ("val" "")
			     ("sel1" "1")
			     ("sel2" "0")
			     ("tagcas" "1")
			     ("tagwil" "0")
			     ("tagmat" "0")
			     ("tagcas" "1")
			     ("tagwil" "0")
			     ("tagmat" "0")
			     ("valcas" "1")
			     ("valwil" "0")
			     ("valmat" "0")
			     )
			   )
		  )
	    
	    (while (not (member dl '(0 1)))
		(if (not (new_dialog "dlg" dcld)) (princ "\nUnable to load dialog")
		    (progn
			(start_list "sel")
			(mapcar 'add_list (append '("Entire drawing") (if ss '("Current Selection"))))
			(end_list)
			(abf:setdet)
			((lambda (x / i j)
			     (start_image "cur")
			     (fill_image 0 0 (dimx_tile "cur") (dimy_tile "cur") -15)
			     (setq i 0)
			     (foreach y x
				 (setq j -1)
				 (foreach z y (vector_image (setq j (1+ j)) i j i z))
				 (setq i (1+ i))
				 )
			     (end_image)
			     )
			    '(
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 253 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 253 254 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 254 254 254 254 254 253 253 253 254 254 254 254 254 254 -15 -15 -15)
			      (-15 -15 -15 254 009 009 009 009 009 252 254 253 009 009 009 009 009 009 254 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 254 252 009 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      (-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15)
			      )
			    )
			(foreach x '("tagmat" "valmat")
			    (start_list x)
			    (mapcar 'add_list '("equals" "begins with" "ends with" "contains"))
			    (end_list)
			    )
			(mapcar
			    '(lambda (x y)
				 (start_list x)
				 (mapcar 'add_list (reverse y))
				 (end_list)
				 )
			    '("taglst" "vallst")
			    (list tagl vall)
			    )
			(action_tile "sel" "(setq det (subst (list \"sel\" (get_tile \"sel\")) (assoc \"sel\" det) det))")
			(action_tile "add"
			    (vl-prin1-to-string
				(quote
				    (if
					(eq (get_tile "tag") "")
					(progn (set_tile "error" "Please specify tag") (mode_tile "tag" 2))
					(progn
					    (setq tagl (cons (get_tile "tag") tagl)
						  vall (cons (get_tile "val") vall)
						  )
					    (mapcar
						'(lambda (x y / tg)
						     (setq tg (get_tile y))
						     (start_list x 2)
						     (add_list tg)
						     (end_list)
						     (set_tile y "")
						     )
						'("taglst" "vallst")
						'("tag" "val")
						)
					    (set_tile "error" "")
					    )
					)
				    )
				)
			    )
			(mapcar
			    '(lambda (x)
				 (action_tile (car x)
				     (vl-prin1-to-string
					 (list 'mapcar
					       (list 'function
						     (list 'lambda '(y)
							   (list 'mode_tile 'y
								 (list 'if
								       (list 'eq
									     (list 'get_tile (car x))
									     "0"
									     )
								       0
								       1
								       )
								 )
							   )
						     )
					       (cons 'list (cdr x))
					       )
					 )
				     )
				 )
			    '(
			      ("tagwil" "tagtx1" "tagmat" "tagtx2")
			      ("valwil" "valtx1" "valmat" "valtx2")
			      )
			    )
			(action_tile "accept"
			    (vl-prin1-to-string
				(quote
				    (progn
					(setq rtn (ssadd))
					(if (eq (get_tile "sel") "0") (setq ss (ssget "_A" fil)))
					(setq fn (if (eq (get_tile "sel1") "1") 'vl-every 'vl-some)
					      tagl (mapcar '(lambda (x) (abf:wcmatch x "tag")) (reverse tagl))
					      vall (mapcar '(lambda (x) (abf:wcmatch x "val")) (reverse vall))
					      )
					(if (eq (get_tile "tagcas") "1") (setq tagl (mapcar 'strcase tagl)))
					(if (eq (get_tile "valcas") "1") (setq vall (mapcar 'strcase vall)))
					(repeat (setq i (sslength ss))
					    (and
						(setq i (1- i) ent (ssname ss i) ats (abf:getattributes ent))
						(apply fn
						       (list
							   '(lambda (x y)
								(vl-some
								    '(lambda (a)
									 (and
									     (wcmatch (car a) x)
									     (wcmatch (cdr a) y)
									     )
									 )
								    ats
								    )
								)
							   tagl
							   vall
							   )
						       )
						(setq rtn (ssadd ent rtn))
						)
					    )
					(done_dialog 1)
					)
				    )
				)
			    )
			(action_tile "cancel" "(done_dialog 0)")
			(action_tile "cur" "(setq det (abf:getdet)) (done_dialog 2)")
			(setq dl (start_dialog))
			(if (= dl 2)
			    (and
				(setq ts (abf:ssget ss))
				(setq det (subst '("sel" "1") (assoc "sel" det) det) ss ts)
				)
			    )
			)
		    )
		)
	    (unload_dialog dcld)
	    (vl-file-delete dcf)
	    (and rtn (sssetfirst nil rtn))
	    )
	(alert "\nNo attributed blocks in the current tab/layout")
	)
    (princ)
    )

(defun abf:getdet nil
    (mapcar '(lambda (x) (list (car x) (get_tile (car x)))) det)
    )

(defun abf:setdet nil
    (mapcar '(lambda (x) (apply 'set_tile x)) det)
    (foreach x
	     '(
	       ("tagwil" "tagtx1" "tagmat" "tagtx2")
	       ("valwil" "valtx1" "valmat" "valtx2")
	       )
	(mapcar
	    '(lambda (y)
		 (mode_tile y (if (eq (get_tile (car x)) "0") 0 1))
		 )
	    (cdr x)
	    )
	)
    )

(defun abf:ssget (ss / pck rtn)
    (setq pck (getvar 'pickfirst))
    (setvar 'pickfirst 0)
    (setq rtn (vl-catch-all-apply 'ssget (list fil)))
    (setvar 'pickfirst pck)
    (if
	(not
	    (or
		(vl-catch-all-error-p rtn)
		(null rtn)
		)
	    )
	rtn
	)
    )

(defun abf:wcmatch (str flg / wm)
    (setq wm (get_tile (strcat flg "mat")))
    (if (eq (get_tile (strcat flg "cas")) "1") (setq str (strcase str)))
    (cond
	((= (get_tile (strcat flg "wil")) "1") str)
	((progn (setq str (LM:escapewildcards str)) (= wm "0")) str)
	((= wm "1") (strcat str "*"))
	((= wm "2") (strcat "*" str))
	((= wm "3") (strcat "*" str "*"))
	)
    )

(defun abf:getattributes (blk / dt rtn)
    (while
	(= (cdr (assoc 0 (setq blk (entnext blk) dt (entget blk)))) "ATTRIB")
	(setq rtn
		 (cons
		     (cons
			 ((lambda (x) (if (eq (get_tile "tagcas") "1") (strcase x) x)) (cdr (assoc 2 dt)))
			 ((lambda (x) (if (eq (get_tile "valcas") "1") (strcase x) x)) (cdr (assoc 1 dt)))
			 )
		     rtn
		     )
	      )
	)
    (reverse rtn)
    )

;;; -------------------------------------- ONLINE REFERENCES -------------------------------------- ;;;

;; Escape Wildcards  -  Lee Mac
;; Escapes wildcard special characters in a supplied string

(defun LM:escapewildcards ( str )
    (vl-list->string
        (apply 'append
            (mapcar
               '(lambda ( c )
                    (if (member c '(35 64 46 42 63 126 91 93 45 44))
                        (list 96 c)
                        (list c)
                    )
                )
                (vl-string->list str)
            )
        )
    )
)
