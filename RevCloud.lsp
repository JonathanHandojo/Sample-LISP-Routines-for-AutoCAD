;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                         RevCloud.lsp                                         ;;;
;;;                                 Created by Jonathan Handojo                                  ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;  This routine allows the user to instantly place revision clouds over the bounding box of    ;;;
;;;  objects. This program offers two methods of clouds, by means of the RC command and the RCM  ;;;
;;;  command. RC allows the user to select a group of objects and cloud these objects            ;;;
;;;  individually. However, the RCM command allows the user to select a group of objects and     ;;;
;;;  cloud these objects as a whole using a single cloud.                                        ;;;
;;;                                                                                              ;;;
;;;  The properties of the cloud can be adjusted through the settings below. Once the            ;;;
;;;  properties are changed, the routine needs to be reloaded back into AutoCAD for the changes  ;;;
;;;  to take affect.                                                                             ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                     Versions and Updates                                     ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;  Version 1.0 (06/10/21) - First Release                                                      ;;;
;;;                                                                                              ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun JH:RevCloudSettings ( / )
    (list
        200     ; Additional outside ffset from the bounding box of the object or selection set (or nil to use default 0)
        120     ; Arc radius of the cloud (or nil to use default 120)
        3       ; AutoCAD Colour Index of the cloud (or nil for none)
        20      ; Polyline width of the cloud. (or nil for none)
        "Prefab Text"     ; Layer (or nil to use current)
    )
)

(defun c:rcm ( / *error* activeundo acadobj adoc bnd msp ss)
    (defun *error* ( msg )
        (vla-EndUndoMark adoc)
        (if (not (wcmatch (strcase msg T) "*break*,*cancel*,*exit*"))
            (princ (strcat "Error: " msg))
        )
    )
    (setq
        acadobj (vlax-get-acad-object)
        adoc (vla-get-ActiveDocument acadobj)
        msp (vla-get-ModelSpace adoc)
        activeundo nil
    )
    (if (= 0 (logand 8 (getvar "UNDOCTL"))) (vla-StartUndoMark adoc) (setq activeundo T))
    (and
        (setq ss (ssget))
        (setq bnd (LM:ssboundingbox ss))
        (apply 'JH:RevCloud (append bnd (mapcar '(lambda (x) (nth x (JH:RevCloudSettings))) '(0 1 2 3 4))))
    )
    (if activeundo nil (vla-EndUndoMark adoc))
    (princ)
)

(defun c:rc ( / *error* activeundo acadobj adoc ent i ll msp ss ur)
    (defun *error* ( msg )
        (vla-EndUndoMark adoc)
        (if (not (wcmatch (strcase msg T) "*break*,*cancel*,*exit*"))
            (princ (strcat "Error: " msg))
        )
    )
    (setq
        acadobj (vlax-get-acad-object)
        adoc (vla-get-ActiveDocument acadobj)
        msp (vla-get-ModelSpace adoc)
        activeundo nil
    )
    (if (= 0 (logand 8 (getvar "UNDOCTL"))) (vla-StartUndoMark adoc) (setq activeundo T))
    (and
        (setq ss (ssget))
        (repeat (setq i (sslength ss))
            (vla-getboundingbox (setq i (1- i) ent (vlax-ename->vla-object (ssname ss i))) 'll 'ur)
            (apply 'JH:RevCloud (append (mapcar 'vlax-safearray->list (list ll ur)) (mapcar '(lambda (x) (nth x (JH:RevCloudSettings))) '(0 1 2 3 4))))
        )
    )
    (if activeundo nil (vla-EndUndoMark adoc))
    (princ)
)

;; JH:RevCloud --> Jonathan Handojo
;; Draws a revision cloud between two point
;; p1, p2 - the corner points of the cloud
;; off - the horizontal and vertical offset from the bounding box of p1 and p2
;; rad - arc radius of the cloud
;; col - an integer denoting AutoCAD Color Index of the cloud
;; wid - polyline width of the polyline
;; lay - 



(defun JH:RevCloud (p1 p2 off rad col wid lay / bul i k l len ll lm p pof pts ur)
    (if (not off) (setq off 0))
    (if (not rad) (setq rad 120))
    (setq 
        ll (mapcar 'min p1 p2)
        ur (mapcar 'max p1 p2)
        ll (list (- (car ll) off) (- (cadr ll) off))
        ur (list (+ (car ur) off) (+ (cadr ur) off))
        len (mapcar 'abs (mapcar '- ll ur))
        i 0
        l 0
        bul (tan (/ (cvunit 110 "degrees" "radians") 4))
        pof (sqrt (* 2 (expt rad 2) (- 1 (cos (cvunit 110 "degrees" "radians")))))
    )
    (repeat 4
        (cond
            ((= i 0) (setq lm (car len) p ll))
            ((= i 1) (setq lm (cadr len) p (list (car ur) (cadr ll))))
            ((= i 2) (setq lm (car len) p ur))
            ((= i 3) (setq lm (cadr len) p (list (car ll) (cadr ur))))
        )
        (setq lm (+ lm l) k 0)
        (while 
            (> lm 0)
            (setq
                pts (cons 
                        (list
                            (cons
                                10
                                (mapcar
                                    '+ p 
                                    (list 
                                        (cond ((= i 0) (- (* pof k) l)) ((= i 2) (+ (* pof k -1) l)) (0.0))
                                        (cond ((= i 1) (- (* pof k) l)) ((= i 3) (+ (* pof k -1) l)) (0.0))
                                    )
                                )
                            )
                            '(40 . 0.0)
                            '(41 . 0.0) 
                            (cons 42 bul)
                        )
                        pts
                    )
                lm (- lm pof)
                k (1+ k)
                l 0
            )
        )
        (setq i (1+ i) l lm)
    )
    (entmakex
        (append
            (list
                '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
                (cons 90 (length (setq pts (reverse pts))))
                '(70 . 1)
            )
            (apply 'append 
                (mapcar
                    '(lambda (a b)
                         (if a (list (cons b a)))
                    )
                    (list wid col lay)
                    '(43 62 8)
                )
            )
            (apply 'append pts)
        )
    )
)

;; Tangent  -  Lee Mac
;; Args: x - real

(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-10))
        (/ (sin x) (cos x))
    )
)

;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; sel - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
    (repeat (setq idx (sslength sel))
        (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
        (if (and (vlax-method-applicable-p obj 'getboundingbox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
            )
            (setq ls1 (cons (vlax-safearray->list llp) ls1)
                  ls2 (cons (vlax-safearray->list urp) ls2)
            )
        )
    )
    (if (and ls1 ls2)
        (mapcar '(lambda ( a b ) (apply 'mapcar (cons a b))) '(min max) (list ls1 ls2))
    )
)

(vl-load-com)