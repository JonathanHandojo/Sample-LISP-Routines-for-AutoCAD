;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                       CommandSave.lsp                                        ;;;
;;;                                 Created by Jonathan Handojo                                  ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;  This AutoLISP routine allows the user to enable the trigger to enable an autosave feature   ;;;
;;;  which save your current document upon each time a certain command ends or is cancelled.     ;;;
;;;  The determination to create such a program is to prevent the loss of work from AutoCAD      ;;;
;;;  crashing. Having an AutoCAD command invoking the safe function every time does make your    ;;;
;;;  AutoCAD run bit slower, but at least this will help prevent any loss of work.               ;;;
;;;                                                                                              ;;;
;;;  The trigger is enabled by running the CMDSAVEON command. To disable this trigger, invoke    ;;;
;;;  the CMDSAVEOFF command. Alternatively, if you would like the command to be initialized on   ;;;
;;;  every time AutoCAD loads this program, you may uncomment the last line of the code by       ;;;
;;;  removing the semicolon at the beginning.                                                    ;;;
;;;                                                                                              ;;;
;;;  The list of commands that will trigger the safety feature are listed in the list below.     ;;;
;;;  The user may add as many commands or remove as many commands as they would like. Once the   ;;;
;;;  list is modified, the user will have to reload this program back into AutoCAD for the       ;;;
;;;  changes to take effect and issue the CMDSAVEON command once again (unless the trigger is    ;;;
;;;  enabled on startup).                                                                        ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                     Versions and Updates                                     ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;  Version 1.0 - (06/10/21) First Release                                                      ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *Cmd_Save:CommandList*
    
    ;; Append list of command names here. (supply the full name of the command)
    '(
    "LINE"
    "ROTATE"
    "COPY"
    "MOVE"
    "FILLET"
    ;"U"	; Undo when triggered with Ctrl + Z as opposed to typing the UNDO command. I highly suggest turning this and "UNDO" off
    )
    
    )

(defun c:cmdsaveon ( / )
    (if (not *Cmd_Save_Reactor*)
        (setq *Cmd_Save_Reactor*
            (VLR-Command-Reactor "Cmd_Autosave"
                '(
                    (:VLR-CommandEnded . Cmdsave:Action)
                    (:VLR-commandCancelled . Cmdsave:Action)
                )
            )
        )
    )
    (princ "\nCommand Autosave Enabled")
    (princ)
)

(defun c:cmdsaveoff ( / )
    (if *Cmd_Save_Reactor*
        (progn
            (vlr-remove *Cmd_Save_Reactor*)
            (setq *Cmd_Save_Reactor* nil)
        )
    )
    (princ "\nCommand Autosave Disabled")
    (princ)
)

(defun Cmdsave:Action (rct lst / )
    (and
        (eq (vlr-data rct) "Cmd_Autosave")
        (member (strcase (car lst)) (mapcar 'strcase *Cmd_Save:CommandList*))
        (vla-Save (JH:adoc))
    )
)

(defun JH:adoc nil 
    (eval 
    (list 'defun 'JH:adoc nil (vla-get-ActiveDocument (vlax-get-acad-object))))
    (JH:adoc)
)

;(c:cmdsaveon)