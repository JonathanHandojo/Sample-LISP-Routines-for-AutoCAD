;; LayerSelect --> Jonathan Handojo
;; Allows the user to select all object inside a layer simply by selecting one of them.
;; Invoke the command by typing LAYSEL

(defun c:laysel ( / esclay lay obj)
    (if (setq obj (entsel "\nSelect an object where all objects in that layer will be selected: "))
        (progn
            (setq
                lay (cdr (assoc 8 (entget (car obj))))
                esclay 
                    (vl-list->string
                        (apply 'append
                            (mapcar
                                '(lambda (x)
                                    (if 
                                        (vl-position x '(35 64 46 42 63 126 91 93 45 44))
                                        (list 96 x)
                                        (list x)
                                    )
                                )
                            (vl-string->list lay)
                            )
                        )
                    )
            )
            (sssetfirst nil (ssget "_X" (list (cons 8 esclay) (cons 410 (getvar 'ctab)))))
        )
    )
    (princ)
)