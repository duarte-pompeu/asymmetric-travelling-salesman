;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;         Heuristicas
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun best-h (estado)
	;	(print "best-h")

		; (let ((caminho (atsp-estado-caminho estado))
		; 	(value 0))
		; (if (and (> 1 (length caminho)) (eq (first (caminho)) (first (last caminho))))
		; 	(return-from best-h 0)
		; (dolist	(x caminho)
		; 	(setf value (+ value (nth x *medias*)))))
		; (print value)
		; value))
		

		(let ((caminho (atsp-estado-caminho estado))
			(value *custoTotal*))
		;(print caminho)
		;(print "*custoTotal*")
		;(print *custoTotal*)
		(if (and (> 1 (length caminho)) (eq (first (caminho)) (first (last caminho))))
			(return-from best-h 0)
		(dolist	(x caminho)
			(setf value (- value (nth x *medias*)))))
		(format t "h=~D, c=~D, f=~D" value (custo estado) (+ value (custo estado)))
		value))


; diz so o numero de caminhos qe faltam ate a solucao
(defun alter-h (estado)
	;	(print "alter-h")
	;(print (- (1+ *dim*) (length (atsp-estado-caminho estado))))
	(return-from alter-h (- (1+ *dim*) (length (atsp-estado-caminho estado)))))
	

(defun distances-h (estado)
	(let ((a 0)
		(b 0)
		(c 0))
		
		(+ a b c)
))
