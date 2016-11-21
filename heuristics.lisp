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
	

; FIXME: doesnt do anything
(defun distances-h (estado)
	(let ((a 0)
		(b 0)
		(c 0))
		
		(+ a b c)
))


; Heuristica media das distancias
; Atribuir maior peso da função h a nós com arcos de valores mais altos
; Ou seja, nós com arcos de baixo valor são beneficiados na função h
(defun media-distancias-h (estado)
	(let*(
		(problema (atsp-estado-problema estado))
		(n (first (array-dimensions problema)))
		(no (car (last (atsp-estado-caminho estado))))
		(somatorio 0)
		(media 0)
	)
		; somar distancia a todos os vizinhos
		(debug no)
		(loop for i from 0 to (1- n) do
			(setf somatorio (+ somatorio (aref problema no i)))
		)
		; descontar distancia a si próprio
		(setf somatorio (- somatorio (aref problema no no))) 
		(debug somatorio)
		
		(setf media (/ somatorio (- n 1)))
		(debug media)
		
		(debug "---")
		media
))