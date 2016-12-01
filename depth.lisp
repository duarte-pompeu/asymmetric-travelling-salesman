(defun r-depth (problema no-inicial)
	;inicializacao
	;depth-visits inicial = custos do problema
	(let* ((n (first (array-dimensions problema)))
		(n-loops 1000)
		; each time you visit (node number, visit), increase heuristic cost
		(depth-visits-array (make-array '(500 500)))
		)
	(loop for i from 0 to (1- n) do
		(loop for j from 0 to (1- n) do
			(setf (aref depth-visits-array i j) 1)))

	(let* ((min-custo 9999))

		;chamada a func recursiva
		(loop for i from 0 to n-loops
		do (let* ((caminho (depth-aux problema no-inicial '() 0 depth-visits-array))
				(custo (dcusto problema caminho))
			)
				(if (< custo min-custo)
					(progn
					(setf min-custo custo)

					(print caminho)
					(print custo)
				))
		))
)))

(defun depth-aux (problema no-actual caminho custo-actual depth-visits-array)
; profundidade: tamanho do caminho
	(let* ((n-nos (array-dimension problema 0))
		(prox-no 0)
		(prox-custo 9999)
		(profundidade (length caminho))
	)

	(if (>= (length caminho) n-nos)
		(return-from depth-aux (append caminho (list 0))))

	;TODO: preciso retirar nos percorridos
	(loop for i from 0 to (1- n-nos)
	do (if (not (member i caminho))
		(if (< (+ (aref problema no-actual i) (aref depth-visits-array i profundidade)) prox-custo)
			(progn (setf prox-no i) (setf prox-custo (+ (aref problema no-actual i) (aref depth-visits-array i profundidade))))
	)))

	;aumentar custo do proximo no
	; FIXME: nao esta no sitio certo
	(let ((custo-heur (aref depth-visits-array prox-no profundidade)))
	(setf (aref depth-visits-array prox-no profundidade) (* custo-heur 2)); 
	) 

	; fix: não sei se ordena bem a lista
	(depth-aux problema prox-no (append caminho (list no-actual)) (+ custo-actual prox-custo) depth-visits-array)
))

(defun dcusto(problema caminho)
	(let* ((l (length caminho))
		(custo 0))
	(if (and (eq (first (last caminho)) 0) (= 1 (length caminho)))
		(return-from custo 0)
		(loop while (>= (- l 2) 0)
			do (progn (setf custo (+ custo (aref problema (nth (- l 2) caminho)(nth (- l 1) caminho))))
									(decf l))))
			custo))
