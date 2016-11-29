; each time you visit (node number, visit), increase heuristic cost
(defvar *depth-visits* (make-array '(20 20)))

(defun r-depth (problema no-inicial)
	;inicializacao
	;depth-visits inicial = custos do problema
	(let* ((n (first (array-dimensions problema))))
	(loop for i from 0 to (1- n) do
		(loop for j from 0 to (1- n) do
			(setf (aref *depth-visits* i j) 1)))
	)
	
	;chamada a func recursiva
	(loop for i from 0 to 100
	do (let* ((caminho (depth-aux problema no-inicial '() 0)))
		;~ (print caminho)
		(print (dcusto problema caminho))
		caminho))
	
	;~ (print *depth-visits*)
)

(defun depth-aux (problema no-actual caminho custo-actual)
; profundidade: tamanho do caminho
	(let* ((n-nos (array-dimension problema 0))
		(prox-no 0)
		(prox-custo 9999)
		(profundidade (length caminho))
	)
	
	(if (>= (length caminho) n-nos)
		(return-from depth-aux caminho))
	
	;TODO: preciso retirar nos percorridos	
	(loop for i from 0 to (1- n-nos)
	do (if (not (member i caminho))
		(if (< (+ (aref problema no-actual i) (aref *depth-visits* no-actual profundidade)) prox-custo)
			(progn (setf prox-no i) (setf prox-custo (aref problema no-actual i))))
	))
	
	;aumentar custo do proximo no
	; FIXME: nao esta no sitio certo
	(let ((custo-heur (aref *depth-visits* prox-no profundidade)))
	(setf (aref *depth-visits* prox-no profundidade) (* custo-heur 1.1))
	)
	
	; fix: não sei se ordena bem a lista
	(depth-aux problema prox-no (append caminho (list no-actual)) (+ custo-actual prox-custo))
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
