; GRUPO   20
;   
; Rui Peres 	73831
; Duarte Pompeu 
;

(in-package :user)

(load "procura")
(load "atsp-problems")

; (load (compile-file "G020.lisp")) (atsp *min* "prof")
; (load (compile-file "G020.lisp")) (atsp *min* "a*.best.heuristic")

; directo da consola:
; clisp -x '(load (compile-file "G020.lisp")) (atsp *min* "prof")'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; O QUE ESTAVA A FAZER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; heuristica com problemas 
; usar os 5 min para ir aperfeiçoando a heuristica 
; custo do caminho ou da soma dos caminhos 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; O QUE ESTAVA A FAZER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *dim* nil) 
(defvar *medias* '())
(defvar *custoTotal* 0)

(defstruct atsp-estado
	problema 
	caminho
	)

(defun atsp-estado-inicial (board)
;	(print "inicial")
	(setq *dim* (array-dimension board 0))
	;(print *dim*)
	(setq *medias* (calcula-medias board))
	;(print *medias*)
	(setq *custoTotal* (apply #'+ *medias*))
	;(setq *custoTotal* (+ *custoTotal* (first (multiple-value-list (ceiling (/ *custoTotal* *dim*))))))
	;(print *custoTotal*)
	(let ((ini
      (make-atsp-estado :problema board
                        :caminho '(0))))
;		(print "fim inicial")
    ini))

(defun extract-row (2d-matrix row-index)
;	(print "extract-row")
     (let* ((n-columns (nth 1 (array-dimensions 2d-matrix)))
         (row-vector (make-array n-columns)))
    (dotimes (i n-columns row-vector)
      (setf (aref row-vector i) (aref 2d-matrix row-index i)))))


(defun calcula-medias(board)
	;	(print "calcula-medias")
	(let ((listayy '())
		(listaxx '())
		(cont 0)
		(maxEle (reduce #'max (extract-row board 0))))
	(dotimes (x *dim*)
		(dotimes (y *dim*)
			(setf cont (+ cont (aref board x y))))
		(setf cont (- cont maxEle))
		(push cont listayy)
		(setf cont 0))
	(setf listayy (reverse listayy))
	(setf listayy (mapcar #'(lambda (x) (first (multiple-value-list (ceiling (/ x (- *dim* 1)))))) listayy))

	(dotimes (y *dim*)
		(dotimes (x *dim*)
			(setf cont (+ cont (aref board x y))))
		(setf cont (- cont maxEle))
		(push cont listaxx)
		(setf cont 0))
	(setf listaxx (reverse listaxx))

	(setf listaxx (mapcar #'(lambda (x) (first (multiple-value-list (ceiling (/ x (- *dim* 1)))))) listaxx))

	(setf listaxx (mapcar #'(lambda (x y) (first (multiple-value-list (ceiling (/ (+ x y) 2))))) listaxx listayy))
;	(print "fim calcula-medias")
	listaxx))

; nao verifica estados repetidos no caminho porque funcao operadores nao permite transicoes para estados ja repetidos
(defun atsp-objectivo? (estado)
;		(print "objectivo")
	;	(print (atsp-estado-caminho estado))
	(let ((caminho (atsp-estado-caminho estado)))
		
		(if (and (eq (1+ *dim*) (length (atsp-estado-caminho estado)))
			(eq (first caminho) (first (last caminho))))
		(return-from atsp-objectivo? t)
		(return-from atsp-objectivo? nil))))

(defun atsp-operador (estado)
	;(print "********* operador ********")
	(let ((caminho (atsp-estado-caminho estado))
		(prob (atsp-estado-problema estado))
		(estados nil)
		novo-caminho)
	(if (eq (length caminho) *dim*)
		(progn (setf novo-caminho (nconc (copy-list caminho) (list (first caminho))))
			(push (make-atsp-estado  :problema prob
                            		 :caminho novo-caminho) estados)
			;(print novo-caminho)
			)
		(progn (dotimes (x *dim*)
			(when (not (numberp (find x caminho :test #'equal)))
				(setf novo-caminho (nconc (copy-list caminho) (list x)))
				(push (make-atsp-estado  :problema prob
	                               :caminho novo-caminho)        
	             estados)
				;(print novo-caminho)
				))))
	(values (reverse estados))))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;         Funcao Custo 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun custo(estado)
	(let* ((caminho (atsp-estado-caminho estado))
		(l (length caminho))
		(custo 0))
	(if (and (eq (first (last caminho)) 0) (= 1 (length caminho)))
		(return-from custo 0)
		(loop while (>= (- l 2) 0)
			do (progn (setf custo (+ custo (aref (atsp-estado-problema estado) (nth (- l 2) caminho)(nth (- l 1) caminho))))
									(decf l))))
			custo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun atsp (problema estrategia)
  (let* ((solucao nil)
    (prob (cria-problema (atsp-estado-inicial problema) 
                          (list #'atsp-operador)
                          :objectivo? #'atsp-objectivo? 
                          :estado= #'equal))
    (prob_h (cria-problema (atsp-estado-inicial problema) 
                          (list #'atsp-operador)
                          :objectivo? #'atsp-objectivo? 
                          :estado= #'equal
                          :custo #'custo 
                          :heuristica #'best-h))
    (prob_2h (cria-problema (atsp-estado-inicial problema) 
                          (list #'atsp-operador)
                          :objectivo? #'atsp-objectivo?
                          :estado= #'equal
                          :custo #'custo
                          :heuristica #'alter-h))
    output
    )

    (cond ((string= estrategia "a*.best.heuristic")
         (setf solucao (procura prob_h "a*" :espaco-em-arvore? T)))
      ((string= estrategia "a*.best.alternative.heuristic")
        (setf solucao (procura prob_2h "a*" :espaco-em-arvore? T)))
    ;  ((string= estrategia "iterative.sampling")
    ;    (setf solucao (minha-procura prob_h "ilds" :espaco-em-arvore? T)))

  ;   ((string= estrategia "best.approach")
   ;     (setf solucao (minha-procura prob "best.approach" :espaco-em-arvore? T)))
;			APAGAR ***********
;			APAGAR ***********
			((string= estrategia "prof")
        (setf solucao (procura prob "profundidade")))
;			APAGAR ***********
;			APAGAR ***********


      (t nil))      

      (if (equal solucao nil)
       solucao
       (progn 
       	(setf solucao (first solucao))
       	 (dotimes (x (length solucao))
        	 (push (atsp-estado-caminho (nth x solucao)) output))
       	 (setf solucao output)))
   (reverse solucao)))
