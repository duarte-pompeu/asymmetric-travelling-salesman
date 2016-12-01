; GRUPO   20
;   
; Rui Peres 	73831
; Duarte Pompeu 
;

(in-package :user)

(load "procura")
(load "atsp-problems")
(load "heuristics")
(load "depth.lisp")

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
    (prob_dists (cria-problema (atsp-estado-inicial problema) 
                          (list #'atsp-operador)
                          :objectivo? #'atsp-objectivo?
                          :estado= #'equal
                          :custo #'custo
                          :heuristica #'distances-h))                          
    (prob-greedy (cria-problema (atsp-estado-inicial problema) 
                          (list #'atsp-operador)
                          :objectivo? #'atsp-objectivo?
                          :estado= #'equal
                          :custo #'custo
                          :heuristica #'greedy-h))                          
    (prob-media-distancias (cria-problema (atsp-estado-inicial problema) 
                          (list #'atsp-operador)
                          :objectivo? #'atsp-objectivo?
                          :estado= #'equal
                          :custo #'custo
                          :heuristica #'media-distancias-h))                          
    output
    )

    (cond ((string= estrategia "a*.best.heuristic")
         (setf solucao (procura prob_h "a*" :espaco-em-arvore? T)))
      ((string= estrategia "a*.best.alternative.heuristic")
        (setf solucao (procura prob_2h "a*" :espaco-em-arvore? T)))
    ;  ((string= estrategia "iterative.sampling")
    ;    (setf solucao (minha-procura prob_h "ilds" :espaco-em-arvore? T)))
      ((string= estrategia "a*.distances")
        (setf solucao (procura prob_dists "a*" :espaco-em-arvore? T)))
      ((string= estrategia "a*.media-distancias")
		(init-media-distancias (atsp-estado-inicial problema))
        (setf solucao (procura prob-media-distancias "a*" :espaco-em-arvore? T)))
      ((string= estrategia "a*.greedy")
		(init-greedy-h (atsp-estado-inicial problema))
        (setf solucao (procura prob-greedy "a*" :espaco-em-arvore? T)))
      ((string= estrategia "r-depth")
        (setf solucao (r-depth problema 0)))
    

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

; comment debug body before deployment to remove prints
(defun debug (thing description)
	;~ (print description)
	;~ (print thing)
	;~ (print "---")
)
