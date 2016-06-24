;GRUPO 39
;----------------------------
;Goncalo Rodrigues 78958
;Telma Correia 78572
;Joao Coelho 77983
;----------------------------
;;; definicao das configuracoes possiveis para cada peca
;;peca i 
(defconstant peca-i0 (make-array (list 4 1) :initial-element T))
(defconstant peca-i1 (make-array (list 1 4) :initial-element T))
;;peca l
(defconstant peca-l0 (make-array (list 3 2) :initial-contents '((T T)(T nil)(T nil))))
(defconstant peca-l1 (make-array (list 2 3) :initial-contents '((T nil nil)(T T T))))
(defconstant peca-l2 (make-array (list 3 2) :initial-contents '((nil T)(nil T)(T T))))
(defconstant peca-l3 (make-array (list 2 3) :initial-contents '((T T T)(nil nil T))))
;;peca j
(defconstant peca-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T))))
(defconstant peca-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil))))
(defconstant peca-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T))))
(defconstant peca-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T))))
;;peca o
(defconstant peca-o0 (make-array (list 2 2) :initial-element T))
;;peca s
(defconstant peca-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T))))
(defconstant peca-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil))))
;;peca z
(defconstant peca-z0 (make-array (list 2 3) :initial-contents '((nil T T)(T T nil))))
(defconstant peca-z1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(nil T))))
;;peca t
(defconstant peca-t0 (make-array (list 2 3) :initial-contents '((T T T)(nil T nil))))
(defconstant peca-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil))))
(defconstant peca-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T))))
(defconstant peca-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))



(defconstant peca-i (make-array 2 ))
(setf (aref peca-i 0) peca-i0)
(setf (aref peca-i 1) peca-i1)
(defconstant peca-l (make-array 4 ))
(setf (aref peca-l 0) peca-l0)
(setf (aref peca-l 1) peca-l1)
(setf (aref peca-l 2) peca-l2)
(setf (aref peca-l 3) peca-l3)
(defconstant peca-j (make-array 4 ))
(setf (aref peca-j 0) peca-j0)
(setf (aref peca-j 1) peca-j1)
(setf (aref peca-j 2) peca-j2)
(setf (aref peca-j 3) peca-j3)
(defconstant peca-o (make-array 1 ))
(setf (aref peca-o 0) peca-o0)
(defconstant peca-s (make-array 2 ))
(setf (aref peca-s 0) peca-s0)
(setf (aref peca-s 1) peca-s1)
(defconstant peca-z (make-array 2 ))
(setf (aref peca-z 0) peca-z0)
(setf (aref peca-z 1) peca-z1)
(defconstant peca-t (make-array 4 ))
(setf (aref peca-t 0) peca-t0)
(setf (aref peca-t 1) peca-t1)
(setf (aref peca-t 2) peca-t2)
(setf (aref peca-t 3) peca-t3)

;;;TIPOS ABSTRACTOS DE DADOS

;;TIPO ACCAO
;cria-accao: inteiro x array -> accao
(defun cria-accao (x l)
  (cons x l)
)

;accao-coluna: accao -> inteiro
(defun accao-coluna (accao)

	(car accao)
)

;accao-peca: accao -> array
(defun accao-peca (accao)

	(cdr accao)
)

;;TIPO TABULEIRO
;cria-tabuleiro: {} -> tabuleiro
(defun cria-tabuleiro ()
	(make-array '(18 10) :initial-element NIL)
)
(defun is-tabuleiro-p (tabuleiro)
  (and (equal (array-dimension tabuleiro 0) 18) (equal (array-dimension tabuleiro 1) 10))
)
;copia-tabuleiro: tabuleiro -> tabuleiro
(defun copia-tabuleiro (tabuleiro) 
  

	(let ((result (cria-tabuleiro)) (cont T))
   (dotimes (i 18 result)
     (if (not cont) (return result))
     (setf cont NIL)
     (dotimes (j 10 result)
       (setf cont (or (setf (aref result i j) (aref tabuleiro i j)) cont)))
     )
   )
    

  )


;tabuleiro-preenchido-p: tabuleiro x inteiro x inteiro -> logico
(defun tabuleiro-preenchido-p (tabuleiro i j)

    (aref tabuleiro i j)
    
  )

;tabuleiro-linha-completa-p: tabuleiro x inteiro -> logico
(defun tabuleiro-linha-completa-p (tabuleiro l) 

      (let ((bool T))
        (dotimes (c 10 bool)
          (if (not (tabuleiro-preenchido-p tabuleiro l c))
              (setf bool NIL)))
        )
    
)

;tabuleiro-preenche!: tabuleiro x inteiro x inteiro -> {}
(defun tabuleiro-preenche! (tabuleiro l c)
  (if (or (> l 17) (< l 0) (> c 9) (< c 0)) 'IGNORE
      (setf (aref tabuleiro l c) T))
    
)

;array->tabuleiro: array -> tabuleiro
(defun array->tabuleiro (l)
  (copia-tabuleiro l)
)

;tabuleiro->array: tabuleiro -> array
(defun tabuleiro->array (tabuleiro)
  (copia-tabuleiro tabuleiro)
  )

;tabuleiros-iguais-p: tabuleiro x tabuleiro -> logico
(defun tabuleiros-iguais-p (t1 t2)
  (equalp t1 t2)
  )

;tabuleiro-topo-preenchido-p: tabuleiro -> logico
(defun tabuleiro-topo-preenchido-p (tabuleiro)
  (let ((result NIL))
    (dotimes (i 10 result)
      (if (tabuleiro-preenchido-p tabuleiro 17 i) (return T))
    ))
  )

(defstruct (estado) (pontos 0) (pecas-por-colocar NIL) (pecas-colocadas NIL) (tabuleiro (cria-tabuleiro)))
  
 

(defun copia-estado (e)
    (let
      ((novo (make-estado)))
    (setf (estado-pontos novo) (estado-pontos e)
      (estado-pecas-por-colocar novo) (copy-list (estado-pecas-por-colocar e))
      (estado-pecas-colocadas novo) (copy-list (estado-pecas-colocadas e))
      (estado-tabuleiro novo) (copia-tabuleiro (estado-tabuleiro e)))
     novo
    )
)

(defun estados-iguais-p (estado1 estado2) 
  (and (equal (estado-pontos estado1) (estado-pontos estado2))
       (equal (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))
       (equal (estado-pecas-colocadas estado1) (estado-pecas-colocadas estado2))
       (tabuleiros-iguais-p (estado-tabuleiro estado1) (estado-tabuleiro estado2)))
)

(defun estado-final-p (e)
  (or (null (estado-pecas-por-colocar e))
      (tabuleiro-topo-preenchido-p (estado-tabuleiro e)))
  )


(defun tabuleiro-altura-coluna (tabuleiro coluna)
    (do ((linha 17 (decf linha)))
        ((or (< linha 0) (tabuleiro-preenchido-p tabuleiro linha coluna)) (+ linha 1)))
    
  )

(defun solucao (e)
  (and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro e)))
       (null (estado-pecas-por-colocar e))
       )
  )

(defun peca-array (str orientacao)
  (cond
   ((equal str 'i)
    (aref peca-i orientacao))
   ((equal str 's)
    (aref peca-s orientacao))
   ((equal str 'z)
    (aref peca-z orientacao))
   ((equal str 'l)
    (aref peca-l orientacao))
   ((equal str 't)
    (aref peca-t orientacao))
   ((equal str 'o)
    (aref peca-o orientacao))
   ((equal str 'j)
    (aref peca-j orientacao))
   )
)

(defun peca-orientacoes (peca)
    (cond
   ((equal peca 'i)
    2)
   ((equal peca 's)
    2)
   ((equal peca 'z)
    2)
   ((equal peca 'l)
    4)
   ((equal peca 't)
    4)
   ((equal peca 'o)
    1)
   ((equal peca 'j)
    4)
     (T 0))
  )

(defun accoes (e)
  (if (or (null (estado-pecas-por-colocar e)) (tabuleiro-topo-preenchido-p (estado-tabuleiro e))) NIL
  (let
      ((result (list))
       (peca (first (estado-pecas-por-colocar e)))
       (array NIL)
       (maxi NIL)
       (maxj NIL))
    
    (setf maxi (peca-orientacoes peca))
    (if (null peca) NIL
      (dotimes (i maxi result)
        (setf array (peca-array peca (- (- maxi 1) i)))
        (setf maxj (- 11 (array-dimension array 1)))
        (dotimes (j maxj result)
          (setf result (cons (cria-accao (- (- maxj 1) j) array) result))
          )
        ))
    ))
  )
        
(defun pontos (linhas)
  (cond
   ((equal linhas 0)
    0)
   ((equal linhas 1)
    100)
   ((equal linhas 2)
    300)
   ((equal linhas 3)
    500)
   ((equal linhas 4)
    800)
   )
  )

  
(defun custo-oportunidade-mal (e)
  (let
      ((peca NIL)
       (total 0))
    (dotimes (i (array-dimension (estado-pecas-colocadas e) 0) NIL)
      (setf peca (peca-array (aref (estado-pecas-colocadas e) i) 0))
      (dotimes (j (array-dimension peca 0) NIL)
        (dotimes (k (array-dimension peca 1) NIL)
          (if (aref peca j k)
              (setf total (1+ total)))
          )
        )
      )
    (- (* (/ total 40) (pontos 4)) (estado-pontos e))
    )
  )
(defun maximo-pontos (peca)
    (cond
   ((equal peca 'i)
    800)
   ((equal peca 's)
    300)
   ((equal peca 'z)
    300)
   ((equal peca 'l)
    500)
   ((equal peca 't)
    300)
   ((equal peca 'o)
    300)
   ((equal peca 'j)
    500)
     )
  )

(defun custo-oportunidade (e)
  (let
      ((total 0))
    (dotimes (i (list-length (estado-pecas-colocadas e)) NIL)
      (setf total (+ total (maximo-pontos (nth i (estado-pecas-colocadas e)))))
      )
    (- total (estado-pontos e))
    )
  )

;resultado: estado x accao -> estado
(defun resultado (est acc)
  
  (let* (    (est (copia-estado est)) (tabuleiro (estado-tabuleiro est)) (c (accao-coluna acc)) (l 0) 
         (peca (accao-peca acc)) (num_linhas (array-dimension peca 0)) (num_colunas (array-dimension peca 1)) (cont 0) )
    
    (dotimes (i num_linhas NIL) 
      (dotimes (j num_colunas NIL)
        (if (aref peca i j)
            (setf l (max l (- (tabuleiro-altura-coluna tabuleiro (+ j c)) i)))
          )
        )
      )
    (dotimes (i num_linhas NIL)
      (dotimes (j num_colunas NIL)
        (if (aref peca i j) (tabuleiro-preenche! tabuleiro (+ i l) (+ j c)))))
    

 
         
    (if (not (tabuleiro-topo-preenchido-p tabuleiro))
        (dotimes (i num_linhas NIL)
          (when (tabuleiro-linha-completa-p tabuleiro (+ i l))
            (tabuleiro-remove-linha! tabuleiro (+ i l))
            (incf cont)
            (decf i))
          )
      )
    (setf (estado-pecas-colocadas est) (cons (first (estado-pecas-por-colocar est)) (estado-pecas-colocadas est)))
    (setf (estado-pecas-por-colocar est) (rest (estado-pecas-por-colocar est)))
    
    (setf (estado-pontos est) (+ (estado-pontos est) (pontos cont)))
    est
    )
  )

;tabuleiro-remove-linha!: tabuleiro x inteiro -> {}
(defun tabuleiro-remove-linha! (tabuleiro int)
  (do ( (a 0 (dotimes (total 10)
      (setf (aref tabuleiro linha total) (aref tabuleiro (+ 1 linha) total))
               ))
       (linha int (incf linha))
   )
      ((> linha 16) a))
  
      (dotimes (fim 10)
        (setf (aref tabuleiro 17 fim) NIL)) 
)
;qualidade: estado -> inteiro
(defun qualidade (estado)
  (* -1 (estado-pontos estado))
  )
(defstruct heap
  (comparision #'<)
  (key-func #'(lambda (x) x))
  (insertions 0)
  (removals 0)
  (size 0)
  (array-size 128)
  (array NIL))


;create-heap optional function x optional function -> heap
;creates a new heap with a priority and key functions
(defun create-heap (&key (comparision #'<) (key #'(lambda (x) x)))
  (let ((h NIL))
    (setf h (make-heap :key-func key :comparision comparision :insertions 0 :removals 0 :size 0))
    (setf (heap-array h) (make-array (heap-array-size h) :adjustable T))
    h
    )
  )

(defun leftchild (i)
  (+ (* 2 i) 1)
  )

(defun rightchild (i)
  (+ (* 2 i) 2)
  )

(defun parentnode (i)
  (floor (/ (- i 1) 2))
  )

(defun switch (heap nodeA nodeB)
  (psetf (aref (heap-array heap) nodeA) (aref (heap-array heap)  nodeB) 
    (aref (heap-array heap)  nodeB) (aref (heap-array heap)  nodeA))
  nodeB
  )

(defun switchUp (heap node)
  (switch heap node (parentnode node))
  (parentnode node)
  )

(defun content (heap node)
  (car (aref (heap-array heap) node))
  )


(defun key (heap node)
  (cdr (aref (heap-array heap) node))
  )
(defun max-key (heap)
  (cdr (aref (heap-array heap) 0))
  )
;push-heap: heap x item -> void
;inserts an item in the heap
(defun push-heap (heap item)

  (if (>= (heap-size heap) (heap-array-size heap))
      (progn
        (setf (heap-array-size heap) (* 2 (heap-array-size heap)))
        (adjust-array (heap-array heap) (heap-array-size heap))
        (push-heap heap item)
        )
    (let ((i (heap-size heap)))
      ;updates size
      (incf (heap-size heap))
      ;calculate item's key and insert at the end
      (setf (aref (heap-array heap) i) (cons item (funcall (heap-key-func heap) item)))
      ;rebalance heap
      (loop
        (when (not (and (> i 0) (funcall (heap-comparision heap) (key heap i) (key heap (parentnode i))))
) (return))
        (setf i (switchUp heap i))
        )
      (incf (heap-insertions heap))

      )
    )
  )
;pop-heap: heap -> item
;gets the item with highest priority
(defun pop-heap (heap)
  (if (> (heap-size heap) 0) 
      

      (let ((i 0) (left 0) (right 0) (not_finished T) (biggest 0) (biggest_val 0))
        (decf (heap-size heap))
        (switch heap 0 (heap-size heap)) ;troca primeiro com ultimo
        (setf left (leftchild i))
        (setf right (rightchild i))
        (loop (when (not (and (< left (heap-size heap)) not_finished)) (return))
          ;if there is only a left branch
          (if (>= right (heap-size heap))
              (setf right left))
          
          ;choose the biggest (max left right)
          (if (funcall (heap-comparision heap) (key heap left) (key heap right))
              (setf biggest left biggest_val (key heap left))
            (setf biggest right biggest_val (key heap right)))
          
          ;check current against biggest (if smaller, switch them up)
          (cond ((not (funcall (heap-comparision heap) (key heap i) biggest_val))
                 (setf i (switch heap i biggest)))
                (T (setf not_finished NIL))
                )
          (setf left (leftchild i) right (rightchild i))
          )
        (incf (heap-removals heap))
        (content heap (heap-size heap))
        )
    NIL
    )
  )

;WARNING! DESTRUCTS HEAP! USE WITH CAUTION!
(defun heap_to_list (heap)
  (let
      ((result NIL))
    (dotimes (i (heap-size heap) result)
      (setf result (cons (pop-heap heap) result)))))

;WARNING! DESTRUCTS HEAP! USE WITH CAUTION!
(defun heap_to_array (heap)
  (let
      ((result (make-array (heap-size heap))))
    (dotimes (i (heap-size heap) result)
      (setf (aref result i) (pop-heap heap)))))


;estrutura problema
(defstruct (problema)
   (estado-inicial (make-estado))
   (solucao #'solucao)
   (accoes #'accoes)
   (resultado #'resultado)
   (custo-caminho #'qualidade)
  )

;index -> indice na lista de nos expandidos
;from -> indice do no pai
;action -> accao tomada para chegar a este no
(defstruct (node)
  item
  index
  from
  action)

(defun procura-A* (problema heuristica &key (maximonos 10000))
  (let
      ((objectivo-p (problema-solucao problema)) 
       (estado-atual (make-node :item (problema-estado-inicial problema) :from NIL :action NIL :index 0))
       (ultimo-expandido 0)
       (ultimo-gerado 0)
       (vizinhos (problema-accoes problema))
       (nos-visitados NIL)
       (lista-accoes NIL)
       (execucao (problema-resultado problema))
       (fila (create-heap :key #'(lambda (node)
                                   (cons (+ (funcall (problema-custo-caminho problema) (node-item node))
                                            (funcall heuristica (node-item node)))
                                         (node-index node)))
                          :comparision #'(lambda (key1 key2) ;funcao de prioridade ;f(n)=g(n)+h(n)
                                           (or 
                                            (< (car key1) (car key2))
                                            (and (= (car key1) (car key2)) (> (cdr key1) (cdr key2))))
                                           )
                              )))

    ;enquanto nao encontra o objectivo
    (loop (if (or (funcall objectivo-p (node-item estado-atual))
                (> (heap-insertions fila) maximonos)) (return))
      (setf nos-visitados (cons estado-atual nos-visitados))
      
      ;geracao de accoes
      (setf lista-accoes (funcall vizinhos (node-item estado-atual)))
      
      ;coloca vizinhos na fila
      (loop (if (null lista-accoes)(return))
        ;executa accao e coloca na fila
        (incf ultimo-gerado)
        (push-heap fila (make-node :item (funcall execucao (node-item estado-atual) (first lista-accoes))
                                   :index ultimo-gerado
                                   :from ultimo-expandido
                                   :action (first lista-accoes)))
        
        (setf lista-accoes (rest lista-accoes)))
      
      (incf ultimo-expandido)
      ;escolhe proximo no a expandir
      (if (= (heap-size fila) 0) (return))
      (setf estado-atual (pop-heap fila))
      )
    ;ultimo no a ser expandido
    (format t "Nos gerados:~$" (heap-insertions fila))
    (format t "Nos expandidos:~$~%" (heap-removals fila))
    (setf nos-visitados (cons estado-atual nos-visitados))
    ;reconstrucao de caminho
    (setf lista-accoes NIL)
    (loop (if (null (node-from estado-atual))(return))
      (setf lista-accoes (cons (node-action estado-atual) lista-accoes))
      (setf estado-atual (nth (- ultimo-expandido (node-from estado-atual)) nos-visitados))
      ) 
    lista-accoes
    )
  )  


(defun procura-pp (problema)
    (let ((estado-atual (list (problema-estado-inicial problema) NIL NIL))
          (vizinhos (problema-accoes problema))
          (stack NIL)
          (lista-accoes NIL)
        (caminho NIL)
          (objectivo-p (problema-solucao problema)))
      
      ;enquanto nao for objectivo
      (loop (when (funcall objectivo-p (car estado-atual)) (return))
        (setf lista-accoes (funcall vizinhos (car estado-atual)))
        
        ;poe uma lista na pilha que contem uma accao, o estado resultante dessa accao e o estado anterior
        (loop (when (null lista-accoes) (return))
          (push (list (funcall (problema-resultado problema) (car estado-atual) (first lista-accoes))
                      estado-atual
                      (pop lista-accoes)) 
                stack)
          )
        
        ;proximo elemento a sair da pilha
        (setf estado-atual (pop stack))
        )
      ;calculo do caminho
      (push (nth 2 estado-atual) caminho)
      (loop (when (null (nth 2 estado-atual)) (return))
        (push (nth 2 estado-atual) caminho)
        )
        (setf estado-atual (nth 1 estado-atual))
      caminho
      )
  )

 ;procura-best: array x lista pecas -> lista accoes

  (defun procura-best (array_t lista_pecas)
	
	(let* ( (num_linhas (array-dimension array_t 0)) (num_colunas (array-dimension array_t 1)) (tabuleiro (make-tabuleiro)) 
			(estado (make-estado)) (problema (make-problema)) (nr_linhas 0) (heuristica 0))
	;preenche o tabuleiro com as pecas iniciais
	(dotimes (i num_linhas NIL)
		(dotimes (j num_colunas NIL)
          (setf tabuleiro (tabuleiro-preenche! tabuleiro (aref array_t i) (aref array_t j)))
		  (if (tabuleiro-preenchido-p tabuleiro) (incf(nr_linhas))) ;calcular nr linhas preenchidas
		)
    )
	
	;inicializa o estado
	(setf (estado-pontos estado) (pontos nr_linhas)
      (estado-pecas-por-colocar estado) lista_pecas
      (estado-pecas-colocadas estado) array_t
      (estado-tabuleiro estado) (copia-tabuleiro (tabuleiro))
	)
	

	;procura: A*
	;heuristica ??
	(procura-A* problema heuristica)
	
	
	;funcao custo-qualidade : #'qualidade
	;obter a sequencia de accoes de modo a conseguir colocar todas as pecas no tabueleiro com o max pontuacao
  ))



(defun maximo-pontos-estado (e)
  (if (= (list-length (estado-pecas-por-colocar e)) 0) 0
  (let
      ((total 0) (total_linhas 0) (total_score 0))
    (dotimes (linha 18)
      (dotimes (coluna 10)
        (if (tabuleiro-preenchido-p (estado-tabuleiro e) linha coluna)
            (incf total))
        )
      )
    (setf total (+ total (* 4 (list-length (estado-pecas-por-colocar e)))))
    (setf total_linhas (floor (/ total 10)))
    (setf total_score (* (pontos 4) (floor (/ total_linhas 4))))
    (setf total_linhas (mod total_linhas 4))
    (setf total_score (+ total_score (* (pontos 3) (floor (/ total_linhas 3)))))
    (setf total_linhas (mod total_linhas 3))
    (setf total_score (+ total_score (* (pontos 2) (floor (/ total_linhas 2)))))
    (setf total_linhas (mod total_linhas 2))
    (setf total_score (+ total_score (* (pontos 1) (floor (/ total_linhas 1)))))
    (* -2 total_score)
    ))
  )

(defun maximo-pontos-sem-tabuleiro (e)
  (let ((total 0) (total_linhas 0) (total_score 0))
      (setf total (+ total (* 4 (list-length (estado-pecas-por-colocar e)))))
    (setf total_linhas (floor (/ total 10)))
    (setf total_score (* (pontos 4) (floor (/ total_linhas 4))))
    (setf total_linhas (mod total_linhas 4))
    (setf total_score (+ total_score (* (pontos 3) (floor (/ total_linhas 3)))))
    (setf total_linhas (mod total_linhas 3))
    (setf total_score (+ total_score (* (pontos 2) (floor (/ total_linhas 2)))))
    (setf total_linhas (mod total_linhas 2))
    (setf total_score (+ total_score (* (pontos 1) (floor (/ total_linhas 1)))))
    (* -1 total_score))
  )
    

(defun procura-e-guarda (procura problema &optional (heuristica NIL))
  (let
      ((lista-accoes NIL) (current (problema-estado-inicial problema))
       (boards "[ [") (heuristics "[") (scores "["))
    (if (null heuristica)     (setf lista-accoes (funcall procura problema ))
      (setf lista-accoes (print (time (funcall procura problema heuristica)))))
    (if (null heuristica) (setf heuristica #'(lambda (e) 0)))
        (dotimes (i 18)
          (setf boards (concatenate 'string boards " [" ))
            (dotimes (j 10)
              (setf boards (concatenate 'string boards (write-to-string (aref (estado-tabuleiro current) i j))))
              (if (< j 9)
                  (setf boards (concatenate 'string boards " ," )))
              )
            (setf boards (concatenate 'string boards "] " ))
            (if (< i 17)
                (setf boards (concatenate 'string boards " ," ))
              (setf boards (concatenate 'string boards " ]" ))))
      
      (setf scores (concatenate 'string scores "0"))
      (setf heuristics (concatenate 'string heuristics (write-to-string (funcall heuristica current))))

      (loop (if (null lista-accoes)(return))
        (setf current (funcall (problema-resultado problema) current (first lista-accoes)))
        (setf boards (concatenate 'string boards ", ["))
        (dotimes (i 18)
          (setf boards (concatenate 'string boards " [" ))
            (dotimes (j 10)
              (setf boards (concatenate 'string boards (write-to-string (aref (estado-tabuleiro current) i j))))
              (if (< j 9)
                  (setf boards (concatenate 'string boards " ," )))
              )
            (setf boards (concatenate 'string boards "] " ))
            (if (< i 17)
                (setf boards (concatenate 'string boards " ," ))
              (setf boards (concatenate 'string boards " ]" ))))
                
        (setf scores (concatenate 'string scores ", " (write-to-string (estado-pontos current))))
        (setf heuristics (concatenate 'string heuristics ", " (write-to-string (funcall heuristica current))))
        (pop lista-accoes)
        )
      
      (setf boards (concatenate 'string boards "]"))
      (setf scores (concatenate 'string scores "]"))
      (setf heuristics (concatenate 'string heuristics "]"))
      (format t "~$" (concatenate 'string boards ", " scores ", " heuristics ", "))
      )
    )
  
  (defun heuristica-altura-tabuleiro-media (estado)
  
  ;media altura das colunas
  (let ((num_colunas 10) (soma 0) (media 0) (pontos (maximo-pontos-estado estado)))
	  (dotimes (i num_colunas NIL)
			  (setf soma (+ soma (tabuleiro-altura-coluna (estado-tabuleiro estado) i)))
		)
	(setf media (/ soma num_colunas))
	
	;funcao do gongas devolve max pontos
	(setf pontos (- pontos  (* 100 media)) )
   )

)


(defun heuristica-bumpiness (e)
  (if (= (list-length (estado-pecas-por-colocar e)) 0) 0
  (let
      ((total 0) (anterior 0))
      (dotimes (coluna 10)
        (psetf anterior (tabuleiro-altura-coluna (estado-tabuleiro e) coluna)
          total (+ total (abs (- (tabuleiro-altura-coluna (estado-tabuleiro e) coluna) anterior))))
        )
      
     (* 10 total)
    ))
  )

(defun heuristica-altura-tabuleiro-maxima (estado)
  (if ( = (list-length (estado-pecas-por-colocar estado)) 0) 0
    ;maxima altura das colunas
    (let ((num_colunas 10) (maximo (tabuleiro-altura-coluna (estado-tabuleiro estado) 0)) (pontos (maximo-pontos-estado estado)) )
      (dotimes (i num_colunas NIL)
        (if (> (tabuleiro-altura-coluna (estado-tabuleiro estado) i) maximo) 
            (setf maximo (tabuleiro-altura-coluna (estado-tabuleiro estado) i))
          )
        )
      
        maximo
      ))
  )

(defun heuristica-altura-tabuleiro-maxima-quad (estado)
  (if ( = (list-length (estado-pecas-por-colocar estado)) 0) 0
    ;maxima altura das colunas
    (let ((num_colunas 10) (maximo (tabuleiro-altura-coluna (estado-tabuleiro estado) 0)) (pontos (maximo-pontos-estado estado)) )
      (dotimes (i num_colunas NIL)
        (if (> (tabuleiro-altura-coluna (estado-tabuleiro estado) i) maximo) 
            (setf maximo (tabuleiro-altura-coluna (estado-tabuleiro estado) i))
          )
        )
      

          (* 5 maximo maximo)
      ))
  )
	
				  
(defun heuristica-linhas-parciais (estado) 
  (if ( = (list-length (estado-pecas-por-colocar estado)) 0) 0
	(let ((num_colunas 10) (num_linhas 17) (decay 0.3) (pecas (list-length (estado-pecas-por-colocar estado))) (blocos_preenchidos 0) (linhas_quase_preenchidas 0) (pontos (maximo-pontos-sem-tabuleiro estado)) )
   (dotimes (i num_linhas NIL)
     (setf decay 1)
			(dotimes (j num_colunas NIL)
     (if (tabuleiro-preenchido-p (estado-tabuleiro estado) i j) 
         (progn
         (if (not (tabuleiro-preenchido-p (estado-tabuleiro estado) i j)) (setf decay 0.4))
         (setf blocos_preenchidos (+ blocos_preenchidos decay)))
         (if  (tabuleiro-preenchido-p (estado-tabuleiro estado) (1+ i) j)
             (progn
               (setf blocos_preenchidos 0)
               (return))
           )
       
       )
		    )
			    (if (>= blocos_preenchidos 5) (setf pecas (- pecas (/ (- 10 blocos_preenchidos) 4))))
    (if (<= pecas 0) (return))
    (cond
          ((equal blocos_preenchidos 5) (setf linhas_quase_preenchidas (+ linhas_quase_preenchidas 0.5)))
     ((equal blocos_preenchidos 6) (setf linhas_quase_preenchidas (+ linhas_quase_preenchidas 0.75)))
			   ((equal blocos_preenchidos 7) (incf linhas_quase_preenchidas))
			   ((equal blocos_preenchidos 8) (setf linhas_quase_preenchidas (+ linhas_quase_preenchidas 1.25)))
			   ((equal blocos_preenchidos 9) (setf linhas_quase_preenchidas (+ linhas_quase_preenchidas 1.5)))
    )

			(setf blocos_preenchidos 0)
		)
		(* -150 linhas_quase_preenchidas)
	)))


(defun heuristica-buracos (estado)
  (if ( = (list-length (estado-pecas-por-colocar estado)) 0) 0
    (let ((tab (estado-tabuleiro estado)) (buracos 0) (temp 1))
      (dotimes (linha 17)
      (dotimes (coluna 10)
        (if 
            (and 
             (not
              (tabuleiro-preenchido-p tab linha coluna)) 
             (tabuleiro-preenchido-p tab (+ linha 1) coluna))
            (progn
              (incf buracos)
              (setf temp 1)
              (loop 
                (if 
                  (or (< (- linha temp) 0) (tabuleiro-preenchido-p tab (- linha temp) coluna))
                  (return))
                (progn
                  (incf buracos)
                  (incf temp)
                  )
                )
                )
          )
        )
      )
      (* buracos 20)
      )
    )
  )

(defun procura-previsao (problema heuristica &key (maximonos 10000))
  (let
      ((caminho NIL) (lista-accoes NIL))
    (loop
      (if ( = (list-length (estado-pecas-por-colocar (problema-estado-inicial problema))) 0) (return))
      (setf lista-accoes (procura-a* problema heuristica :maximonos maximonos))
      (if (null lista-accoes) (return))
      (if (= (list-length (estado-pecas-por-colocar (problema-estado-inicial problema)))
             (list-length lista-accoes)) (return))
      (setf caminho (cons (pop lista-accoes) caminho))
      (setf (problema-estado-inicial problema) (funcall (problema-resultado problema)
                 (problema-estado-inicial problema)
                 (first caminho)))
      )
    (append (reverse caminho) lista-accoes)
    )
  )

(setf *random-state* (make-random-state t))

;crossover:
;33% chance to get a gene from individual A
;33% chance from B
;33% chance to get a randomly weighted average from A and B

(defun crossover (ind_a ind_b)
  (let ((result (make-list (list-length ind_a))))
    (dotimes (i (list-length ind_a) result)
      (if (eq (random 3) 0) 
          (setf (nth i result) (nth i ind_a))
        
        (if (eq (random 2) 0)
            (setf (nth i result) (nth i ind_b))
          (let 
              ((r (/ (random 1000) 1000)))
            (setf (nth i result) (+ (* r (nth i ind_a)) (* (- 1 r) (nth i ind_b))))
            )
          )
        )
      )
    )
  )

(defun mutation (genes)
  (setf (nth (random (list-length genes)) genes ) (- (/ (random 2000) 1000) 1))
  )

;procura - funcao de procura que recebe um estado inicial e uma funcao heuristica e devolve uma lista de accoes
;estado-inicial - estado inicial usado na procura
;solucao - funcao que ve se um estado e solucao
;resultado - funcao que diz o resultado de aplicar uma accao a um estado
;fitness - funcao de fitness
;heuristicas - lista de heuristicas a usar para quais queremos os coeficientes
;pop - populacao
;limit - numero maximo de geracoes
;mutation_chance - chance de mutacao

(defun get_genetic_coefficients (procura gerador-estados solucao resultado fitness heuristicas 
                                         &key (constant-h #'(lambda (e) 0)) (pop 100) (genes NIL) (generation 0) (limit 10) (mutation_chance 0.02)) 
     ;initialize genes as random numbers between -1 and 1
  (when (null genes)
      (setf genes (make-array pop :initial-element NIL))
      (dotimes (i pop) 
        (dotimes (j (list-length heuristicas))
          (setf (aref genes i) (cons (- ( / (random 2000) 1000) 1) (aref genes i)))
          )
        )
    )

  (let
      ((lista_accoes NIL) (estado-inicial (funcall gerador-estados)) (current NIL) (fitness_vals (make-array pop)) 
       (total_fitness 0) (num_h (list-length heuristicas)) (new_genes NIL))
    ;fitness_vals - array of lists: first member -> fitness value, second member -> individual ID (genes index)
    ;new_genes - array containing the genes of the new population
    ;num_h - number of heuristics
    ;total_fitness - total fitness
    
    (setf new_genes (make-array (list pop)))
    
    ;get fitness values for all population
    (dotimes (i pop) 
      (setf lista_accoes (funcall procura (make-problema :estado-inicial estado-inicial)
                                  #'(lambda (estado) ;linear function combining all heuristics
                                      (let ((resultado (funcall constant-h estado)))
                                        (dotimes (k num_h resultado)
                                          (setf resultado (+ resultado (* (nth k (aref genes i)) (funcall (nth k heuristicas) estado))))
                                          )
                                        )
                                      ))) ;search for the solution
      (setf current estado-inicial)
      (loop (if (not(and (not (null lista_accoes)) (not (funcall solucao current)))) (return)) ;get final state
        (setf current (funcall resultado current (first lista_accoes)))
        (setf lista_accoes (rest lista_accoes))
        )
      (setf (aref fitness_vals i) (list 0 i)) ;initialize fitness_vals
      (setf (first (aref fitness_vals i)) (funcall fitness current)) ;get fitness value for solution state
      (setf total_fitness (+ total_fitness (first (aref fitness_vals i))))
      (format t "FITNESS: ~$~%" (first (aref fitness_vals i)))
      
      )
    ;sort fitness values
    (setf fitness_vals (sort fitness_vals #'> :key #'(lambda (x)
                                         (first x)))) ;sort
    
    (format t "Best: ~$ with genes: ~a" (first (aref fitness_vals 0)) (aref genes (nth 1 (aref fitness_vals 0))))
    
    (if (= total_fitness 0) (progn
                              (setf total_fitness pop)
                                    (dotimes (i pop) 
        (setf (first (aref fitness_vals i)) 1)
        )))
                              
    (let ((cumulative_fitness 0)) ;get cumulative probabilities according to fitness value
      (dotimes (i pop) 
        (setf cumulative_fitness (+ cumulative_fitness (/ (first (aref fitness_vals i)) total_fitness)))
        (setf (first (aref fitness_vals i)) cumulative_fitness)
        )
      )
    
    
    ;select individuals for breeding :D and mutate them D:
    
    (dotimes (i pop)
      (let ((r1 (/ (random total_fitness) total_fitness)) (r2  (/ (random total_fitness) total_fitness)) 
             (m (/ (random 1000) 1000))(a 0) (b 0))
        (loop (if (not(and (< a (- pop 1)) (< (first (aref fitness_vals a)) r1))) (return)) ;get first individual
          (incf a))
        (loop (if(not(and (< b (- pop 1)) (< (first (aref fitness_vals b)) r2)))(return)) ;gets second invidivual
          (incf b))
        
        ;generates a new child from parent A and B
        (setf (aref new_genes i) (crossover (aref genes (nth 1 (aref fitness_vals a)))
                                            (aref genes (nth 1 (aref fitness_vals b)))))
        (if (< m mutation_chance)
            (mutation (aref new_genes i))
          )
        )
      )
    
    ;returns best solution if limit has been reached or goes to next generation
    (if (>= generation limit) (aref genes (nth 1 (aref fitness_vals 0)))
      (get_genetic_coefficients procura gerador-estados solucao resultado fitness heuristicas
                                :constant-h constant-h :pop pop :genes new_genes :generation (1+ generation) :limit limit)
      )
    )
    
  )


(defun random-element (list)
  (nth (random (length list)) list))

(defun random-pecas (n)
	(let ((lista-pecas nil))
		(dotimes (i n)
			(push (random-element (list 'o 'i 'j 'l 's 'z 't)) lista-pecas))
   lista-pecas))
(defun get-wells (estado)
  (let
       ((totalI (count 'i (estado-pecas-por-colocar estado)))(temp 0) (result 0) (before (tabuleiro-altura-coluna (estado-tabuleiro estado) 0 )))
    (dotimes (coluna 10)
      (setf temp (- before (tabuleiro-altura-coluna (estado-tabuleiro estado) coluna))
        before (tabuleiro-altura-coluna (estado-tabuleiro estado) coluna))
      (if (> (abs temp) 2)
          (setf result (+ result (floor (/ (abs temp) 3))))))
    (setf result (- result totalI))
    (if (> result 0)
        (* 800 (- result totalI))
      0)
    )
  )


(defstruct n-heap
  (comparision #'<)
  (key-func #'(lambda (x) x))
  (insertions 0)
  (removals 0)
  (size 0)
  (array-size 0)
  (array NIL))


;create-n-heap optional integer x function x optional function -> heap
;creates a new set of n heaps with a priority and key functions
(defun create-n-heap (n &key (comparision #'<) (key #'(lambda (x) x)))
  (let ((h NIL))
    (setf h (make-n-heap :key-func key :comparision comparision :insertions 0 :removals 0 :size 0 :array-size n))
    (setf (n-heap-array h) (make-array (n-heap-array-size h)))
    h
    )
  )

(defun add-heap (n heap)
  (setf (aref (n-heap-array heap) n) (create-heap 
                                 :comparision (n-heap-comparision heap)
                                      :key (n-heap-key-func heap)))
  )

(defun delete-heap (n heap)
  (if (not (null (aref (n-heap-array heap) n)))
  (setf (n-heap-size heap) (- (n-heap-size heap) (heap-size (aref (n-heap-array heap) n)))))
  (setf (aref (n-heap-array heap) n) NIL)
  )

(defun push-n-heap (heap n item)
  (incf (n-heap-insertions heap))
  (incf (n-heap-size heap))
  (if (null (aref (n-heap-array heap) n)) (add-heap n heap))
  (push-heap (aref (n-heap-array heap) n) item)
  )

(defun pop-n-heap (heap)
  (let
      ((maxk NIL) (maxheap 0) (result NIL))
    (dotimes (i (n-heap-array-size heap))
      (if (not (null (aref (n-heap-array heap) i)))
          (if (null maxk)
              (setf maxk (max-key (aref (n-heap-array heap) i))
                maxheap i)
            (if (funcall (n-heap-comparision heap)  (max-key (aref (n-heap-array heap) i)) 
                         maxk
                           )
            (setf maxk (max-key (aref (n-heap-array heap) i)) maxheap i))
            )
        )
      )
    (setf result (pop-heap (aref (n-heap-array heap) maxheap)))
    (incf (n-heap-removals heap))
    (decf (n-heap-size heap))
    (if (= (heap-size (aref (n-heap-array heap) maxheap)) 0)
        (delete-heap maxheap heap))
    result))
(defun procura-pre (problema heuristica &key (maximonos 10000))
  (let
      ((objectivo-p (problema-solucao problema)) 
       (estado-atual (make-node :item (problema-estado-inicial problema) :from NIL :action NIL :index 0))
       (ultimo-expandido 0)
       (ultimo-gerado 0)
       (vizinhos (problema-accoes problema))
       (nos-visitados NIL)
       (lista-accoes NIL)
       (rank 0)
       (execucao (problema-resultado problema))
       (fila (create-n-heap (1+ (list-length (estado-pecas-por-colocar (problema-estado-inicial problema))))
              :key #'(lambda (node)
                                   (cons (+ (funcall (problema-custo-caminho problema) (node-item node))
                                            (funcall heuristica (node-item node)))
                                         (node-index node)))
                          :comparision #'(lambda (key1 key2) ;funcao de prioridade ;f(n)=g(n)+h(n)
                                           (or 
                                            (< (car key1) (car key2))
                                            (and (= (car key1) (car key2)) (< (cdr key1) (cdr key2))))
                                           )
                              )))

    ;enquanto nao encontra o objectivo
    (loop
      (if (funcall objectivo-p (node-item estado-atual)) (return))

      (if (>= (n-heap-size fila) maximonos) 
          (progn
            ;(format t "bef:~$~%" (n-heap-size fila))
            (incf rank)
            (delete-heap rank fila)
            ;(format t "aft:~$~%" (n-heap-size fila))
            ))
      (setf nos-visitados (cons estado-atual nos-visitados))
      
      ;geracao de accoes
      (setf lista-accoes (funcall vizinhos (node-item estado-atual)))
      
      ;coloca vizinhos na fila
      (loop
        (if (null lista-accoes) (return))
        ;executa accao e coloca na fila
        (incf ultimo-gerado)
        (push-n-heap fila (1+ (list-length (estado-pecas-colocadas (node-item estado-atual))))
                     (make-node :item (funcall execucao (node-item estado-atual) (first lista-accoes))
                                   :index ultimo-gerado
                                   :from ultimo-expandido
                                   :action (first lista-accoes)))
        (setf lista-accoes (rest lista-accoes)))
      (incf ultimo-expandido)
      ;escolhe proximo no a expandir
      (if (= (n-heap-size fila) 0) (return))
      
      (setf estado-atual (pop-n-heap fila))

      )
    ;ultimo no a ser expandido
    ;(format t "Nos gerados:~$" (n-heap-insertions fila))
    ;(format t "Nos expandidos:~$~%" (n-heap-removals fila))
    (setf nos-visitados (cons estado-atual nos-visitados))
    ;reconstrucao de caminho
    (setf lista-accoes NIL)
    (loop
      (if (null (node-from estado-atual)) (return))
      (setf lista-accoes (cons (node-action estado-atual) lista-accoes))
      (setf estado-atual (nth (- ultimo-expandido (node-from estado-atual)) nos-visitados))
      ) 
    lista-accoes
    )
  )
(setf r (random-pecas 6))
  


(defun cria-tabuleiro-aleatorio (&optional (prob-inicial 2.0) (decaimento 0.05))
	(let ((tabuleiro (cria-tabuleiro))
		  (prob prob-inicial)
		  (coluna-a-evitar 0))
		(dotimes (linha 18)
			;;;precisamos de escolher sempre uma coluna para nao preencher, se nao podemos correr o risco de criarmos uma linha
			;;;completamente preenchida
			(setf coluna-a-evitar (random 10)) 
			(dotimes (coluna 10)
				(when (and (not (= coluna-a-evitar coluna)) (<= (random 1.0) prob)) (tabuleiro-preenche! tabuleiro linha coluna)))
			;;;nao podemos permitir valores negativos de probabilidade
			(setf prob (max 0 (- prob decaimento))))
   tabuleiro))

(defun a () (get_genetic_coefficients #'procura-pre
                          #'(lambda () (make-estado
                                        :tabuleiro (cria-tabuleiro-aleatorio 1.0 0.2)
                                        :pecas-por-colocar  (random-pecas 6)))
                          #'solucao
                          #'resultado
                          #'(lambda (e) (* (Estado-pontos e) (estado-pontos e))) 
                                      (list
                                       #'(lambda (e) (* 20 (heuristica-altura-tabuleiro-maxima e)))
                                       #'heuristica-linhas-parciais
                                       #'heuristica-buracos
                          #'maximo-pontos-estado
                          )
                          :pop 100
                          :limit 20
                          :mutation_chance 0.05
                                      ))

(defun tab-jeitoso () (let ((a (cria-tabuleiro)) (i 4))
                        (dotimes (c 10 a)
                          (dotimes (l i)
                            (tabuleiro-preenche! a l c))
                          (if (>= c 7) (decf i) (incf i))
                          )))

(defun h2 (e)
    (+ (* 20 0.55 (heuristica-altura-tabuleiro-maxima e))
       (* 0.10 (heuristica-linhas-parciais e))
       (* 0.33 (maximo-pontos-estado e))
       (* 0.056 (heuristica-buracos e))))
                        
(defun b() (procura-e-guarda #'procura-pre
                  (make-problema
                   :estado-inicial
                    (make-estado
                     :tabuleiro (cria-tabuleiro)
                     ;:tabuleiro (tab-jeitoso)                     
                     :pecas-por-colocar '(T T Z J T T Z J O I T Z J O T T Z I J T Z J O T Z J I O T Z T Z J O T Z J I) )) #'h2))
