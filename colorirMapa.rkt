;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname colorirMapa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define GRAFO (list (list "BA" "SE" "AL" "PE" "PI" "MA" "TO" "GO" "MG" "ES")
(list "MG" "GO" "MS" "SP" "RJ" "ES" "BA")
(list "PA" "AP" "RR" "AM" "MT" "TO" "MA")
(list "MT" "PA""AM" "RO" "MS" "GO" "TO")
(list "PE" "PB" "CE" "PI" "BA" "AL")
(list "GO" "BA" "TO" "MT" "MS" "MG")
(list "TO" "MA" "PA" "MT" "GO" "BA")
(list "MS" "PR" "SP" "GO" "MG" "MT")
(list "AM" "RR" "AC" "MT" "RO" "PA")
(list "MA" "PA" "TO" "BA" "PI")
(list "PI" "MA" "BA" "PE" "CE")
(list "CE" "PI" "PE" "PB" "RN")
(list "SP" "PR" "MS" "RJ" "MG")
(list "PR" "SC" "SP" "MS")
(list "RJ" "SP" "MG" "ES")
(list "ES" "MG" "RJ" "BA")
(list "RO" "AM" "AC" "MT")
(list "PB" "RN" "CE" "PE")
(list "AL" "PE" "BA" "SE")
(list "SC" "RS" "PR")
(list "AC" "RO" "AM")
(list "RR" "PA" "AM")
(list "RN" "CE" "PB")
(list "SE" "AL" "BA") 
(list "RS" "SC")
(list "AP" "PA"))) 

(define COR (list "blue" "red" "yellow" "green"))

;encontra um vertice no grafo e verifica se sua cor e igual a cor passada como parametro
;true: cor ja usada em um vizinho
;false: cor nao usada em um vizinho
;string string -> boolean 
(define (corIgual? cor vertice grafo)
   (cond
    [(empty? grafo) empty];nunca chegara aqui
    [(string=? (first (first grafo)) vertice) 
       (cond
          [(string=? (first (rest (first grafo))) cor) true]  
          [else false]
       ) 
    ]
    [else (corIgual? cor vertice (rest grafo))]
   )
)

;testa uma cor com todos os vertices vizinhos, se ela nao estiver sendo usada, ela sera selecionada.
;string list -> boolean
(define (encontrouCor? cor listaDoVerticeParaColorir grafo)
  (cond
    [(empty? listaDoVerticeParaColorir) true]
    [(equal? (corIgual? cor (first listaDoVerticeParaColorir) grafo) true) false ]
    [else (encontrouCor? cor (rest listaDoVerticeParaColorir) grafo)] 
  )
  
)

;percorre a lista de cores ate encontrar uma cor que nao esteja sendo usada nos vertices vizinhos
;list list -> string 
(define (percorrerCor listaCor listaDoVerticeParaColorir grafo)
 (cond 
    [(empty? listaCor) empty];nunca sera executado, pois sempre uma cor sera encontrada
    [(equal? (encontrouCor? (first listaCor) (rest listaDoVerticeParaColorir) grafo) true) (first listaCor)]
    [else (percorrerCor (rest listaCor) listaDoVerticeParaColorir grafo)]
 )
)

;seta uma cor para o primeiro vertice da listaDoVerticeParaColorir.
;(list (list "SIGLA" "cor") ... (list "SIGLA" "VIZINHO1" ...))) 
;grafo com a listaDoVerticeParaColorir retirado dele
;list grafo -> grafo
(define (encontrarCor listaDoVerticeParaColorir grafo)
  (cons (list (first listaDoVerticeParaColorir) (percorrerCor COR listaDoVerticeParaColorir grafo) ) grafo) ;(list (list "BA" "red")... (resto do grafo))
)

;string -> grafo
(define (removerVertice vertice grafoParaComparacao)
   (cond
      [(empty? grafoParaComparacao)  grafoParaComparacao ]
      [(string=? (first (first grafoParaComparacao)) vertice) (removerVertice vertice (rest grafoParaComparacao))]
      [else (cons (first grafoParaComparacao ) (removerVertice vertice (rest grafoParaComparacao)))]
   )     
)
   

;retorna um grafo semi-colorido no seguinte formato:
   ;(list (list "SIGLA" "cor") ... (list "SIGLA" "VIZINHO1" ...)))  
;list grafo -> grafo
(define (colorir listaDoVerticeParaColorir grafoParaComparacao)
  (encontrarCor listaDoVerticeParaColorir (removerVertice (first listaDoVerticeParaColorir) grafoParaComparacao))
 )

;string list -> boolean
;recebe uma string que pode ser tanto uma cor quanto um vertice e verifica se e' uma cor valida das disponiveis
(define (colorido? str listaCor)
   (cond 
     [(empty? listaCor) false]
     [(string=? str (first listaCor)) true]
     [else (colorido? str (rest listaCor))]
   )
)

;;retorna um vertice nao colorido ainda
;grafo -> list
(define  (encontrarVerticeParaColorirHelper grafo)
   (cond
     [(empty? grafo) empty ]
     [else 
        (cond
           [(equal? (colorido? (first (rest (first grafo))) COR ) false)  (first grafo)]  
           [else (encontrarVerticeParaColorirHelper (rest grafo))]
        ) 
     ]
   )
)


;retorna (list (list "BA" "blue") ( list ("MG" "red"))
;grafo grafo -> grafo
(define (encontrarVerticeParaColorir grafo grafoParaComparacao)
  (cond
     [(empty? grafo) empty ]
     [else
        (colorir (encontrarVerticeParaColorirHelper grafo) grafoParaComparacao)
     ] 
  )
)


;percorre o grafo para colorir ele
;sempre passa um grafo atualizado com as cores para a funcao de colorir
;grafoAux usado pra gerar a recursividade
;grafo grafo -> grafo
(define (colorirMapa grafo grafoAux)
  (cond
     [(empty? grafoAux) grafo]
     [else
      (colorirMapa (encontrarVerticeParaColorir grafo grafo) (rest grafoAux)) 
     ]
    
  )
)

;imprime o grafo no formato:
  ;sigla -> cor
;grafo -> void
(define (imprimirMapa grafoColorido)
  (cond
    [(empty? grafoColorido) (display "")]
    [else
     (begin 
       (display (string-append (first (first grafoColorido)) "  ->  " (first (rest (first grafoColorido)))))
       (newline)
       (imprimirMapa (rest grafoColorido)) 
     )
    ]
  )
)

(imprimirMapa (colorirMapa GRAFO GRAFO))
 



 

