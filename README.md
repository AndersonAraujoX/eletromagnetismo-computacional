# Projetos de Eletromagnetismo Computacional

Este repositório contém os projetos desenvolvidos para a disciplina de Eletromagnetismo Computacional. Os projetos abordam diferentes problemas em eletromagnetismo, utilizando métodos computacionais para simulação e análise.

## Projeto 1: Blindagem Eletrostática

Este projeto explora o conceito de blindagem eletrostática em diferentes geometrias.

* **Configuração de mínima energia em círculos e elipses:** Desenvolvimento de um programa para encontrar a configuração de mínima energia de N cargas distribuídas em um círculo e, posteriormente, em uma elipse. 
* **Influência da dimensão do potencial:** Investigação da diferença ao utilizar a expressão do potencial em 2D (proporcional a ln r) e 3D (proporcional a 1/r).
* **Efeito de cargas externas:** Análise do efeito de uma carga positiva externa na configuração de equilíbrio das cargas na elipse. 
* **Extensão para elipsoides:** Cálculo da configuração de mínima energia para cargas distribuídas em diferentes tipos de elipsoides (esfera, esferoide oblato e prolato) utilizando o potencial 3D.

## Projeto 2: A Equação de Laplace

Este projeto foca na solução numérica da equação de Laplace em duas dimensões.

* **Solução com o método de Jacobi:** Implementação do método de relaxação de Jacobi para resolver a equação de Laplace em uma caixa quadrada com condições de contorno específicas. 
    * Visualização das linhas equipotenciais e do campo elétrico. 
    * Análise do critério de convergência e do número de iterações. 
* **Aplicação em diferentes geometrias:** Resolução da equação de Laplace para um prisma oco com uma barra metálica interna e para um capacitor.
    * Visualização das linhas equipotenciais e do campo elétrico para essas configurações. 
* **Solução com o método do caminho aleatório:** Resolução do problema da caixa quadrada utilizando o método do caminho aleatório e comparação com o método de Jacobi. 
## Projeto 3: A Equação de Poisson

Este projeto aborda a solução numérica da equação de Poisson em três dimensões.

* **Solução com o método de Jacobi:** Implementação do método de relaxação de Jacobi para resolver a equação de Poisson em uma caixa cúbica com uma carga pontual na origem.
* **Solução com o método de Gauss-Seidel:** Resolução do mesmo problema utilizando o método de Gauss-Seidel e comparação com o método de Jacobi. 
    * Análise do número de iterações para convergência. 
    * Visualização das linhas equipotenciais e do campo elétrico. 
    * Análise do potencial em diferentes direções e comparação com o potencial de uma carga pontual no vácuo.
* **Deslocamento da carga:** Resolução do problema com a carga pontual deslocada da origem. 

## Projeto 4: Cargas em Movimento

Este projeto investiga os campos elétricos e magnéticos gerados por cargas em movimento.

* **Carga com velocidade constante:** Cálculo dos campos E e B para uma carga pontual se movendo com velocidade constante, utilizando o método da bisseção para encontrar o tempo retardado.
    * Visualização do campo elétrico e análise do campo magnético. 
    * Comparação com o campo de uma carga estacionária.
* **Carga com aceleração constante:** Cálculo dos campos E e B para uma carga pontual com aceleração constante. 
    * Visualização do campo elétrico, análise do campo magnético e comparação com o caso de velocidade constante.
* **Carga oscilando:** Cálculo do campo elétrico de radiação e do vetor de Poynting para uma carga pontual oscilando. 
    * Análise da média temporal do campo elétrico e do vetor de Poynting. 
    * Comparação dos resultados com previsões teóricas. 
