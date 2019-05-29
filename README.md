# Classificação Binária

Quando analisamos a qualidade do ajuste para um modelo de classificação binário, é comum olharmos a qualidade do output somente em termos da classificação default:
- Se Probabilidade>=0.5, target recebe 1.
- 0, caso contrário.
No entanto, ao adotarmos o valor de 0.5 como ponto de corte para a probabilidade estimada para o target, podemos estar sendo muito rigorosos de acordo com o problema. Nesse contexto, tal aplicação visa variar a medida de corte (cutoff) e assim avaliar o desempenho das predições através de métricas calculadas através de tabelas de contingência (Acurácia, Sensibilidade|Recall, Especificidade, Precision e f1).

![stack Overflow](https://github.com/kelvercampanha/ClassificacaoBinaria/blob/master/Gr%C3%A1ficos.png)
