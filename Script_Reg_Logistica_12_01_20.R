#===========================================================================================
#Regressao Logistica:
#===========================================================================================

#===========================================================================================
#Regressao logistica e uma tecnica estatistica muito poderosa, 
#utilizada para modelagem de saidas binarias (sim ou nao). 
#Quando se quer medir a relacaoo de uma variavel dependente binaria 
#com uma ou mais variaveis independentes, e comum utilizar esta tecnica. 
#Pense, por exemplo, numa empresa que empresta dinheiro para um cliente. 
#Com base nas informacoes deste cliente (idade, profissao, etc.), 
#e interessante a empresa tentar prever se o cliente vai pagar a davida 
#ou nao. Uma forma de tentar prever isso e utilizando a regressao logistica.

#O modelo de regressao logística e utilizado quando a variavel dependente e binaria, 
#categorica ordenada ou mesmo categorica desordenada 
#(quando nao ha relacao hierarquica entre elas).

#
#===========================================================================================

#Baixando pacotes para a analise:
library(MASS)
library(caret)
library(xlsx)
library(e1071)
library(biotools)
library(MVN)
library(ffmanova)
library(goftest)
library(nlme)
library(klaR)
library("kernlab")
library(ROCR)
library(pROC)
library(faraway)
library(stargazer)
library(mfx)
library(ggplot2)
library("ResourceSelection")
library(modEvA)

#===========================================================================================
#Importando dados:
library(readxl)
BD_DF <- read_excel("Consultorias/Lais_Moreira_R/Dezembro/bancodedados_ind_DF.xlsx")

#Separando as variaveis do data frame BD_DF:
attach(BD_DF)

#Visualizando:
ggplot(BD_DF, aes(x=i_num_dpp, y=Tipo_setor)) + 
  geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

#Fazer graficos com as demais variaveis.
#===========================================================================================

#===========================================================================================
#Desenvolvendo o Modelo de Reg. Logistica:

#Dentro dos modelos logisticos temos o probit e o logit. O logit mostra
#as probabilidades de ocorrencia de determinado evento com base nas 
#variaveis independentes. Vamos utiliza-lo na nossa modelagem:

modelo <- glm(Tipo_setor ~ i_num_dpp +  i_num_dom_imp +
                i_num_mor_dpp + i_num_med_mor_por_dpp +
                i_perc_pes_res_brancas +
                i_perc_PR_nalf +
                i_Renda_Med_PR_DPP +
                i_perc_dpp_sem_col_lixo +
                i_perc_PR_sexo_fem +
                i_perc_pr_ate_30 +
                i_perc_pr_ren_ate_3_SM +
                i_Renda_Med_PR_DPP +
                i_perc_DPP_sem_ab_agua + i_perc_DPP_sem_ban_san +
                i_perc_DPP_sem_esg_fos +
                i_num_med_ban_hab + i_perc_dpp_outra_ocup,
                data = BD_DF, family = binomial(link = "logit"))

#Vendo os resultados do modelo:
summary(modelo)

#De forma mais visual:
stargazer(modelo, title="Resultados",type = "text")

#Encontrando a razao das chances no modelo. Os resultados encontrados mostram 
#em quantas vezes cada coeficiente tem forca para mostrar que o um domicilio e As:

exp(modelo$coefficients)

#De forma mais visual:
logitor(Tipo_setor ~ i_num_dpp +  i_num_dom_imp +
                i_num_mor_dpp + i_num_med_mor_por_dpp +
                i_perc_pes_res_brancas +
                i_perc_PR_nalf +
                i_Renda_Med_PR_DPP +
                i_perc_dpp_sem_col_lixo +
                i_perc_PR_sexo_fem +
                i_perc_pr_ate_30 +
                i_perc_pr_ren_ate_3_SM +
                i_Renda_Med_PR_DPP +
                i_perc_DPP_sem_ab_agua + i_perc_DPP_sem_ban_san +
                i_perc_DPP_sem_esg_fos +
                i_num_med_ban_hab + i_perc_dpp_outra_ocup,
                data = BD_DF)

#O resultado acima evidencia que para uma alteração em 1 (uma) unidade em 
#i_perc_pr_ate_30 a chance de que y seja igual a 1 aumenta em 6% ((1,06-1)*100). 
#Dito de outra forma, a chance de y=1 é 1,06 vezes maior quando x3 aumenta 
#em uma unidade (sendo que aqui mantêm-se as demais variáveis independentes constantes).

#VIF - Variance Inflation Factor:

#Os problemas de multicolinearedade nos modelos de regressao, ou seja, as relacoes 
#entre as variaveis do modelo, podem prejudicar a capacidade preditiva do mesmo. 
#Nas palavras de Hair et al. (2009, 191), "multicolinearidade 
#cria variancia "compartilhada" entre variáveis, diminuindo assim a capacidade 
#de prever a medida dependente, bem como averiguar os papeis relativos de cada 
#variável independente". Para resolver esta questão, utiliza-se o teste 
#do fator de inflacao da variancia (VIF - Variance Inflation Factor), 
#indice o qual nao deve ficar abaixo de 10 para representar baixo problema 
#de multicolinearidade segundo Rawlings, Pantula, e Dickey (1998).

#===========================================================================================

#===========================================================================================
#Selecao de variaveis:

#Como a selecao de todas as regressoes possiveis necessita de um consideravel esforco 
#computacional, outros metodos foram desenvolvidos para selecionar o melhor subconjunto 
#de variaveis sequencialmente, adicionando ou removendo variaveis em cada passo.
#O crierio para a adicao ou remocao de covariaveis e geralmente baseado na estatistica F, 
#comparando modelos com e sem as variaveis em questao. O AIC, assim como outros criterios, 
#tambem podem ser utilizados na decisao de inserir e remover variaveis. 
#Existem tres procedimentos automaticos: (1) Metodo Forward, (2) Metodo Backward e 
#(3) Metodo Stepwise.

#Forward: Esse procedimento parte da suposicao de que nao ha variavel no modelo, 
#apenas o intercepto. A ideia do metodo ao adicionar uma variavel de cada vez. 
#A primeira variavel selecionada e aquela com maior correlacao com a resposta.

#Enquanto o metodo Forward começa sem nenhuma variavel no modelo e adiciona 
#variáveis a cada passo, o método Backward faz o caminho oposto; 
#incorpora inicialmente todas as variáveis e depois, por etapas, cada uma 
#pode ser ou nao eliminada.

#Stepwise: e uma modificação da seleção Forward em que cada passo todas as variaveis 
#do modelo sao previamente verificadas pelas suas estatisticas F parciais. 
#Uma variavel adicionada no modelo no passo anterior pode ser redundante 
#para o modelo por causa do seu relacionamento com as outras variaveis e se 
#sua estatística F parcial for menor que  Fout, ela e removida do modelo.
#Iniciamos com uma variavel: aquela que tiver maior correlação com a variável resposta.
#A cada passo do forward, depois de incluir uma variavel, aplica-se o backward
#para ver se sera descartada alguma variavel.

#O metodo Stepwise auxilia o pesquisador em selecionar as variáveis importantes 
#ao modelo, sendo que podem ser utilizadas nas direçoes "both", "backward", "forward". 
#Este metodo, por sua vez, utiliza o 
#Criterio de Informação de Akaike (AIC - Akaike Information Criterion) 
#na combinaçao das variáveis dos diversos modelos simulados para selecionar 
#o modelo mais ajustado. Quanto menor o AIC, melhor o ajuste do modelo. 
#O AIC é calculado da seguitne forma:
###AIC= ???2log(Lp)+2[(p+1)+1]
#onde Lp é a função de maxima verossimilhança e p é o número de variáveis explicativas 
#do modelo. 

stepewiselog <- step(modelo, scale = 0.10, direction = c("both"))
summary(stepewiselog)

modelo2 <- glm(Tipo_setor ~ i_num_mor_dpp +
                 i_num_med_mor_por_dpp +
                 i_perc_PR_nalf +
                 i_Renda_Med_PR_DPP +
                 i_perc_dpp_sem_col_lixo +
                 i_perc_PR_sexo_fem +    
                 i_perc_pr_ate_30 +
                 i_perc_DPP_sem_esg_fos +
                 i_num_med_ban_hab ,
              data = BD_DF, family = binomial(link = "logit"))

summary(modelo2)

#De forma mais visual:
stargazer(modelo2, title="Resultados",type = "text")

#VIF - Variance Inflation Factor:

#Os problemas de multicolinearedade nos modelos de regressao, ou seja, as relacoes 
#entre as variaveis do modelo, podem prejudicar a capacidade preditiva do mesmo. 
#Nas palavras de Hair et al. (2009, 191), "multicolinearidade 
#cria variancia "compartilhada" entre variáveis, diminuindo assim a capacidade 
#de prever a medida dependente, bem como averiguar os papeis relativos de cada 
#variável independente". Para resolver esta questão, utiliza-se o teste 
#do fator de inflacao da variancia (VIF - Variance Inflation Factor), 
#indice o qual nao deve ficar abaixo de 10 para representar baixo problema 
#de multicolinearidade segundo Rawlings, Pantula, e Dickey (1998).

vif(modelo2)

#Ajustando o modelo:
modelo3 <- glm(Tipo_setor ~ i_num_mor_dpp +
                 i_num_med_mor_por_dpp +
                 i_perc_dpp_sem_col_lixo +
                 i_perc_pr_ate_30 +
                 i_perc_DPP_sem_esg_fos +
                 i_num_med_ban_hab ,
               data = BD_DF, family = binomial(link = "logit"))

summary(modelo3)
vif(modelo3)
#Os vifs foram muito altos. Vai ter que explicar teoricamente o porque disso.

#Encontrando a razao das chances no modelo. Os resultados encontrados mostram 
#em quantas vezes cada coeficiente tem forca para mostrar que o um domicilio e As:

exp(modelo3$coefficients)#resultados mais condizentes com o esperado.
#===========================================================================================

#===========================================================================================
#Fazendo as previsoes do modelo:


previsoes <- predict(object = modelo3, newdata = BD_DF, type ="response")
View(previsoes)#visualizando o vetor de probabilidades.
table(previsoes)

#Visualizando as previsoes do nosso modelo:
plot(sort(previsoes))

View(BD_DF_pred)
table(BD_DF_pred$Tipo_Setor_previsto)

#Montando os vetores de limiares:
limiares <- sort(previsoes)

#===========================================================================================

#===========================================================================================
#Montando a matriz de confusao:

#Primeiro montando os objetos para medir a qualidade do modelo:
classe_predita <- ifelse(previsoes > 0.034,1,0)
  
#Gerar matriz de confusao:
confusao <- table(Predito = classe_predita, original = Tipo_setor)
confusionMatrix(classe_predita, Tipo_setor, positive = "0")
  
#Verificando valores previstos:
vp <- confusao[1,1] #verdadeiros positivos
fn <- confusao[2,1] #falsos negativos
  
vn <- confusao[2,2] #verdadeiros negativos
fp <- confusao[1,2] #falsos positivos
  
#Calculando a acuracia do modelo:
acuracia <- sum(diag(confusao))/sum(confusao)
  
#Calculando a sensitividade:
sensitividade <- vp/(vp+fn)
  
#Calculando a espeficidade:
especificidade <- vn/(vn+fp)

#Colocando o vetor de probabilidades na nossa base original:
BD_DF_pred <- data.frame(BD_DF, probabilidaes = previsoes, 
                         Tipo_Setor_previsto = classe_predita)
#===========================================================================================

#===========================================================================================
#Curva ROC:
#A Curva ROC (Receiver Operating Characteristic Curve) associada ao modelo logistico 
#mensura a capacidade de predicao do modelo proposto, atraves das predições da 
#sensibilidade e da especificidade. Segundo Fawcett (2006) esta técnica serve para 
#visualizar, organizar e classificar o modelo com base na performance preditiva.
#A curva ROC é produzida bi-dimensionalmente, pela obtenção da relação entre a taxa 
#dos verdadeiros positivos do modelo e da taxa dos falsos positivos preditos. 
#Desta forma, o ponto inferior esquerdo (0,0) significa que não é predita uma 
#classificacao positiva; no canto oposto do grafico (1,1) classifica os resultados 
#incondicionalmente positivos e; o ponto (0,1) representa uma excelente classificacao. 
#Quanto mais ao noroeste do gráfico o ponto estiver melhor.

#Fazendo a curva:
roc(Tipo_setor, modelo3$fitted.values, plot=T, legacy.axes=T,
    percent=T, xlab="Porcentagem Positiva Falsa",
    ylab="Porcentagem Positiva Verdadeira")

#No grafico abaixo demonstrada a elaboracao do conceito da Area sobre a Curva ROC 
#(AUC - Area Under the ROC Curve), que objetiva comparar os classificadores a partir 
#da parformance da curva em um rnico valor escalar (Fawcett 2006).
#Este indicador representa a probabilidade de que o classificador efetue predicoes
#randomicas na instancia positiva melhor do que na instância negativa. O indicador 
#AUC sempre tera seu valor entre 0 e 1, sendo que quanto maior, 
#melhor e nunca um classificador realistico deve estar abaixo de 0,5. 
#Hosmer e Lemeschow (2000) sugere a utilização de AUC acima de 0,7 como aceitavel. 
#Sua AUC foi 0,96 e o preditor otimo foi de 0,034 (limiar que maximiza sua funcao).

roc1 <- plot.roc(Tipo_setor, modelo3$fitted.values)

plot(roc1,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)
#===========================================================================================

#===========================================================================================
#teste Hosmer e Lemeshow:
#O teste de Hosmer e Lemeshow e utilizado para demonstrar a qualidade do ajuste do 
#modelo, ou seja, se o modelo pode explicar os dados observados. Para este teste, 
#os dados são divididos de acordo com as probabilidades previstas em 10 grupos iguais, 
#sendo que os números previstos e os reais são comparados com a estatistica do 
#qui-quadrado. Hair et al. (2009) sugerem um tamanho de amostra de pelo menos 50 
#casos para a realização deste teste.
#A hipótese nula H0do qui-quadrado (p=0,05) deste teste é a de que as proporcoes 
#observadas e esperadas sao as mesmas ao longo da amostra. 

teste_hl<- hoslem.test(Tipo_setor,modelo3$fitted.values,g=10)
teste_hl

#No teste nao se rejeitou H0, portanto, as proporcoes observadas e esperadas sao 
#as mesmas ao longo da amostra.

# Pseudo R2:
#Semelhante ao coeficiente de determinação R2 da regressao multipla, a medida de 
#pseudo R2 representam o ajuste geral do modelo proposto. Sua interpretacao, portanto, 
#e semelhante a regressao multipla. Abaixo segue o calculo do pseudo R2:
#R2LOGIT=???2LLnulo???(???2LLmodelo)???2LLnulo
#Lembrando que o valor -2LL representa -2 vezes o logaritmo do valor 
#de verossimilhanca, onde a verossimilhanca do modelo nulo e comparado com o 
#modelo completo. 
RsqGLM(modelo3)
#===========================================================================================
