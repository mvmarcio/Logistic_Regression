#SCRIPT REGRESSAO LOGISTICA - JUL/2020

#Regressao Logistica
#Pacotes necessarios
#Listar todos aqui

library(stats)
library(mfx)
library(MASS)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(tidyverse)
library(ResourceSelection)
library(DHARMa)
library(psych)
library(ggplot2)
library(car)
library(pROC)

#1 - Modelo de Reg. Logistica:

attach(bancodedados_ind)

#Modelo LOGIT com todas as 17 variaveis

logit_17 <- glm(Tipo_setor ~
                   i_num_dpp + 
                   i_num_mor_dpp + 
                   i_num_med_mor_por_dpp + 
                   i_perc_pes_res_brancas +
                   i_perc_PR_sexo_fem +
                   i_num_dom_imp +
                   i_perc_dpp_outra_ocup +
                   i_perc_pr_ate_30 +
                   i_perc_PR_nalf +
                   i_perc_PR_ate_30_nalf +
                   i_perc_pr_ren_ate_3_SM +
                   i_Renda_Med_PR_DPP +
                   i_perc_dpp_sem_col_lixo +
                   i_perc_DPP_sem_ab_agua +
                   i_perc_DPP_sem_ban_san + 
                   i_perc_DPP_sem_esg_fos +
                   i_num_med_ban_hab,
                data = bancodedados_ind, family = binomial(link = "logit"))

#Visualizacao dos Resultados

sum17 <- summary(logit_17)

sum17

write.xlsx(sum17$coefficients, "D:/Metodologia_R/Resultado_final_RL/logit_17.xlsx")

#Visualizacao das razoes de chance

odds_logit17 <- logitor(Tipo_setor ~
                          i_num_dpp + 
                          i_num_mor_dpp + 
                          i_num_med_mor_por_dpp + 
                          i_perc_pes_res_brancas +
                          i_perc_PR_sexo_fem +
                          i_num_dom_imp +
                          i_perc_dpp_outra_ocup +
                          i_perc_pr_ate_30 +
                          i_perc_PR_nalf +
                          i_perc_PR_ate_30_nalf +
                          i_perc_pr_ren_ate_3_SM +
                          i_Renda_Med_PR_DPP +
                          i_perc_dpp_sem_col_lixo +
                          i_perc_DPP_sem_ab_agua +
                          i_perc_DPP_sem_ban_san + 
                          i_perc_DPP_sem_esg_fos +
                          i_num_med_ban_hab,
                        data = bancodedados_ind)

odds_logit17

write.xlsx(odds_logit17$oddsratio, "D:/Metodologia_R/Resultado_final_RL/odds_logit17.xlsx")

#As razoes de chance mostradas pela funcao logitor correspondem ao numero de euler elevado ao coeficiente da variavel em questao:

exp(logit$coefficients)

#O resultado das razoes de chance permitem a leitura das inferencias estatisticas.

#1.1 - Procedimento Stepwise pelo PACOTE MASS

#Como a selecao de todas as regressoes possiveis necessita de um consideravel esforco 
#computacional, outros metodos foram desenvolvidos para selecionar o melhor subconjunto 
#de variaveis sequencialmente, adicionando ou removendo variaveis em cada passo.
#O criterio para a adicao ou remocao de covariaveis e geralmente baseado na estatistica F, 
#comparando modelos com e sem as variaveis em questao. Porem, foi utilizado o criterio de
#Informa??o de Akaike (AIC - Akaike Information Criterion), tambem adequado na decisao de
#inserir e remover variaveis. Quanto menor o AIC, melhor o ajuste do modelo.

logit_step <- logit_17 %>% stepAIC(trace = TRUE)

stepwise <- summary(logit_step)

stepwise$coefficients

#O procedimento Stepwise selecionou 10 variaveis. Porem, foi removido o indicador
#do abastecimento de agua, por nao apresentar significancia estatistica a 5%.

#Modelo logit final - com 9 variaviis independentes:

logit_9 <- glm(Tipo_setor ~ i_num_mor_dpp + 
                 i_num_med_mor_por_dpp +
                 i_perc_PR_sexo_fem +
                 i_perc_pr_ate_30 +
                 i_perc_PR_nalf +
                 i_Renda_Med_PR_DPP +
                 i_perc_dpp_sem_col_lixo + 
                 i_perc_DPP_sem_esg_fos +
                 i_num_med_ban_hab,
               data = bancodedados_ind, family = binomial(link = "logit"))

summary(logit_9)

exp(logit_9$coefficients)

#O coef. de PR nalf foi negativo. Porque?

#Detalhamento do banco de dados:

describe(bancodedados_ind)

View(bancodedados_ind$i_perc_PR_nalf>20)
View(bancodedados_ind$i_perc_PR_nalf>20 & bancodedados_ind$Tipo_setor==1)

#_________________________________________________________________________________________________#

#2- Teste dos Pressupostos

#2.1 - Premissa de Linearidade

#Para fazer a analise da relacao entre as variaveis independentes (preditoras) e o termo logit
#(probabilidades/1 - probabilidade), e necessario rodar as predicoes das probabilidades primeiro.

#Predicoes
#Treshold = 0.043

probab <- predict(logit_9, type = "response")
classes.preditas <- ifelse(probab > .04300811, 1, 0)

View(classes.preditas)

#Para analisar linearidade (segundo Kassambara, 2018):

soind <- data.frame(i_num_mor_dpp, 
                    i_num_med_mor_por_dpp,
                    i_perc_PR_sexo_fem,
                    i_perc_pr_ate_30,
                    i_perc_PR_nalf,
                    i_Renda_Med_PR_DPP,
                    i_perc_dpp_sem_col_lixo,
                    i_perc_DPP_sem_esg_fos,
                    i_num_med_ban_hab) #dataframe contendo somente os indicadores

#a) Selecionar somente os dados numericos dos meu banco de dados

theme_set(theme_classic())
mydata <- soind %>%
  dplyr::select_if(is.numeric)
preditores <- colnames(mydata)

#b) Vincular o logit e organizar os dados para plotagem

mydata <- mydata %>%
  mutate(logit = log(probab/(1-probab))) %>%
  gather(key = "predictors", value = "predictor.value", - logit)

#c) Plotar os graficos 

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~predictors, scales = "free_y") +
  theme(
    axis.text = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif", size = 12)
  )

###### Uma variavel sera removida (Sexo Feminino) ######

#Novo modelo:

logit_8 <- glm(Tipo_setor ~ i_num_mor_dpp + 
                 i_num_med_mor_por_dpp +
                 i_perc_pr_ate_30 +
                 i_perc_PR_nalf +
                 i_Renda_Med_PR_DPP +
                 i_perc_dpp_sem_col_lixo + 
                 i_perc_DPP_sem_esg_fos +
                 i_num_med_ban_hab,
               data = bancodedados_ind, family = binomial(link = "logit"))


exp(logit_8$coefficients)

sumlogit8 <- summary(logit_8)

sumlogit8$coefficients

write.xlsx(sumlogit8$coefficients, "D:/Metodologia_R/Resultado_final_RL/logit8_coefficients.xlsx")

#Razao de chances do novo modelo

odds_logit8 <- logitor(Tipo_setor ~ i_num_mor_dpp + 
                         i_num_med_mor_por_dpp +
                         i_perc_pr_ate_30 +
                         i_perc_PR_nalf +
                         i_Renda_Med_PR_DPP +
                         i_perc_dpp_sem_col_lixo + 
                         i_perc_DPP_sem_esg_fos +
                         i_num_med_ban_hab,
                       data = bancodedados_ind)

odds_logit8$oddsratio

write.xlsx(odds_logit8$oddsratio, "D:/Metodologia_R/Resultado_final_RL/logit8_odds.xlsx")

#Repetir o procedimento acima para o novo modelo

#Predict

probab8 <- predict(logit_8, type = "response")
classes.preditas8 <- ifelse(probab8 > .04300811, 1, 0)

View(classes.preditas8)

#Banco de dados
soind8 <- data.frame(i_num_mor_dpp, 
                     i_num_med_mor_por_dpp,
                     i_perc_pr_ate_30,
                     i_perc_PR_nalf,
                     i_Renda_Med_PR_DPP,
                     i_perc_dpp_sem_col_lixo,
                     i_perc_DPP_sem_esg_fos,
                     i_num_med_ban_hab)

attach(bancodedados_ind)

bd_8 <- data.frame(Cod_Setor,
                   Tipo_setor,
                   i_num_mor_dpp, 
                   i_num_med_mor_por_dpp,
                   i_perc_pr_ate_30,
                   i_perc_PR_nalf,
                   i_Renda_Med_PR_DPP,
                   i_perc_dpp_sem_col_lixo,
                   i_perc_DPP_sem_esg_fos,
                   i_num_med_ban_hab)

write.xlsx(bd_8, "D:/Metodologia_R/Resultado_final_RL/bd_8.xlsx")


#a) Selecionar somente os dados numericos do banco de dados

mydat8 <- soind8 %>%
  dplyr::select_if(is.numeric)
predictors8 <- colnames(mydat8)

#b) Vincular o logit e organizar os dados para plotagem

mydat8 <- mydat8 %>%
  mutate(logit = log(probab8/(1-probab8))) %>%
  gather(key = "predictors8", value = "predictor.value", - logit)

#c) Plotar os graficos

ggplot(mydat8, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~predictors8, scales = "free_y") +
  theme(
    axis.text = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif", size = 12)
  )


attach(bd_8)

#Estudo do indicador pr-nalf x logit

ggplot(bancodedados_ind, aes(logit, i_perc_PR_nalf))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() + 
  theme(
    axis.text = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif", size = 12)
  )

#Estudo do indicador pr-nalf x probabilities

ggplot(bancodedados_ind, aes(probab8, i_Renda_Med_PR_DPP))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  theme(
    axis.text = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif", size = 12)
  )


ggplot(bancodedados_ind, aes(probab8, i_num_med_ban_hab))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  theme(
    axis.text = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif", size = 12)
  )


ggplot(bancodedados_ind, aes(probab8, i_perc_PR_nalf))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() + 
  theme(
    axis.text = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif", size = 12)
  )

#___________________________________________________________________________________________#

#2.2 - Premissa da ausencia de outliers

#Visualizacao dos Residuos
par(family="serif")
plot(logit_8$residuals, ylab = "Resíduos")

ggplot(logit_8) +
  geom_point(mapping = aes(x = logit_8$fitted.values, y = logit_8$residuals)) +
  xlab("Probabilidades Preditas") + ylab("Residuos") +
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  theme(
    axis.text = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif", size = 12)
  )

#Um outlier muito alto esta atrapalhando a visualizacao. 

View(logit_8$residuals)

#O outlier se refere a linha 2076, com o valor de 2084.
#Para melhor visualizacao dos residuos, ele sera retirado,
#junto com outros dois setores que apresentaram valores
#em torno de 80.

resl8 <- data.frame(logit_8$residuals)
resl8_semoutlier <- resl8[c(-2076,-4026,-2963),]

fitvl8 <- data.frame(logit_8$fitted.values)
fitvl8_semoutlier <- fitvl8[c(-2076,-4026,-2963),]

ggplot() +
    geom_point(mapping = aes(x = fitvl8_semoutlier, y = resl8_semoutlier)) +
               xlab("Probabilidades Preditas") + ylab("Residuos") +
    geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(
    axis.text = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif", size = 12)
  )



#Assim, podemos ver seu comportamento com mais facilidade.
#O comportamento pode ser considerado homocedastico?

#2.2.1 Analise de Outliers segundo Hilbe, 2015

mu <- logit_8$fitted.values

dr <- resid(logit_8, type= "deviance") # deviance residual

hat <- hatvalues(logit_8) # hat matrix diagonal

stdr <- dr/sqrt(1-hat) # standardized deviance residual

View(stdr^2)

par(family="serif")
plot(mu, stdr^2)
abline(h = 4, col= "red")

#Por esse metodo, foram identificados 43 outliers.

#2.2.2  Analise de Outliers segundo Kassambara, 2015
par(family="serif")
plot(logit_8, which = 4, id.n = 3)

#Foram apontados os outliers da linha 615, 1296 e 2963.

model.data <- augment(logit_8) %>%
  mutate(index = 1:n())

model.data %>% top_n(3, .cooksd)

View (model.data %>%
        filter(abs(.std.resid) > 3))

#Grafico - ###MEXER AQUI MARCIO
Setor <- data.frame(ifelse(model.data$Tipo_setor==1,"SBN","NE"))
model.data["Setor"] <- Setor

res_graph <- ggplot(model.data, aes(index, .std.resid)) +
  geom_point(aes(color = as.factor(Setor), alpha = .5)) +
  theme_bw() +
  scale_fill_discrete(name = "Tipo de Setor", labels = c("NE", "SbN")) +
  theme(
    axis.text = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif", size = 12)
  ) + scale_y_continuous(breaks = c(0,1))

res_graph

#Quero colocar somente 0 e 1 na legenda e nao de 0 a 1  !
#Tentei assim, mas nao deu certo:

scale_y_discrete(labels = c("0" = "NE", "1" = "SbN"))

#Aqui, foram apontados apenas dois outlier, da linha 2076 e 2963.
#Como faco para o Tipo_Setor apresentar apenas 0 e 1 na legenda do grafico?

#Conclusao: meus dados possuem outliers. Isso deve ser levado em consideracao.

#2.3 Premissa de Ausencia de Multicolinearidade

#VIF - Variance Inflation Factor - Deve ser menor que 10.

vif <- vif(logit_8)

vif

write.xlsx(vif, "D:/Metodologia_R/Resultado_final_RL/vif_logit_9.xlsx")

#Nenhuma variavel apresentou VIF alto a ponto de indicar multicolinearidade.

#__________________________________________________________________________________#

#Avaliando o modelo

#4- Questao da Inflacao de zeros e a possivel superdispersao

#O alto numero de zeros no meu BD pode estar gerando superdispersao?
#Quantos porcento dos meus dados sao zeros?

(sum(soind8 == 0))/36621 *100

#19% ! Isso ? inflacionado?

#Testar superdispersao

#Teste de extra dispersao -> estat qui quadrado/residual dof ~ 1
#O teste resultou em 0.94

est_pr <- resid(logit_8, type = "pearson") # calculates Pearson residuals

pchi2 <- sum(residuals(logit_8, type = "pearson")^2)
disp <- pchi2/logit_8$df.residual
c(pchi2, disp)

#Testando a superdispersao com REFIT (true) e sem REFIT (false).

sim_res <- simulateResiduals(logit_8, refit=T)
par(family="serif")
plot(sim_res)

sim_resF <- simulateResiduals(logit_8, refit=F)
par(family="serif")
plot(sim_resF)

#Teste de Heterocedasticidade (Vignette - Dharma)

sim_rest <- simulateResiduals(fittedModel = logit_8)
par(family="serif")
plot(sim_rest)

testQuantiles(sim_rest)

sim_rest$scaledResiduals

plotResiduals(sim_rest, bd_8$i_perc_PR_nalf)

#Testar se o modelo e SUPER ou SUB disperso (two-sided)

testDispersion(sim_res, alternative = c("two.sided"), plot = T)
testDispersion(sim_resF, alternative = c("two.sided"), plot = T)

#O teste nao rejeita a hipotese de que o dado nao tem super ou subdispersao.
#Dispersao = 1.073, valor que indica a ausencia de extra dispers?o (sub ou super).

#Teste de extra dispersao -> estat qui quadrado/residual dof ~ 1
#O teste resultou em 0.94

est_pr <- resid(logit_8, type = "pearson") # calculates Pearson residuals

pchi2 <- sum(residuals(logit_8, type = "pearson")^2)
disp <- pchi2/logit_8$df.residual
c(pchi2, disp)

#Visualizacao dos resultados:
par(family="serif")
plot(sim_res)

#Eu nao sei interpretar esses graficos. Preciso de ajuda.
#Aparentemente o teste KS (Komolgorov-Smirnov test) e o de Outlier
#deram resultados em vermelho (preocupantes).
#No caso do outlier, ja era esperado e, no caso do teste KS,
#nao ha problema na rejeicao da hipotese nula, ja que os dados
#nao precisam apresentar distribuicao normal.

plot(sim_resF)

#Aqui, apenas o teste de outlier apresentou resultado preocupante.

#Conclusao (eu acho): Meu modelo NAO apresenta sub ou super dispersao.

#3- Ajuste do modelo (Goodness of fit)


#3.1 teste Hosmer e Lemeshow:
#O teste de Hosmer e Lemeshow e utilizado para demonstrar a qualidade do ajuste do 
#modelo, ou seja, se o modelo pode explicar os dados observados. Para este teste, 
#os dados s?o divididos de acordo com as probabilidades previstas em 10 grupos iguais, 
#sendo que os n?meros previstos e os reais s?o comparados com a estatistica do 
#qui-quadrado. Hair et al. (2009) sugerem um tamanho de amostra de pelo menos 50 
#casos para a realiza??o deste teste. #A hip?tese nula H0do qui-quadrado (p=0,05)
#deste teste ? a de que as proporcoes observadas e esperadas sao as mesmas ao longo da amostra. 


teste_hl <- hoslem.test(Tipo_setor, logit_8$fitted.values, g = 10)

teste_hl

#No teste nao se rejeitou H0, portanto, as proporcoes observadas e esperadas sao 
#as mesmas ao longo da amostra. p-value = 0.08368.

#3.2 Pearson Chi2 Goodness-of-Fit Test

pr <- sum(residuals(logit_8, type= "pearson")^2)
pr

df <- logit_8$df.residual
df

p_value <- pchisq(pr, df, lower=F)
p_value

print(matrix(c("Pearson Chi GOF","Chi2","df","p-value", " ",
               + round(pr,4), df, round(p_value,4)), ncol=2))

#p_value > 0.05 significa que rejeitamos a hipotese nula de que o modelo nao esta bm ajustado.


#_____________________________________________________________________________#

#Predicoes do modelo:

prob8 <- predict(object = logit_8, newdata = bancodedados_ind, type = "response")
classes.pred8 <- ifelse(prob8 > .05438969, 1, 0)

View(prob8) #vetor de probabilidades.

tab_logit_8 <- table(classes.pred8)

View(tab_logit_8)

#Visualizando as previsoes do nosso modelo:
plot(sort(prob8))


#Montando os vetores de limiares:
limiares <- sort(prob8)
View(limiares)

#Colocando o vetor de probabilidades na nossa base original:
bancodedados_pred_RL <- data.frame(bancodedados_ind, Probabilidades = prob8, 
                                   Tipo_Setor_previsto = classes.pred8)

View(bancodedados_pred_RL)
table(bancodedados_pred_RL$Tipo_Setor_previsto)

#Matriz de Confusao

#Primeiro montando os objetos para medir a qualidade do modelo:
acuracia <- c()
sensitividade <- c()
especificidade <- c()

#Gerar matriz de confusao:
confusao <- table(Predito = classes.pred8, Original = bancodedados_ind$Tipo_setor)

confusao

#Verificando valores previstos:
vp <- confusao[1,1] #verdadeiros positivos
fn <- confusao[2,1] #falsos negativos

vn <- confusao[2,2] #verdadeiros negativos
fp <- confusao[1,2] #falsos positivos


#Calculando a acuracia do modelo:
acuracia <- sum(diag(confusao))/sum(confusao)

acuracia

#Calculando a sensitividade:
sensitividade <- vp/(vp+fn)

sensitividade

#Calculando a espeficidade:
especificidade <- vn/(vn+fp)

especificidade

lines(logit_8$fitted.values)

#Curva ROC:
#A Curva ROC (Receiver Operating Characteristic Curve) associada ao modelo logistico 
#mensura a capacidade de predicao do modelo proposto, atraves das predi??es da 
#sensibilidade e da especificidade. Segundo Fawcett (2006) esta t?cnica serve para 
#visualizar, organizar e classificar o modelo com base na performance preditiva.
#A curva ROC ? produzida bi-dimensionalmente, pela obten??o da rela??o entre a taxa 
#dos verdadeiros positivos do modelo e da taxa dos falsos positivos preditos. 
#Desta forma, o ponto inferior esquerdo (0,0) significa que n?o ? predita uma 
#classificacao positiva; no canto oposto do grafico (1,1) classifica os resultados 
#incondicionalmente positivos e; o ponto (0,1) representa uma excelente classificacao. 
#Quanto mais ao noroeste do gr?fico o ponto estiver melhor.

#Curva ROC

auc(as.numeric(logit_8$model[1]==1),as.vector(fitted(logit_8)))

plot(roc(as.numeric(logit_8$model[1]==1),as.vector(fitted(logit_8))))

mean(bancodedados_ind$Tipo_setor, na.rm=T)

library(pROC)

myroc <- roc(as.numeric(logit_8$model[1]==1),as.vector(fitted(logit_8)))
mycoords <- coords(myroc, "all", transpose = TRUE)

par(family="serif")
plot(mycoords["threshold",], mycoords["specificity",], type="l", col="red", xlab="Cutoff", ylab="Performance")
lines(mycoords["threshold",], mycoords["sensitivity",], type="l", col="blue")
legend(100, 0.4, c("Specificity", "Sensitivity"), col=c("red", "blue"), lty=1)

par(family="serif")
best.coords <- coords(myroc, "best", best.method = "youden", best.weights = c(1, 0.5), transpose = TRUE)
abline(v=best.coords["threshold"], lty=2, col="grey")
abline(h=best.coords["specificity"], lty=2, col="red")
abline(h=best.coords["sensitivity"], lty=2, col="blue")

best.coords
