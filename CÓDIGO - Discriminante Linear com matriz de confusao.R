library(readr)
library(ggplot2)
library(dplyr)
library(car)
library(summarytools)
library(pROC)
library(readxl)
library(MASS)

rm(list = ls())

setwd('C:/Users/rdsf/OneDrive/�rea de Trabalho/GV/Modelagem estat�stica avan�ada/Trabalho')
german_data = read_excel("Base - German.xlsx")

# Tratando a vari�vel resposta para fator
german_data <- german_data %>% mutate(response = factor(response,
                                                                  levels = c(2,1),
                                                                      labels = c('Ruim','Bom')))

GD = data.frame(german_data$chk_acc,german_data$duration,german_data$history,german_data$purpose
                ,german_data$amount,german_data$savings,german_data$employment,german_data$install_rate
                ,german_data$pers_status,german_data$guarantor,german_data$real_state,german_data$other_installment,
                german_data$housing,german_data$num_credits,german_data$job,german_data$num_dependents
                ,german_data$response)

#Separando os dados em treino e teste
set.seed(123)
treino = sample(dim(GD)[1], dim(GD)[1]*0.7); treino
treinoGD = GD[treino,]%>%as_tibble()
testeGD = GD[-treino,]%>%as_tibble()

###Exclus�o de Amount###
treinoGD=data.frame(treinoGD[,-5])
testeGD=data.frame(testeGD[,-5])

####REGRESSAO LINEAR DISCRIMINANTE#####

Discr. = lda(german_data.response ~., data = treinoGD)
Discr.$call
Discr.

###Lista dos termos existentes no modelo####
names(Discr.)


##Propor��o de cada grupo nas observa�oes de treino####
Discr.$prior

###M�dia de cada variavel nos dois grupos#####
Discr.$means

#Histograma dos grupos###
plot(Discr.)

Previs�o <- Discr. %>% predict(testeGD)

###Lista dos termos existentes no modelo####
names(Previs�o)

head(Previs�o$class)
head(Previs�o$posterior)


###PRecis�o de 75,7% do modelo###
mean(Previs�o$class==testeGD$german_data.response)


###Coluna com a probadilidade e o resultado da previs�o, previs�o inicial � que quando o valor for maior que 50%
##o resultado � bom###

testeGD %>% mutate(Discr.Prob = Previs�o$posterior[,2],
                       Discr.Pred = Previs�o$class) %>%
  as_tibble() -> testeGD; testeGD

testeGD$Resultado = if_else(testeGD$german_data.response==testeGD$Discr.Pred,
                                 if_else(testeGD$german_data.response=="Bom",100,0),
                                 if_else(testeGD$german_data.response=="Ruim",-500,-100))

#Verificando resultado acumulado total:
sum(testeGD$Resultado)                                          
#Resumo dos resultados obtidos:
table(testeGD$Resultado)

#Criando uma chave primária para melhor verificação:
testeGD=testeGD%>%mutate(id = row_number())

#Criando coluna com o Valor acumulado:
testeGD=testeGD %>% mutate(VALOR_ACC = cumsum(Resultado))

#verificando o melhor resultado de corte no modelo: Ocorreu no ID 106 com valor de 2400
max(testeGD$VALOR_ACC)

ctable(x = testeGD$german_data.response, y = testeGD$Discr.Pred)
confusionMatrix


# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)

# Label observado
Y_OBS <- testeGD_LOG2$german_data.response

# Label previsto usando: 
#       se PROB > 50% -> 1 (Yes)
#       se PROB > 30% -> 1 (Yes)
Y_CLAS1 <- factor(ifelse(testeGD_LOG2$Y_VAL_TEST_LOG > 0.5,"Bom","Ruim"),
                  levels = c("Ruim","Bom"),
                  labels = c('Ruim','Bom')) 

confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Bom')

write.csv(testeGD,"C:/Users/rdsf/OneDrive/�rea de Trabalho/GV/Modelagem estat�stica avan�ada/Trabalho/ lll.csv")
