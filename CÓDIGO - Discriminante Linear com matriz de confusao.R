library(readr)
library(ggplot2)
library(dplyr)
library(car)
library(summarytools)
library(pROC)
library(readxl)
library(MASS)

rm(list = ls())

setwd('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Modelagem estatística avançada/Trabalho')
german_data = read_excel("Base - German.xlsx")

# Tratando a variável resposta para fator
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

###Exclusão de Amount###
treinoGD=data.frame(treinoGD[,-5])
testeGD=data.frame(testeGD[,-5])

####REGRESSAO LINEAR DISCRIMINANTE#####

Discr. = lda(german_data.response ~., data = treinoGD)
Discr.$call
Discr.

###Lista dos termos existentes no modelo####
names(Discr.)


##Proporção de cada grupo nas observaçoes de treino####
Discr.$prior

###Média de cada variavel nos dois grupos#####
Discr.$means

#Histograma dos grupos###
plot(Discr.)

Previsão <- Discr. %>% predict(testeGD)

###Lista dos termos existentes no modelo####
names(Previsão)

head(Previsão$class)
head(Previsão$posterior)


###PRecisão de 75,7% do modelo###
mean(Previsão$class==testeGD$german_data.response)


###Coluna com a probadilidade e o resultado da previsão, previsão inicial é que quando o valor for maior que 50%
##o resultado é bom###

testeGD %>% mutate(Discr.Prob = Previsão$posterior[,2],
                       Discr.Pred = Previsão$class) %>%
  as_tibble() -> testeGD; testeGD

testeGD$Resultado = if_else(testeGD$german_data.response==testeGD$Discr.Pred,
                                 if_else(testeGD$german_data.response=="Bom",100,0),
                                 if_else(testeGD$german_data.response=="Ruim",-500,-100))

#Verificando resultado acumulado total:
sum(testeGD$Resultado)                                          
#Resumo dos resultados obtidos:
table(testeGD$Resultado)

#Criando uma chave primÃ¡ria para melhor verificaÃ§Ã£o:
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

write.csv(testeGD,"C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Modelagem estatística avançada/Trabalho/ lll.csv")
