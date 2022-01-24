#Trabalho Final - Modelagem estatística avançada
#Alunos:
# Bruno PIoli
# Guilherme Alves Alvarenga
# Igor Anselmo
# João Pedro Dannemann
# Ricardo Santos
##################

#Bibliotecas iniciais
library(readr)
library(ggplot2)
library(dplyr)
library(caret)
library(summarytools)


#Parte 1: Definindo a base de dados:
rm(list = ls())
#obtendo a tabela origem
setwd("~/MBA/4. Modelagem estatística avançada/Trabalho")
german_data <- read_delim("german_data.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

#Selecionando as variÃ¡veis de maneira intuitiva
GD = german_data%>%select(-c(present_residence,age,telephone,foreign))
names(GD)
head(GD)
#Separando os dados em treino e teste
set.seed(123)
treino = sample(dim(GD)[1], dim(GD)[1]*0.7); treino
treinoGD = GD[treino,]%>%as_tibble()
testeGD = GD[-treino,]%>%as_tibble()
head(treinoGD)
head(testeGD)

#Verificando correÃ§Ã£o das variÃ¡veis contÃ�nuas
GDnum=treinoGD%>%select(duration,amount,install_rate,num_credits,num_dependents,response)%>%cor()
GDnum
#variável amount e duration tem correlação superior a 60%,
#é retirada a variável com menor correlação com a resposta (AM)
treinoGD=treinoGD%>%select(-amount)
testeGD=testeGD%>%select(-amount)
#Primeiro adicionando colunas com nomes "Bom" e "Ruim" para variável resposta (na base de testes apenas)
testeGD=testeGD%>%mutate(resp = if_else(response==1,"Bom","Ruim"))
#Para as próximas etapas será criada um backup da base de testes para facilitar a limpeza.
testeGDBKP=testeGD

#Parte 2: Modelo de REGRESSAO LINEAR DISCRIMINANTE#####
library(MASS)
Discr. = lda(response ~., data = treinoGD)
#Proporção de cada grupo nas observaçoes de treino#
Discr.$prior
###Média de cada variavel nos dois grupos#
Discr.$means
#Histograma dos grupos#
plot(Discr.)
#Realizando a previsão nos dados de teste:
Previsão <- Discr. %>% predict(testeGD)
#Calculando a precisão do modelo:
mean(Previsão$class==testeGD$response)
#Precisão de 75,67%

#Realizando primeiro teste, com previsão indicando probabilidade superior a 50% sendo bom.


#Colocando a probabilidade dentro da base teste
testeGD %>% mutate(Discr.Prob = Previsão$posterior[,2],
                       Discr.Pred = Previsão$class) %>%
  as_tibble() -> testeGD; testeGD
#Nomeando as previsões obtidas com corte de 50% em "Bom" e "Ruim"
testeGD=testeGD%>%mutate(Discr.Pred = if_else(Discr.Pred==1,"Bom","Ruim"))

#Aplicando os valores conforme 
testeGD$Resultado = if_else(testeGD$resp==testeGD$Discr.Pred,
                                 if_else(testeGD$resp=="Bom",100,0),
                                 if_else(testeGD$resp=="Ruim",-500,-100))
#Resumo dos resultados obtidos:
table(testeGD$Resultado)

#Verificando resultado acumulado total:
sum(testeGD$Resultado)                                          
#Resultado total - Prejuízo de R$7.700, o peso da aceitação de créditos ruins é muito considerável.


#Criando uma chave primária para melhor verificação:
testeGD=testeGD%>%mutate(id = row_number())

#Criando coluna com o Valor acumulado:
testeGD=testeGD %>% mutate(VALOR_ACC = cumsum(Resultado))

#verificando o melhor resultado de corte no modelo: Ocorreu no ID 106 com valor de R$2.400
max(testeGD$VALOR_ACC)

#matriz de confusão
ctable(x = testeGD$resp, y = testeGD$Discr.Pred)

# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)

# Label observado
Y_OBS <- as.factor(testeGD$resp)
Y_OBS
#corte inicial prox. 0 para não gerar valores nulos
Corte=0.01

Y_CLAS1 <- factor(ifelse(testeGD$Discr.Prob < Corte,"Bom","Ruim"),
                  levels = c("Ruim","Bom"),
                  labels = c('Ruim','Bom')) 
x=confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Bom')
#Variáveis que entrarão na tabela comparativa:
Acurária = c(x$overall[['Accuracy']])
Precisão = x$byClass[['Precision']]
Revocação = x$byClass[['Recall']]
F1=x$byClass[['F1']]
PBom_RBom=c(x$table[1,1])
PRuim_RBom=c(x$table[2,1])
PBom_RRuim=c(x$table[1,2])
PRuim_RRuim=c(x$table[2,2])
Resultado_Teste=c(100*PBom_RBom-100*PRuim_RBom-500*PBom_RRuim)
Comp_Corte=data.frame(Corte,Acurária,Precisão,Revocação,F1,PBom_RBom,PRuim_RBom,PBom_RRuim,PRuim_RRuim,Resultado_Teste)

#Loop variando o corte em 1% e criando tabela comparativa
for (i in 1:100){
  Corte = i/100
  Y_CLAS1 <- factor(ifelse(testeGD$Discr.Prob < Corte,"Bom","Ruim"),levels = c("Ruim","Bom"),labels = c('Ruim','Bom')) 
  x=confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Bom') 
  Acurária = c(x$overall[['Accuracy']])
  Precisão = x$byClass[['Precision']]
  Revocação = x$byClass[['Recall']]
  F1=x$byClass[['F1']]
  PBom_RBom=c(x$table[1,1])
  PRuim_RBom=c(x$table[2,1])
  PBom_RRuim=c(x$table[1,2])
  PRuim_RRuim=c(x$table[2,2])
  Resultado_Teste=c(100*PBom_RBom-100*PRuim_RBom-500*PBom_RRuim)
  Novo_Corte=data.frame(Corte,Acurária,Precisão,Revocação,F1,PBom_RBom,PRuim_RBom,PBom_RRuim,PRuim_RRuim,Resultado_Teste)
  Comp_Corte= rbind(Comp_Corte,Novo_Corte)}
#A tabela Comp_Corte traz o resumo dos resultados.
#Salvando bases:         
write.csv(testeGD,"C:/Users/pioli/OneDrive/Documentos/MBA/4. Modelagem estatística avançada/Trabalho/RLD.csv")
write.csv(Comp_Corte,"C:/Users/pioli/OneDrive/Documentos/MBA/4. Modelagem estatística avançada/Trabalho/Corte_RLD.csv")

Comp_Corte=NA
Previsão=NA
x=NA
#Parte 3: MNULLo de Regressão Logística:##########

#Primeiro recuperamos a base de testes do backup:
testeGD=testeGDBKP

#Recaptiulando as variáveis usadas:
names(treinoGD)
#Transformando a variável Response em Factor:
treinoGD = treinoGD %>% mutate(response = factor(response,
                                                        levels = c(2,1),
                                                        labels = c('Ruim','Bom')))
str(treinoGD)

#Realizando a regressão Logística
library(car)
Logistica1 <- glm(response ~ ., data= treinoGD, family = binomial(link='logit'))

#Análise do VIF na regressão, para veriricar a multicolinearidade 
vif(Logistica1) 

#Retirada da variável Real_state pelo alto VIF (>5)

treinoGD=treinoGD%>%dplyr::select(-real_state)
testeGD=testeGD%>%dplyr::select(-real_state)

#Nova regressão sem a variável retirada:
Logistica2 = glm(response ~ ., data= treinoGD, family = binomial(link='logit'))
vif(Logistica2) 

#Não há mais multicolinearidade, obtendo agora o melhor AIC:
Logistica3 <- stepAIC(Logistica2,direction = 'both', trace = TRUE); vif(Logistica3)
Logistica3
vif(Logistica3)
#Gerando os dados de previsão nas bases de treino e teste Logistica:
set.seed(123)
Y_VAL_TRAIN_LOG <- predict(Logistica3, type = 'response') 
Y_VAL_TEST_LOG  <- predict(Logistica3, newdata = testeGD, type = 'response')

#Implementando o resultado da previsão na base de teste:
testeGD=testeGD%>%mutate(Log.Prob=Y_VAL_TEST_LOG)

#Probabilidade Inicial
testeGD$Pred = if_else(Y_VAL_TEST_LOG>0.5,"Bom","Ruim") 

testeGD$Resultado = if_else(testeGD$resp==testeGD$Pred,
                                 if_else(testeGD$resp=="Bom",100,0),
                                 if_else(testeGD$resp=="Ruim",-500,-100))


#Verificando resultado acumulado total:
sum(testeGD$Resultado)                                          
#Resumo dos resultados obtidos:
table(testeGD$Resultado)

#Criando uma chave primária para melhor verificação:
testeGD=testeGD%>%mutate(id = row_number())

#Criando coluna com o Valor acumulado:
testeGD=testeGD %>% mutate(VALOR_ACC = cumsum(Resultado))
#verificando o melhor resultado de corte no modelo (item 28 de 300, com R$1.900 de resultado)
max(testeGD$VALOR_ACC)

# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)
# Label observado
Y_OBS <- as.factor(testeGD$resp)
Y_OBS
#corte inicial prox. 0 para não gerar valores nulos
Corte=0.1

Y_CLAS1 <- factor(ifelse(testeGD$Log.Prob > Corte,"Bom","Ruim"),
                  levels = c("Ruim","Bom"),
                  labels = c('Ruim','Bom')) 

x=confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Bom')
#Variáveis que entrarão na tabela comparativa:
Acurária = c(x$overall[['Accuracy']])
Precisão = x$byClass[['Precision']]
Revocação = x$byClass[['Recall']]
F1=x$byClass[['F1']]
PBom_RBom=c(x$table[1,1])
PRuim_RBom=c(x$table[2,1])
PBom_RRuim=c(x$table[1,2])
PRuim_RRuim=c(x$table[2,2])
Resultado_Teste=c(100*PBom_RBom-100*PRuim_RBom-500*PBom_RRuim)
Comp_Corte=data.frame(Corte,Acurária,Precisão,Revocação,F1,PBom_RBom,PRuim_RBom,PBom_RRuim,PRuim_RRuim,Resultado_Teste)

#Loop variando o corte em 1% e criando tabela comparativa
for (i in 1:100){
  Corte = i/100
  Y_CLAS1 <- factor(ifelse(testeGD$Log.Prob > Corte,"Bom","Ruim"),levels = c("Ruim","Bom"),labels = c('Ruim','Bom')) 
  x=confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Bom') 
  Acurária = c(x$overall[['Accuracy']])
  Precisão = x$byClass[['Precision']]
  Revocação = x$byClass[['Recall']]
  F1=x$byClass[['F1']]
  PBom_RBom=c(x$table[1,1])
  PRuim_RBom=c(x$table[2,1])
  PBom_RRuim=c(x$table[1,2])
  PRuim_RRuim=c(x$table[2,2])
  Resultado_Teste=c(100*PBom_RBom-100*PRuim_RBom-500*PBom_RRuim)
  Novo_Corte=data.frame(Corte,Acurária,Precisão,Revocação,F1,PBom_RBom,PRuim_RBom,PBom_RRuim,PRuim_RRuim,Resultado_Teste)
  Comp_Corte= rbind(Comp_Corte,Novo_Corte)}
#A tabela Comp_Corte traz o resumo dos resultados.

#Salvando bases:         
write.csv(testeGD,"C:/Users/pioli/OneDrive/Documentos/MBA/4. Modelagem estatística avançada/Trabalho/LOG.csv")
write.csv(Comp_Corte,"C:/Users/pioli/OneDrive/Documentos/MBA/4. Modelagem estatística avançada/Trabalho/Corte_LOG.csv")
