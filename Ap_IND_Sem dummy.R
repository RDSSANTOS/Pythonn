library(rms)
library(rpart.plot)
library(pROC)
library(Metrics)
library(stringr)
library(summarytools)
library(rapportools)
library(lubridate)
library(hmeasure)
library(e1071)
library(dplyr)
library(car)
library(caret)
library(rattle)
library(randomForest)

setwd('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Analise Preditiva/Individual')

Arquivo_cheio = read.csv("uber.csv", sep = ",", dec = ",", header = T, stringsAsFactors = T)

Arquivo_base = read.csv("Base - Virgulas.csv", sep = ";",header =  T, stringsAsFactors = F)

Arquivo_bqm = read.csv("Base - BQM.csv", sep = ";", dec = "," ,header =  T, stringsAsFactors = T)

Brook = read.csv("Brooklyn.csv", sep = ";", dec = "," ,header =  T, stringsAsFactors = T)
Manh = read.csv("Manhatan.csv", sep = ";", dec = "," ,header =  T, stringsAsFactors = T)
Queen = read.csv("Queen.csv", sep = ";", dec = "," ,header =  T, stringsAsFactors = T)


graphics.off()
rm(list = ls())



##### Arvore com BQM_Full###

set.seed(123)


INDEX_TRAIN <- createDataPartition(Arquivo_bqm$pickups, p = 0.7, list = F)
TRAIN_SET <- Arquivo_bqm[INDEX_TRAIN, ] 
TEST_SET  <- Arquivo_bqm[-INDEX_TRAIN,]

summary(TRAIN_SET$pickups); summary(TEST_SET$pickups)

MDL_FIT <- rpart(pickups ~.,
                 data = TRAIN_SET,
                 method = 'anova',
                 control = rpart.control(minbucket = 10, cp = -1))

# saida da arvore
MDL_FIT
summary(MDL_FIT)

# avaliando a necessidade da poda da arvore
printcp(MDL_FIT)
plotcp(MDL_FIT)

# aqui conseguimos podar a Arvore controlando o cp que reduz o valor minimo do 
# erro, que Ã© um parametro de controle

PARM_CTRL <- MDL_FIT$cptable[which.min(MDL_FIT$cptable[,"xerror"]),"CP"]

MDL_FIT.PRUNE <- prune(MDL_FIT, cp = PARM_CTRL)

# saida da Arvore
MDL_FIT.PRUNE
summary(MDL_FIT.PRUNE)

plotcp(MDL_FIT.PRUNE)
fancyRpartPlot(MDL_FIT.PRUNE)

rpart.plot(MDL_FIT.PRUNE, 
           cex = 0.5, 
           type = 3,
           box.palette = "BuRd",
           branch.lty = 3, 
           shadow.col ="gray", 
           nn = TRUE,
           main = 'Regression Trees')

rpart.plot(MDL_FIT.PRUNE,
           type = 3, 
           cex = 0.5, 
           clip.right.labs = FALSE,
           branch = .4,
           box.palette = "BuRd",       # override default GnBu palette
           main = 'Regression Trees')


predict
#Realizando as predicoes

# Valor de AMOUNT pela arvore regressao com maior desenvolvimento
Y_VAL_TRAIN <- predict(MDL_FIT.PRUNE) 
Y_VAL_TEST  <- predict(MDL_FIT.PRUNE, newdata = TEST_SET)

#Analise dos erros##
postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET$pickups)
postResample(pred = Y_VAL_TEST,  obs = TEST_SET$pickups)

mse(Y_VAL_TRAIN, TRAIN_SET$pickups)
mse(Y_VAL_TEST, TEST_SET$pickups)

sse(Y_VAL_TRAIN, TRAIN_SET$pickups)
sse(Y_VAL_TEST, TEST_SET$pickups)

MDL_FINAL <- MDL_FIT.PRUNE
round(MDL_FINAL$variable.importance, 3)
varImp(MDL_FINAL)

# Convertendo a variavel para unidade original

RESULT_TRAIN <- data.frame(AMOUNT_OBS  = TRAIN_SET$pickups,
                           AMOUNT_PRED = Y_VAL_TRAIN) %>%
  mutate(RESIDUO = AMOUNT_PRED - AMOUNT_OBS)

RESULT_TEST  <- data.frame(AMOUNT_OBS  = TEST_SET$pickups,
                           AMOUNT_PRED = Y_VAL_TEST) %>%
  mutate(RESIDUO = AMOUNT_PRED - AMOUNT_OBS)

--------------------------------------------------------------------------------

##### Random FOrest com BQM_Full###
  
  
INDEX_TRAIN <- createDataPartition(Arquivo_bqm$pickups, p = 0.7, list = F)
TRAIN_SET_RF <- Arquivo_bqm[INDEX_TRAIN, ] 
TEST_SET_RF  <- Arquivo_bqm[-INDEX_TRAIN,]
  

MDL_FIT_RF <- randomForest(pickups ~ .,
                        data = TRAIN_SET_RF,
                        importance = T,
                        mtry       = 4,
                        nodesize   = 5, 
                        ntree      = 1000)

MDL_FIT_RF
plot(MDL_FIT_RF, main = 'Out-of-bag error')

getTree(MDL_FIT_RF, k = 500, labelVar=TRUE)

# a partir de 300 arvores o erro estabiliza e nao ha mais melhoria

MDL_FIT_RF <- randomForest(pickups ~ .,
                           data = TRAIN_SET_RF,
                           importance = T,
                           mtry       = 4,
                           nodesize   = 5, 
                           ntree      = 300)

plot(MDL_FIT_RF, main = 'Out-of-bag error')

#Realizando as predicoes

Y_VAL_TRAIN_RF <- predict(MDL_FIT_RF) 
Y_VAL_TEST_RF  <- predict(MDL_FIT_RF, newdata = TEST_SET_RF)

###Avaliando a performance dos modelos e existencia de overfitting

postResample(pred = Y_VAL_TRAIN_RF, obs = TRAIN_SET_RF$pickups)
postResample(pred = Y_VAL_TEST_RF,  obs = TEST_SET_RF$pickups)

mse(Y_VAL_TRAIN_RF, TRAIN_SET_RF$pickups)
mse(Y_VAL_TEST_RF, TEST_SET_RF$pickups)

sse(Y_VAL_TRAIN_RF, TRAIN_SET_RF$pickups)
sse(Y_VAL_TEST_RF, TEST_SET_RF$pickups)


# sinais de overfitting entre as amostras de treino e teste? 
MDL_FINAL_RF <- MDL_FIT_RF

# 5) Importancia das variaveis (Modelo final)
#varImp(MDL_FINAL_RF)

# o pacote random forest também possui uma forma de ver a importancia das 
# variaveis
varImpPlot(MDL_FINAL_RF, sort= T, main = 'Importancia das Variaveis')

MDL_FINAL_RF$importance

varImp(MDL_FINAL_RF, sort = T)
# 6) Inspecao dos valores previstos vs observados (modelo final)


RESULT_TRAIN_RF <- data.frame(AMOUNT_OBS  = TRAIN_SET_RF$pickups,
                           AMOUNT_PRED = Y_VAL_TRAIN_RF) %>%
  mutate(RESIDUO = AMOUNT_PRED - AMOUNT_OBS)

RESULT_TEST_RF  <- data.frame(borough = TEST_SET_RF$borough,AMOUNT_OBS  = TEST_SET_RF$pickups,
                           AMOUNT_PRED = Y_VAL_TEST_RF) %>%
  mutate(RESIDUO = AMOUNT_PRED - AMOUNT_OBS)


data.frame(RESULT_TEST_RF,RESULT_TRAIN_RF)


Y_VAL_TEST_RF2  <- predict(a$pickups)
a = as.list(Arquivo_bqm)