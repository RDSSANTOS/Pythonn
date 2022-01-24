##Arquivos###

setwd('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Métodos Matriciais/Trabalhos')


## EX1 - Componente Principal###

c = read_excel("Cars.xlsx")


pca = data.frame(c[3:10])
pca

pca_padronizado = scale(pca)
pca_padronizado

comp1 = prcomp(pca, scale = T)
plot(comp1)

comp1$x
summary(comp1)

comp2 = prcomp(pca_padronizado)
plot(comp2)

comp2$x
summary(comp2)

quatrocomp = comp1$x[,1:4]

round(cor(pca, quatrocomp),5)

round(cor(pca_padronizado, quatrocomp),5)

##c-E##############

duascomp = comp1$x[,1:2]

duascomp_Padr = scale(duascomp)

dist_duascomp = dist(duascomp_Padr)

set.seed(11)

Kmeans = kmeans(duascomp_Padr,2, nstart = 25)

Kmeans$size
Kmeans$centers


cor(c[3:10])

cor(pca)
cor1 = round(cor(pca_padronizado),3)

auto = eigen(cor1)

auto

inv= round(solve(cor1),3)
inv


v1 = eigen(cor1)$vector[,1]
v2 = eigen(cor1)$vector[,2]
v3= eigen(cor1)$vector[,3]
v4= eigen(cor1)$vector[,4]
v5= eigen(cor1)$vector[,5]
v6= eigen(cor1)$vector[,6]
v7= eigen(cor1)$vector[,7]
v8= eigen(cor1)$vector[,8]



###F###

x=c(-0.17413241, -0.36753180,  0.01375612, -0.68214295, -0.52446332,  -0.57510432, 
    -0.79494180,  0.32461643) 
x

MM = round((cor1%*%x),3)
MM
eigen(MM)

det(cor1)
det(round(cor1),2)

sum(MM)
xx = diag(x)

cor1%*%x

solve(cor1, x)
t(cor1)%*%cor1

## EX2 - Cluster ###

library(cluster)
library(gmodels)
library(fpc)
library(NbClust)


dd = data.frame(c$weight,c$Length,c$Braking,c$Cylinders,c$City)

padr. = scale(dd)
padr.
head(padr.)



dist. = dist(padr.)

clust. = hclust(dist., method = "ward.D2")
plot(clust.)

dd$hcl = cutree(clust.,2)

dist(padr., method = "manhattan")


e = data.frame(c$CODE)


boxplot(dd$c.weight~dd$hcl, main= 'weight', col=topo.colors(4))

set.seed(11)
clustpamk = pamk(padr., krange = 3, diss = T)
clustpamk$nc
clustpamk$pamobject$medoids

dd$pamk = clustpamk$pamobject$clustering
table(dd$pamk)

boxplot(dd$c.weight~dd$pamk, main= 'weight', col=topo.colors(4))

disp = c[,7]

dispp = scale(disp)

kk = as.data.frame(dispp)
kk1 =  as.data.frame(cbind(dispp,dd$pamk)

boxplot(kk1$Displacement ~ kk1$V2, main= 'weight', col=topo.colors(4))

## EX3 - Regras de associação ###

library(arules)
library(arulesViz)

assoc =  read_excel("Assoc.xlsx")

ww = data.frame(assoc[,2:11])
ww

class(ww)

ww_matrix = as.matrix(ww)
class(ww_matrix)

ww_tr = as(ww_matrix,"transactions")
inspect(ww_tr)
summary(ww_tr)

itemFrequency(ww_tr) # suporte de cada item 
itemFrequencyPlot(ww_tr, col=topo.colors(2)) #nÃ£o ordena
itemFrequencyPlot(ww_tr,topN=10, main="item support",ylim=c(0,.8), col=topo.colors(10));grid(col=3)

regras = apriori(ww_tr)
regras
inspect(sort(regras, by = "conf")
View(regras)

regras2 = apriori(data = ww_tr, parameter = list(supp=0.2,conf=0.8, minlen = 2, maxlen = 4, target = "rules"))
regras2  
inspect(regras2)                 
  rules=apriori(data = cbc.tr, parameter = list(supp=.05,conf=0.6, 
                                                minlen=2, maxlen=7, target="rules" ))
  
##Eliminando Redundantes####
  
inspect(regras2[is.redundant(regras2)])
  redundante = regras2[is.redundant(regras2, measure = "Confidence")]
redundante  
  
  inspect(rules[is.redundant(rules)]) # output em branco se nao houver regras redundantes
  rules.pruned=rules[!is.redundant(rules, measure="confidence")]
  rules.pruned
  inspect(sort(rules.pruned, by="conf"))
  

rb = apriori(data = ww_tr, parameter = list(supp = 0.2, conf = 0.6, minlen=2, maxlen=4,target="rules"), 
             appearance = list(rhs="B"))
rb

inspect(sort(rb, by = "confidence"))
                 
rules.child=apriori(data = cbc.tr, 
                        parameter = list(supp=.05,conf=0.6, minlen=2, maxlen=5,  target="rules"), 
                        appearance = list(rhs="ChildBks"))