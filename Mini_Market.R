setwd('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Métodos Matriciais/Trabalhos')


rm(list = ls())

Arquivo_Inicial = read_excel("minimarket.xlsx")

Quant. = as.data.frame(c(Arquivo_Inicial[,4:7]))
Quant_Padr = as.data.frame(scale(Quant.))

Quali = as.data.frame(c(Arquivo_Inicial[,2:7]))
Quali
               

view(dfSummary(Quali))       
Quali %>% group_by(zona) %>% summarise(n = n(), media = mean(lucro))                      

boxplot(Arquivo_Inicial$lucro ~ Arquivo_Inicial$zona)
par(mfrow = c(1,2))
boxplot(Arquivo_Inicial$lucro, main = "Box Plot - Lucro")
hist(Arquivo_Inicial$lucro, main = "Histograma - Lucro")

boxplot(Arquivo_Inicial$faturamento, main = "Box Plot - Faturamento")
hist(Arquivo_Inicial$faturamento,main = "Histograma - Faturamento")

boxplot(Arquivo_Inicial$metas, main = "Box Plot - Metas")
hist(Arquivo_Inicial$metas,main = "Histograma - Metas")

boxplot(Arquivo_Inicial$publ, main = "Box Plot - Publicidade")
hist(Arquivo_Inicial$publ,main = "Histograma - Publicidade")

summary(Arquivo_Inicial)

summary(Arquivo_Inicial$lucro)
summary(Arquivo_Inicial$faturamento)
summary(Arquivo_Inicial$metas)
summary(Arquivo_Inicial$publ)

####Matriz de Correlação####

correl1 = cor(Quant.)
correl1

correl2 = cor(Quant_Padr)
correl2

print(correl1, digits = 2)
print(correl2, digits = 2)


###Padronização####

Padr. = scale(Quant.)
head(Padr.)

###Hierarquico aglomerativo###


distancias=dist(Quant_Padr)
clust=hclust(distancias, method = "ward.D2")
clust1=hclust(distancias, method = "average")
plot(clust, hang=-1, main="MiniMarket - Ward.D2")
plot(clust1, hang=-1, main="MiniMarket - Average")


par(mfrow=c(1,4))
boxplot(Quali$lucro~Quali$hc, col=2, main="Lucro")
boxplot(Quali$faturamento~Quali$hc, col=3, main="Faturamento")
boxplot(Quali$metas~Quali$hc, col=4, main="Metas")
boxplot(Quali$publ~Quali$hc, col=5, main="Publicidade")


###Hierarquico aglomerativo com quali###

Quali$zona=as.factor(Quali$zona)
Quali$idade=as.factor(Quali$idade)

par(mfrow = c(1,1))

dist.mix=daisy(Quali)
hcQuali=hclust(dist.mix, method = "ward.D2")
plot(hcQuali,hang = -1, main = "MiniMarket (Quali. e Quant.) - Ward.D2")
abline(h = 1.5, col = 2)

Quali$hc = cutree(hcQuali,2)
nb=NbClust(hcQuali,method = "ward.D2", index = "all" )

par(mfrow=c(1,4))
boxplot(Quali$lucro~Quali$hc, col=2, main="Lucro")
boxplot(Quali$faturamento~Quali$hc, col=3, main="Faturamento")
boxplot(Quali$metas~Quali$hc, col=4, main="Metas")
boxplot(Quali$publ~Quali$hc, col=5, main="Publicidade")


> aggregate(us[,2:4],list(us$hc), median)

> aggregate(Quali[,3:6],list(Quali$hc), median)
aggregate(Quali[,3:6],list(Quali$hc), mean)
aggregate(Quali[,3:6],list(Quali$hc), min)
aggregate(Quali[,3:6],list(Quali$hc), max)

library(NbClust)
nb=NbClust(data=Quant_Padr,diss= distancias, distance = NULL, 
           min.nc = 2,max.nc = 8,method = "ward.D2", index = "all" )

###k-medoids###

kmd=pamk(dist.mix, diss = T, k=2:6, critout = T)
kmd$nc



set.seed(1986)

kmd2=pamk(dist.mix, diss = T, k=2:6, critout = T)
kmd2$nc

table(Quali$hc,Quali$zona)
table(Quali$hc,Quali$idade)

table(Quali$zona,Quali$hc)
table(Quali$idade,Quali$hc)

###Rand Index###

boxplot(salario$Salary ~ salario$Sex, data = salario , main = "Box Plot - Salário Inicial x Gênero", 
        cex.main = 1.2, xlab = "Gênero", ylab = "Salário Inicial", cex.axis = 1.2,  horizontal = F, 
        ylim = c(60000,250000), col = c("blue1","red1"), border = "black")
legend("topright", legend = c("1 = Masculino","2 = Feminino"),fill = c("blue1","red1"))