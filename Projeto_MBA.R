mba = read.csv('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Inferencia Estatistica/Aula 4/Case MBA_Salaries.csv',
        sep = ";",header = T,stringsAsFactors = T)

satis = read.csv('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Inferencia Estatistica/Aula 4/Satis.csv',
                 sep = ";",header = T,stringsAsFactors = T)

salario = read.csv('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Inferencia Estatistica/Aula 4/Salaries.csv',
                 sep = ";",header = T,stringsAsFactors = T)

salario2 = read.csv('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Inferencia Estatistica/Aula 4/Salaries_2.csv',
                    sep = ";",header = T,stringsAsFactors = T)

regreSex = read.csv('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Inferencia Estatistica/Aula 4/Reg - Gmat_Sex.csv',
                    sep = ";",header = T,stringsAsFactors = T)

regreIng = read.csv('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Inferencia Estatistica/Aula 4/Reg - Gmat_Frstlang.csv',
                    sep = ";",header = T,stringsAsFactors = T)

regreSal = read.csv('C:/Users/rdsf/OneDrive/Área de Trabalho/GV/Inferencia Estatistica/Aula 4/Reg - Salaries_Gend.csv',
                    sep = ";",header = T,stringsAsFactors = T)

mba
satis
salario


View(mba)
View(satis)
View(salario)
View(regreSex)
View(regreIng)
View(salario2)
View(regreSal)

hist(satis$Satis, main = "Histograma - Satisfação", xlab = "Satisfação com o MBA", breaks = c(0,1,2,3,4,5,6,7),
     ylim = c(0,110),
     border = "black", col = "blue", labels = TRUE)


s = mean(satis$Satis)
s

freq(satis$Satis)

text(frequency(satis$Satis))


table(satis$Satis)

freq(satis$Satis)
descr(satis$Satis)

view(freq(satis$Satis), report.nas = F, prop = "C")


str(mba)

freq(mba)

dfSummary(mba)

view(dfSummary(mba))

mba %>% group_by(Frstlang) %>% summarise(N = n(), 
                                    Média = mean(Gmat_tot),
                                    Mediana = median(Gmat_tot),
                                    Mín = min(Gmat_tot), 
                                    Máx = max(Gmat_tot),
                                    Amp = max(Gmat_tot) - min(Gmat_tot),
                                    Q1 = quantile(Gmat_tot,0.25),
                                    Q3 = quantile(Gmat_tot, 0.75),
                                    Desv.Pad. = sd(Gmat_tot),
                                    Var. = var(Gmat_tot),
                                    Iqr = IQR(Gmat_tot))

mba %>% group_by(Frstlang) %>% summarise(N = n(), 
                                         Média = mean(Gmat_vpc),
                                         Mediana = median(Gmat_vpc),
                                         Mín = min(Gmat_vpc), 
                                         Máx = max(Gmat_vpc),
                                         Amp = max(Gmat_vpc) - min(Gmat_vpc),
                                         Q1 = quantile(Gmat_vpc,0.25),
                                         Q3 = quantile(Gmat_vpc, 0.75),
                                         Desv.Pad. = sd(Gmat_vpc),
                                         Var. = var(Gmat_vpc),
                                         Iqr = IQR(Gmat_vpc))

mba %>% group_by(Frstlang) %>% summarise(N = n(), 
                                         Média = mean(Gmat_tpc),
                                         Mediana = median(Gmat_tpc),
                                         Mín = min(Gmat_tpc), 
                                         Máx = max(Gmat_tpc),
                                         Amp = max(Gmat_tpc) - min(Gmat_tpc),
                                         Q1 = quantile(Gmat_tpc,0.25),
                                         Q3 = quantile(Gmat_tpc, 0.75),
                                         Desv.Pad. = sd(Gmat_tpc),
                                         Var. = var(Gmat_tpc),
                                         Iqr = IQR(Gmat_tpc))

salario %>% group_by(Sex) %>% summarise(N = n(), 
                                        Média = mean(Salary),
                                        Mediana = median(Salary),
                                        Mín = min(Salary), 
                                        Máx = max(Salary),
                                        Amp = max(Salary) - min(Salary),
                                        Q1 = quantile(Salary,0.25),
                                        Q3 = quantile(Salary, 0.75),
                                        Desv.Pad. = sd(Salary),
                                        Var. = var(Salary),
                                        Iqr = IQR(Salary))

salario2 %>% group_by(Sex) %>% summarise(N = n(), 
                                        Média = mean(Salary),
                                        Mediana = median(Salary),
                                        Mín = min(Salary), 
                                        Máx = max(Salary),
                                        Amp = max(Salary) - min(Salary),
                                        Q1 = quantile(Salary,0.25),
                                        Q3 = quantile(Salary, 0.75),
                                        Desv.Pad. = sd(Salary),
                                        Var. = var(Salary),
                                        Iqr = IQR(Salary))

salario %>% summarise(N = n(), 
          Média = mean(Salary),
          Mediana = median(Salary),
          Mín = min(Salary), 
          Máx = max(Salary),
          Amp = max(Salary) - min(Salary),
          Q1 = quantile(Salary,0.25),
          Q3 = quantile(Salary, 0.75),
          Desv.Pad. = sd(Salary),
          Var. = var(Salary),
          Iqr = IQR(Salary))

salario2 %>% summarise(N = n(), 
                      Média = mean(Salary),
                      Mediana = median(Salary),
                      Mín = min(Salary), 
                      Máx = max(Salary),
                      Amp = max(Salary) - min(Salary),
                      Q1 = quantile(Salary,0.25),
                      Q3 = quantile(Salary, 0.75),
                      Desv.Pad. = sd(Salary),
                      Var. = var(Salary),
                      Iqr = IQR(Salary))


mba %>% group_by(Frstlang) %>% summarise(n = n())
mba %>% group_by(Satis) %>% summarise(n = n())
mba %>% group_by(Salary) %>% summarise(n = n())

ctable(x = mba$Sex, y = mba$Satis, prop = "c")

boxplot(Gmat_tot ~ Sex, data = mba , main = "Box Plot - Gmat_Tot x Sex", 
        cex.main = 1.2, xlab = "Sex", ylab = "Gmat_Tot", cex.axis = 1.2,  horizontal = F, 
        ylim = c(400,800), col = c("dodgerblue","red"), border = "black")
legend("topright", legend = c("1 = Masc.","2 = Fem."),fill = c("dodgerblue","red"))

boxplot(Gmat_tot ~ Frstlang, data = mba , main = "Box Plot - Gmat_Tot x Frstlang", 
        cex.main = 1.2, xlab = "Frstlang", ylab = "Gmat_Tot", cex.axis = 1.2,  horizontal = F, 
        ylim = c(400,800), col = c("dodgerblue","red"), border = "black")
legend("topright", legend = c("1 = Inglês","2 = Outra"),fill = c("dodgerblue","red"))

points(mean(mba$Sex))

boxplot(Gmat_tot ~ Frstlang, data = mba , main = "Box Plot - Gmat_Tot x First Language", 
        cex.main = 2, xlab = "Frstlang", ylab = "Gmat_Tot", cex.axis = 1.2,  horizontal = F, 
        ylim = c(400,800), col = c("dodgerblue","dodgerblue4"), border = "gray20")


boxplot(salario$Salary ~ salario$Sex, data = salario , main = "Box Plot - Salário Inicial x Gênero", 
        cex.main = 1.2, xlab = "Gênero", ylab = "Salário Inicial", cex.axis = 1.2,  horizontal = F, 
        ylim = c(60000,250000), col = c("blue1","red1"), border = "black")
legend("topright", legend = c("1 = Masculino","2 = Feminino"),fill = c("blue1","red1"))

boxplot(salario2$Salary ~ salario2$Sex, data = salario , main = "Box Plot - Salário Inicial x Gênero", 
        cex.main = 1.2, xlab = "Gênero", ylab = "Salário Inicial", cex.axis = 1.2,  horizontal = F, 
        ylim = c(60000,165000), col = c("blue1","red1"), border = "black")
legend("topright", legend = c("1 = Masculino","2 = Feminino"),fill = c("blue1","red1"))




boxplot(salario$Salary ~ salario$Frstlang, data = salario , main = "Box Plot - Salario x First Language", 
        cex.main = 2, xlab = "Frstlang", ylab = "Gmat_Tot", cex.axis = 1.2,  horizontal = F, 
        ylim = c(60000,220000), col = c("dodgerblue","dodgerblue4"), border = "gray20")


c = prop.table(table(mba$Sex))
c

d = table(mba$Sex)
d

freqS = freq(mba$Sex,report.nas = F)
view(freqS)

freqA = freq(mba$Age, report.nas = F)
view(freqA)


barplot(c, main = "Proporção - Homens e Mulheres", cex.main = 2, cex.names = 1.2, xlab = "Sexo", ylab = "% Freq", cex.axis = 1.0,
        ylim = c(0,1), col = c("dodgerblue4","red"), border = "black")

legend("topright", legend = c("1 = Masc.","2 = Fem."),fill = c("dodgerblue4","red"))

text(c, y = mba$Sex, label = mba$Sex, pos = 3, cex = 0.8, col = "red")

hist(mba$Age)

plot(salario2$Age, salario2$Salary, main = "Work_Yrs x Salario",
     xlab = "Idade", ylab = "Tempo de Recuperação (Dias)", cex.axis = 1.2,
     pch = 19, cex = 0.8, col = "dodgerblue", ylim = c(0,200000))

plot(salario2$Salary, salario2$Age, main = "Salário x Idade",
     xlab = "Salário Inicial", ylab = "Idade", cex.axis = 1.2,
     pch = 19, cex = 0.8, col = "dodgerblue")

ab = plot(salario2$Salary, salario2$Age, main = "Salário x Idade",
     xlab = "Salário Inicial", ylab = "Idade", cex.axis = 1.2,
     pch = 19, cex = 0.8, col = "dodgerblue")
abline(lm(salario2$Salary ~ salario2$Age, data = salario2))

plot(salario2$Salary, salario2$Work_yrs, main = "Salário x Anos de trabalho",
     xlab = "Salário Inicial", ylab = "Anos de trabalho", cex.axis = 1.2,
     pch = 19, cex = 0.8, col = "dodgerblue")




mba %>% summarise(N = n(),Mín = min(mba$Age), 
                  Máx = max(mba$Age),
                  Amp = max(mba$Age) - min(mba$Age),
                  Média = mean(mba$Age), 
                  Mediana = median(mba$Age),
                  Q1 = quantile(mba$Age,0.25),
                  Q3 = quantile(mba$Age, 0.75),
                  Desv.Pad. = sd(mba$Age),
                  Var. = var(mba$Age),
                  Iqr = IQR(mba$Age))


            
mba %>% summarise(N = n(),Mín = min(mba$Gmat_tot), 
                  Máx = max(mba$Gmat_tot),
                  Amp = max(mba$Gmat_tot) - min(mba$Gmat_tot),
                  Média = mean(mba$Gmat_tot), 
                  Mediana = median(mba$Gmat_tot),
                  Q1 = quantile(mba$Gmat_tot,0.25),
                  Q3 = quantile(mba$Gmat_tot, 0.75),
                  Desv.Pad. = sd(mba$Gmat_tot),
                  Var. = var(mba$Gmat_tot),
                  Iqr = IQR(mba$Gmat_tot))

mba %>% summarise(N = n(),Mín = min(mba$Gmat_vpc), 
                  Máx = max(mba$Gmat_vpc),
                  Amp = max(mba$Gmat_vpc) - min(mba$Gmat_vpc),
                  Média = mean(mba$Gmat_vpc), 
                  Mediana = median(mba$Gmat_vpc),
                  Q1 = quantile(mba$Gmat_vpc,0.25),
                  Q3 = quantile(mba$Gmat_vpc, 0.75),
                  Desv.Pad. = sd(mba$Gmat_vpc),
                  Var. = var(mba$Gmat_vpc),
                  Iqr = IQR(mba$Gmat_vpc))


mba %>% summarise(N = n(),Mín = min(mba$Gmat_qpc), 
                  Máx = max(mba$Gmat_qpc),
                  Amp = max(mba$Gmat_qpc) - min(mba$Gmat_qpc),
                  Média = mean(mba$Gmat_qpc), 
                  Mediana = median(mba$Gmat_qpc),
                  Q1 = quantile(mba$Gmat_qpc,0.25),
                  Q3 = quantile(mba$Gmat_qpc, 0.75),
                  Desv.Pad. = sd(mba$Gmat_qpc),
                  Var. = var(mba$Gmat_qpc),
                  Iqr = IQR(mba$Gmat_qpc))

mba %>% summarise(N = n(),Mín = min(mba$Gmat_tpc), 
                  Máx = max(mba$Gmat_tpc),
                  Amp = max(mba$Gmat_tpc) - min(mba$Gmat_tpc),
                  Média = mean(mba$Gmat_tpc), 
                  Mediana = median(mba$Gmat_tpc),
                  Q1 = quantile(mba$Gmat_tpc,0.25),
                  Q3 = quantile(mba$Gmat_tpc, 0.75),
                  Desv.Pad. = sd(mba$Gmat_tpc),
                  Var. = var(mba$Gmat_tpc),
                  Iqr = IQR(mba$Gmat_tpc))

mba %>% summarise(N = n(),Mín = min(mba$Gmat_tpc), 
                  Máx = max(mba$Gmat_tpc),
                  Amp = max(mba$Gmat_tpc) - min(mba$Gmat_tpc),
                  Média = mean(mba$Gmat_tpc), 
                  Mediana = median(mba$Gmat_tpc),
                  Q1 = quantile(mba$Gmat_tpc,0.25),
                  Q3 = quantile(mba$Gmat_tpc, 0.75),
                  Desv.Pad. = sd(mba$Gmat_tpc),
                  Var. = var(mba$Gmat_tpc),
                  Iqr = IQR(mba$Gmat_tpc))

mba %>% summarise(N = n(),
        Mín = min(mba$Work_yrs), 
                  Máx = max(mba$Work_yrs),
                  Amp = max(mba$Work_yrs) - min(mba$Work_yrs),
                  Média = mean(mba$Work_yrs), 
                  Mediana = median(mba$Work_yrs),
                  Q1 = quantile(mba$Work_yrs,0.25),
                  Q3 = quantile(mba$Work_yrs, 0.75),
                  Desv.Pad. = sd(mba$Work_yrs),
                  Var. = var(mba$Work_yrs),
                  Iqr = IQR(mba$Work_yrs))

salario %>% summarise(N = n(),
                      Mín = min(salario$Salary), 
                       Máx = max(salario$Salary),
                       Amp = max(salario$Salary) - min(salario$Salary),
                       Média = mean(salario$Salary), 
                       Mediana = median(salario$Salary),
                       Q1 = quantile(salario$Salary,0.25),
                       Q3 = quantile(salario$Salary, 0.75),
                       Desv.Pad. = sd(salario$Salary),
                       Var. = var(salario$Salary),
                       Iqr = IQR(salario$Salary))

salario %>% summarise(N = n(),
                      Mín = min(salario$Salary), 
                      Máx = max(salario$Salary),
                      Amp = max(salario$Salary) - min(salario$Salary),
                      Média = mean(salario$Salary),
                      Mediana = median(salario$Salary),
                      Q1 = quantile(salario$Salary,0.25),
                      Q3 = quantile(salario$Salary, 0.75),
                      Desv.Pad. = sd(salario$Salary),
                      Var. = var(salario$Salary),
                      Iqr = IQR(salario$Salary))


group_by(mba, Sex) %>% summarise(n = n(), mean = mean(Gmat_tot), var = var(Gmat_tot))

group_by(mba, Age) %>% summarise(n = n(), mean = mean(Gmat_tot), var = var(Gmat_tot))

t.test(mba$Age, mba$Gmat_tot, alternative = "two.sided", var.equal = F)
t.test(mba$Age, mba$Gmat_tot, alternative = "less", var.equal = F)
t.test(mba$Age, mba$Gmat_tot, alternative = "greater", var.equal = F)

t.test(regreIng$GMAT_tot_Ing, regreIng$GMAT_tot_Outras, alternative = "two.sided", var.equal = F)
t.test(regreIng$GMAT_tot_Ing, regreIng$GMAT_tot_Outras, alternative = "less", var.equal = F)
t.test(regreIng$GMAT_tot_Ing, regreIng$GMAT_tot_Outras, alternative = "greater", var.equal = F)

t.test(regreSal$Salary_H, regreSal$Salary_M, alternative = "two.sided", var.equal = F)
t.test(regreSal$Salary_H, regreSal$Salary_M, alternative = "less", var.equal = F)
t.test(regreSal$Salary_H, regreSal$Salary_M, alternative = "greater", var.equal = F)

Anova = aov(mba$Sex ~ mba$Gmat_tot, data = mba)
summary(Anova)

summary(lm(Gmat_tot ~ Salary, data = salario))
summary(lm(Salary ~ Gmat_tot, data = salario))
summary(lm(Work_yrs ~ Salary, data = salario))
summary(lm(Salary ~ Work_yrs, data = salario))


group_by(mba, Salary) %>% summarise(n = n())

reg1 = lm(formula = Salary ~ Age, data = salario2)
reg2 = lm(formula = Age ~ Salary, data = salario2)
reg3 = lm(formula = Salary ~ Work_yrs, data = salario2)
reg4 = lm(formula = Work_yrs ~ Salary, data = salario2)

summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)

abli