#Load ####
library (dplyr)
library (ggplot2)
library (shiny)
library(tidyverse)
library(readxl)
library(coefplot)
library(sjPlot)
library(dplyr)
library(statsr)
library(BAS)
library(tidyr)
library(openxlsx)
library(readxl)


base1 <- read_excel("base1.xlsx")

base1$log_eleitorado <- log(base1$eleitorado)

tabi <- read_excel("base_total.xlsx")
write.xlsx(base1, file = 'monte_carlo6464.xlsx')
tabi$log_eleitoradoT <- log(tabi$eleitorado)

write.xlsx(tabi, file = 'sohparatotal.xlsx')

cor(base1$atual, base1$Bolsonaro)
cor(base1$Bolsonaro, base1$masculino)
cor(base1$Bolsonaro, base1$log_eleitorado)
cor(base1$atual, base1$masculino)
cor(base1$atual, base1$log_eleitorado)

modelo1 <- lm(atual ~ Bolsonaro + log_eleitorado + masculino, data = base1)
summary(modelo1)
plot(modelo1)
tab_model(modelo1, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
coefplot(modelo1, intercept = F, outerCI = F)


#BAS priori 2####
base2 <- base1 %>% select(atual, Bolsonaro, log_eleitorado, masculino)

#model bas
modelbas<-bas.lm(atual ~.,data=base2, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(modelbas, which=1, add.smooth=F)
#2. cumulative probability
plot(modelbas, which=2)


#3. model dimension plot
plot(modelbas, which=2)
#4.PIP
plot(modelbas, which = 4, ask=FALSE, caption="", sub.caption="")

#model rank
image(modelbas, rotate = F)

summary(modelbas)


#Agrolândia
mun <- data.frame(log_eleitorado = 8.80026465131034, masculino = 49.5,Bolsonaro = 67.9324894514768)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values


#Agronomica
mun <- data.frame(log_eleitorado = 8.18757739559151, masculino = 50.5,Bolsonaro = 75.055617352614)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Atalanta
mun <- data.frame(log_eleitorado = 7.81156848934518, masculino = 49.6,Bolsonaro = 70.3928716079384)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Aurora
mun <- data.frame(log_eleitorado = 8.35772841676521, masculino = 50.8,Bolsonaro = 71.2643678160919)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Braço do Trombudo
mun <- data.frame(log_eleitorado = 7.88004820097158, masculino = 50.2,Bolsonaro = 73.6762481089259)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#chap. do lag.
mun <- data.frame(log_eleitorado = 7.66434663209862, masculino = 53,Bolsonaro = 56.2646644767715)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Dona Emma
mun <- data.frame(log_eleitorado = 7.86134179559999, masculino = 50.6,Bolsonaro = 59.2678227360308)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Ibirama
mun <- data.frame(log_eleitorado = 9.36306145899385, masculino = 49,Bolsonaro = 67.2789699570815)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Imbuia
mun <- data.frame(log_eleitorado = 8.34307787116938, masculino = 50.5,Bolsonaro = 61.8186146155677)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Ituporanga
mun <- data.frame(log_eleitorado = 9.65271583410444, masculino = 48.8,Bolsonaro = 70.4446157800051)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Jose Boiteux
mun <- data.frame(log_eleitorado = 8.10288913464087, masculino = 50.9,Bolsonaro = 57.2336561743341)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Laurentino
mun <- data.frame(log_eleitorado = 8.485083137498, masculino = 49.9,Bolsonaro = 70.8798017348203)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Lontras
mun <- data.frame(log_eleitorado = 8.83753604610757, masculino = 48.7,Bolsonaro = 71.0656213704994)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Mirim Doce
mun <- data.frame(log_eleitorado = 7.58680353516258, masculino = 51.1,Bolsonaro = 62.6267748478702)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#petrolandia
mun <- data.frame(log_eleitorado = 8.4069317971587, masculino = 51.2,Bolsonaro = 67.4854845913354)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#pouso redondo
mun <- data.frame(log_eleitorado = 9.20412107216227, masculino = 49.6,Bolsonaro = 65.0734554236265)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#presidente getúlio
mun <- data.frame(log_eleitorado = 9.28591155882326, masculino = 49.1,Bolsonaro = 71.006026889198)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Presidente Nereu
mun <- data.frame(log_eleitorado = 7.44249272279444, masculino = 51.3,Bolsonaro = 65.377855887522)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

summary(base1$log_eleitorado)
boxplot(base1$log_eleitorado)
hist(base1$log_eleitorado)
hist(base1$Bolsonaro)
summary(base1$Bolsonaro)

#Rio do Campo
mun <- data.frame(log_eleitorado = 8.38389034410182, masculino = 51.2,Bolsonaro = 66.4076782449726)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Rio do Oeste
mun <- data.frame(log_eleitorado = 8.51859221232995, masculino = 50.5,Bolsonaro = 64.9690433393249)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values


#Rio do Sul
mun <- data.frame(log_eleitorado = 10.5931285937668, masculino = 47,5,Bolsonaro = 68,8660311088811)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values


#Salete
mun <- data.frame(log_eleitorado = 8.51579221050061, masculino = 50.5,Bolsonaro = 65.3514920889245)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values


#Santa Terezinha
mun <- data.frame(log_eleitorado = 8.58895555764313, masculino = 53.7,Bolsonaro = 57.7438570364855)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values


#Taio
mun <- data.frame(log_eleitorado = 9.36606124697296, masculino = 49.5,Bolsonaro = 71.5447154471545)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values


#Trom Central
mun <- data.frame(log_eleitorado = 8.375168691, masculino = 49.1,Bolsonaro = 70.5394190871369)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Vidal Ramos
mun <- data.frame(log_eleitorado = 8.41803561988302, masculino = 51,6,Bolsonaro = 61.6828621908127)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Vitor Meireles
mun <- data.frame(log_eleitorado = 8.1371033896393, masculino = 52,Bolsonaro = 57.268207078093)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Witmarsum
mun <- data.frame(log_eleitorado = 7.9251575122247, masculino = 50.7,Bolsonaro = 61.7498192335503)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Alto Vale  com pop total
mun <- data.frame(log_eleitorado = 12.1335395863958, masculino = 50.38,Bolsonaro = 67.543694592139)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

# Alto Vale com pop media
mun <- data.frame(log_eleitorado = 8.50, masculino = 50.38,Bolsonaro = 67.543694592139)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values
# [1] 48,26

# novo posteriori (3)
# Efeito LULA, Ciro e Curso Superior ####

novo <- read_excel("basenovos.xlsx")
modelo2 <- lm(previsao2022 ~ Lula2006 + superior_comp + Ciro2018, data = novo)
summary(modelo2)

base4 <- novo %>% select(previsao2022, Ciro2018, superior_comp, Lula2006)


#model bas
modelbas<-bas.lm(previsao2022 ~.,data=base4, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(modelbas, which=1, add.smooth=F)

plot(modelbas, which=2)

#PIP
plot(modelbas, which = 4, ask=FALSE, caption="", sub.caption="")

#model rank
image(modelbas, rotate = F)

summary(modelbas)


#Alto Vale  com pop total
mun <- data.frame(Ciro2018 = 3.69, superior_comp = 8.17, Lula2006 = 23.66)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values
#[1] 47.12563


# nova posteriori ############


#Base 66 = nome do arquivo para o próximo posteriori

# % última posteriri (dep)
#% var02 Bolsonaro 2018/votação em outros no Vale do Itajaí (vide imagem fonte)(para ALto vale média )
#% var03 Bolsonaro 2018/votação Nenhum/branco/nulo
#% var04 Bolsonaro 2018/indeciso*0.47

base66 <- read_excel("base 66 _ posteriori 4.xlsx")

modelo66 <- lm(atual ~ var02 + var03 + var04, data = base66)
summary(modelo66)

base667 <- base66 %>% select(atual, var02, var03, var04)


#model bas
modelbas<-bas.lm(atual ~.,data=base667, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(modelbas, which=1, add.smooth=F)

plot(modelbas, which=2)

#PIP
plot(modelbas, which = 4, ask=FALSE, caption="", sub.caption="")

#model rank
image(modelbas, rotate = F)

summary(modelbas)


#Alto Vale  com pop total# com as médias
mun <- data.frame(var02 = 1.6508, var03 = 7.3391, var04 = 12.69376)
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values
#[1] 48.23288




