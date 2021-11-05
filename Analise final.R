#Bolsonaro alto vale
options("scipen"=100, "digits"=4)


#10 mil tentativas
#fórmula rbeta(10000, a1 + s, a2 + n-s) 
#acrescentar '>= .5)/10000' para responder à pergunta -> 50% ou mais dos votos

#Bolsonaro alto vale 2018 priori - (uniforme)
# n = 100
# s 82.42

a1 <- 1
a2 <- 1
n <- 100
s <- 67.52 #votação alto vale 2018 bolsonaro
w <- a1+s
z <- a2+(n-s)
#fórmula rbeta(10000, a1 + s, a2 + n-s) 
uniform <- rbeta(10000, w, z)
uniform_50 <- sum (uniform >= .5)/10000
hist(uniform)
mean(uniform)

#novo dado 47,5 # pesquisa ND+
# novo a1 <- ? -> 68.52 (a1 + s) da posteriori 2
# novo a2 <- ? -> 33.48 (a2 + n - s) da posteriori 2
#novo n <- ? -> 100
#novo s <- ? -> 47,5
#10 mil tentativas
#fórmula rbeta(10000, a1 + s, a2 + n-s) 
#acrescentar '>= .5)/10000' para responder à pergunta

a1 <- 68.52
a2 <- 33.48
n <- 100
s <- 47.5
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
posteriori1_50 <-sum(rbeta(10000, w, z) >= .5)/10000 #+ que 50%
posteriori1_2018 <- sum(rbeta(10000, w, z) >= .6752)/10000 # mais que 2018
hist(posteriori1)
mean(posteriori1)


#novo 48,26 (com base na análise inicial)
a1 <- w
a2 <- z
n <- 100
s <- 48.26
w <- a1+s
z <- a2+(n-s)
posteriori2 <- rbeta(10000, w, z)
posteriori2_50 <- sum(rbeta(10000, w, z) >= .5)/10000 #+ que 50%
posteriori2_2018 <- sum(rbeta(10000, w, z) >= .6752)/10000 # mais que 2018
hist(posteriori2)
mean(posteriori2)

#novo 47.12563 (com base em Lula 2006, Ciro 2018, % curso superior)
a1 <- w
a2 <- z
n <- 100
s <- 47.12563
w <- a1+s
z <- a2+(n-s)
posteriori3 <- rbeta(10000, w, z)
posteriori3_50 <- sum(rbeta(10000, w, z) >= .5)/10000 #+ que 50%
posteriori3_2018 <- sum(rbeta(10000, w, z) >= .6752)/10000 # mais que 2018
hist(posteriori3)
mean(posteriori3)


#novo 48.23288 (com base em pesquisa ND+ votos em outros(além de bolsonaro)
#nenhum,branco e nulo , indecisos*0,47)
a1 <- w
a2 <- z
n <- 100
s <- 48.23288
w <- a1+s
z <- a2+(n-s)
posteriori4 <- rbeta(10000, w, z)
posteriori4_50 <- sum(rbeta(10000, w, z) >= .5)/10000 #+ que 50%
posteriori4_2018 <- sum(rbeta(10000, w, z) >= .6752)/10000 # mais que 2018
posteriori4_50valid <- sum(rbeta(10000, w, z) >= .4444)/10000 # mais que 2018
hist(posteriori4)
mean(posteriori4)

hist(posteriori4*100, main = "", xlab = "previsão % primeiro turno Bolsonaro 2022 Alto Vale - 10 mil simulações",
     ylab = "Frequência", col= "blue")


# aí juntar os histogramas bonitos
#ou arrumá-los

#conclusão altamente improvável que repita votação de 2018, mas por outro lado segue sendo a força 
#dominante na região com base nos dados que dispomos hoje deve ser o mais votado e com ampla margem
#à frente do segundo colocado

summary(posteriori4)

mean(posteriori4)
# [resultado] 0.5173

# se 10% dos votos forem brancos ou nulos, a previsão em cima de votos válidos é de:

# 51,73 - 90
# x - 100
(51.73 * 100) / 90
#[1] 57.48

# o intervalo
# 43 min
# 43 - 90
# x - 100
(43 * 100) / 90
#[1] 47.78

# 60,03 max
# 60,3 - 90
# x - 100
(60.03 * 100) / 90
#[1] 66,7


(50*100) / 90
# 50% válidos
100 -55.56
