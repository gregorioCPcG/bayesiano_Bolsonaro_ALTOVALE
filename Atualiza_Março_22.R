#Atualização - sem novos dados, mas com novas técnicas

#ajuste ###############
# começo repete igual https://github.com/gregorioCPcG/bayesiano_Bolsonaro_ALTOVALE/blob/Atualizacao-Mar%C3%A7o_2022/Analise%20final.R
options("scipen"=100, "digits"=4)
library(tidyverse)
a1 <- 1
a2 <- 1
n <- 100
s <- 67.52 #votação alto vale 2018 bolsonaro
w <- a1+s
z <- a2+(n-s)
#fórmula rbeta(10000, a1 + s, a2 + n-s) 
uniform <- rbeta(10000, w, z)
a1 <- 68.52
a2 <- 33.48
n <- 100
s <- 47.5
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
a1 <- w
a2 <- z
n <- 100
s <- 48.26
w <- a1+s
z <- a2+(n-s)
posteriori2 <- rbeta(10000, w, z)
a1 <- w
a2 <- z
n <- 100
s <- 47.12563
w <- a1+s
z <- a2+(n-s)
posteriori3 <- rbeta(10000, w, z)
a1 <- w
a2 <- z
n <- 100
s <- 48.23288
w <- a1+s
z <- a2+(n-s)
posteriori4 <- rbeta(10000, w, z)
Bolsonaro <- posteriori4
rm(n,posteriori1,posteriori2, posteriori3, posteriori4, uniform,w,z,s,a1,a2)
a <- Bolsonaro
Bolsonaro <- data.frame(Bolsonaro)

#### Agora o bixo pega ####
max(a)
min(a)
boxplot(a*100)
summary(a)
sd(a)
IQR(a)
hist(a)
media <- mean(a)
sumar <- summary(a)
sumar
media
# testar intervalos
n <- 1000
samp <- sample_n(Bolsonaro, n)
samp %>%
  summarise (mean_samp60 = mean(Bolsonaro), med_s60 = median(Bolsonaro),
             se_s60 = sd(Bolsonaro))
sumar#para conferir
sd(Bolsonaro$Bolsonaro) # para conferir

#confidence 95%
z_star_95 <- qnorm(0.975)
z_star_95
samp %>%
  summarise(lower = mean(Bolsonaro) - z_star_95 * (sd(Bolsonaro) / sqrt(n)),
            upper = mean(Bolsonaro) + z_star_95 * (sd(Bolsonaro) / sqrt(n)))
media#so pra conferir

##
# confidence levels
params <- Bolsonaro %>%
  summarise(mu = mean(Bolsonaro))

samp %>%
  summarise(samp_min = min(Bolsonaro), samp1_max = max(Bolsonaro))
Bolsonaro %>%
  summarise(pop_min = min(Bolsonaro), pop_max = max(Bolsonaro))#para conferir

library(statsr)
library(dplyr)
ci <- Bolsonaro %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(Bolsonaro) - z_star_95 * (sd(Bolsonaro) / sqrt(n)),
            upper = mean(Bolsonaro) + z_star_95 * (sd(Bolsonaro) / sqrt(n)))
ci %>%
  slice(1:5) # só pra ver
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))
ci %>%
  slice(1:20) # só pra ver

ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line

# 99% Confidence Level

ci99 <- Bolsonaro %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(Bolsonaro) - 2.58 * (sd(Bolsonaro) / sqrt(n)),
            upper = mean(Bolsonaro) + 2.58 * (sd(Bolsonaro) / sqrt(n)))

ci99 %>%
  slice(1:5)
#
ci99 <- ci99 %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "sim", "não"))
#
ci99_data <- data.frame(ci99_id = c(1:50, 1:50),
                        ci99_bounds = c(ci$lower, ci$upper),
                        capture_mu = c(ci$capture_mu, ci$capture_mu))
#
ggplot(data = ci99_data, aes(x = ci99_bounds, y = ci99_id, 
                             group = ci99_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line
# colocar nomes na imagem

ci99_data$capturou <- as.factor(ci$capture_mu)
levels(ci99_data$capturou) <- c('sim','não')
levels(ci99_data$capturou)
ci99_data$x <- 100*ci99_data$ci99_bounds

ggplot(data = ci99_data, aes(x = x, y = ci99_id, 
                             group = ci99_id, color = capturou)) +
  geom_point(size = 3) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray")+ xlim(51.25,52) + labs(title = "Confiabilidade da previsão", 
                                                              subtitle = "99% de Nível de Confiança", x = "Previsão % votos Bolsonaro Alto Vale turno 1 2022", y = "Simulação", 
                                                              caption = "Fonte: https://sites.google.com/view/gregoriosilva")
  