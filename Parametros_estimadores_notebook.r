library(tidyverse)
library(gridExtra)
## define tamanho das figuras
options(repr.plot.width=6, repr.plot.height=4)

x = c(5.0, 4.1, 6.6, 4.5, 4.9, 7.7, 4.0, 5.0, 7.0, 7.9)

n = length(x)
xb = mean(x)
xb

var_a = sum((x - xb)^2)/(n - 1)
var_a
desv_a = sqrt(var_a)
desv_a

sd(x)

mi = 10
sg = 2
x = c(4, 10)
fx = (1/(sqrt(2 * pi * sg^2))) * exp((-1/2) * ((x - mi)/sg)^2)
fx

x = seq(1, 2.1, 0.01)
fx = dnorm(x, mean = 1.68, sd = 0.10)


ggplot(data.frame(x = x, fx = fx)) +
    geom_point(aes(x, fx))

ggplot(data.frame(x = c(1,2.5))) +
  stat_function(aes(x = x), fun = dnorm,
               args = list(mean = 1.68, sd = 0.1))

ie = read.csv("datasets/IE_BICT_2019.csv", sep = ";", dec = ',')
dim(ie)

temp <- data.frame(tm = datasets::nhtemp)
dim(temp)

alt_plt <- ggplot(ie, aes(x = Altura)) +
   geom_histogram(aes(y =..density..), 
                  fill = 'dodgerblue4', color = 'black', bins = 10) +
   stat_function(fun = dnorm, args = list(mean = mean(ie$Altura, na.rm = T),
                                          sd = sd(ie$Altura, na.rm = T))) +
   labs(x = "Alturas em metros",
        y = "Frequencia relativa") +
   theme_classic()

temp_plt <- ggplot(temp, aes(x = tm)) +
   geom_histogram(aes(y =..density..),
                  fill = 'dodgerblue4', color = 'black', bins = 10) +
   stat_function(fun = dnorm, args = list(mean = mean(temp$tm, na.rm = T),
                                          sd = sd(temp$tm, na.rm = T))) +
   labs(x = "Average Yearly Temperatures in New Haven - USA",
        y = "Frequencia relativa") +
   theme_classic()

options(repr.plot.width=7, repr.plot.height=4)
grid.arrange(alt_plt, temp_plt, ncol = 2)

xb = mean(ie$Altura, na.rm = T)
dp = sd(ie$Altura, na.rm = T)
xb; dp

px = pnorm(1.80, mean = xb, sd = dp, lower.tail = TRUE)
px

ggplot(data.frame(x = c(1.3, 2.1))) +
  stat_function(aes(x = x), fun = dnorm,
               args = list(mean = xb, sd = dp)) +
  geom_area(stat = 'function', fun = dnorm, fill = 'red',
            args = list(mean = xb, sd = dp),
            xlim = c(1.2, 1.8))

x = c(1.97, 1.83, 1.62, 1.77, 1.63, 1.68, 1.63, 1.68, 1.69, 1.85)
(xb = mean(x))
(sd = sd(x))
(n = length(x))

alfa = 0.05
t = qt(1-alfa/2, df = n - 1)
t

(IC_sup = xb + t * sd/sqrt(n))
(IC_sup = xb - t * sd/sqrt(n))

muX = round(xb,2)
sigmaX = round(dp,2)
x1 <- 1.8
n1 <- 2
n2 <- 10

tam <- 20
lim_x <- muX + c(-4,4) * sigmaX
exprx <- expression(
   paste('média = ', mu, '; desvio padrão = ', sigma))
expr1 <- expression(
   paste('média = ', mu, '; desvio padrão = ', frac(sigma, sqrt('n'["1"]))))
expr2 <- expression(
   paste('média = ', mu, '; desvio padrão = ', frac(sigma, sqrt('n'["2"]))))

p1 <- ggplot(data.frame(x = lim_x), aes(x = x)) +
  stat_function(fun = dnorm,
                args = list(mean = muX,
                            sd = sigmaX)) +
   geom_area(stat = "function", fun = dnorm,
             args = list(mean = muX,
                         sd = sigmaX),
             fill = "#00998a", 
             xlim = c(x1, lim_x[2])) +
   labs(x = "X", y = "") +
   scale_y_continuous(breaks = NULL) +
   scale_x_continuous(breaks = c(muX, x1)) +
   theme(axis.text.x = element_text(size = tam),
         axis.title.x = element_text(size = tam),
         plot.title = element_text(hjust = 0.5, size = tam)) +
   ggtitle(exprx) +
   theme_classic()

p2 <- ggplot(data.frame(x = lim_x), aes(x = x)) +
  stat_function(fun = dnorm,
                args = list(mean = muX,
                            sd = sigmaX/sqrt(n1))) +
   geom_area(stat = "function", fun = dnorm,
             args = list(mean = muX,
                         sd = sigmaX/sqrt(n1)),
             fill = "#00998a", 
             xlim = c(x1, lim_x[2])) +
   labs(x = expression(bar("X")), y = "") +
   scale_y_continuous(breaks = NULL) +
   scale_x_continuous(breaks = c(muX, x1)) +
   theme(axis.text.x = element_text(size = tam),
         axis.title.x = element_text(size = tam),
         plot.title = element_text(hjust = 0.5, size = tam)) +
   ggtitle(expr1) +
   theme_classic()

p3 <- ggplot(data.frame(x = lim_x), aes(x = x)) +
  stat_function(fun = dnorm,
                args = list(mean = muX,
                            sd = sigmaX/sqrt(n2))) +
   geom_area(stat = "function", fun = dnorm,
             args = list(mean = muX,
                         sd = sigmaX/sqrt(n2)),
             fill = "#00998a", 
             xlim = c(x1, lim_x[2])) +
   labs(x = expression(bar("X")), y = "") +
   scale_y_continuous(breaks = NULL) +
   scale_x_continuous(breaks = c(muX, x1)) +
   theme(axis.text.x = element_text(size = tam),
         axis.title.x = element_text(size = tam),
         plot.title = element_text(hjust = 0.5, size = tam)) +
   ggtitle(expr2) +
   theme_classic()

options(repr.plot.width=6, repr.plot.height=12)
pn <- grid.arrange(p1,p2,p3, ncol = 1)

(area <- round(pnorm(x1, mean = muX, sd = sigmaX, lower.tail = F), 3))
(area1 <- round(pnorm(x1, mean = muX, sd = sigmaX/sqrt(n1), lower.tail = F), 3))
(area2 <- round(pnorm(x1, mean = muX, sd = sigmaX/sqrt(n2), lower.tail = F), 3))


options(repr.plot.width=6, repr.plot.height=3)
n1 <- 2
n2 <- 5
n3 <- 30
ggplot(data = data.frame(x = c(-4,4)),
       mapping = aes(x = x)) +
  stat_function(mapping = aes(color = "Dist. Normal"),
                fun = dnorm) +
  stat_function(mapping = aes(color = paste("Dist. t; n =",n1)),
                fun = dt,
                args = list(df = n1-1)) +
  stat_function(mapping = aes(color = paste("Dist. t; n =",n2)),
                fun = dt,
                args = list(df = n2-1)) +
  stat_function(mapping = aes(color = paste("Dist. t; n =",n3)),
                fun = dt,
                args = list(df = n3-1)) +
  labs(colour = "", y = "Densidade de probabilidade", x = "") +
  theme_classic()
