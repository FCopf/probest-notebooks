library(tidyverse)
options(repr.plot.width=5, repr.plot.height=5)

set.seed(839)
mu = 2.65
sigma = 0.5
n = 10
amostra = rnorm(n = n, mean = mu, sd = sigma) %>% round(2)
amostra

(xb = mean(amostra))
(dp = sd(amostra))
(ep = dp / sqrt(n))

(tc = (xb - mu) / ep)

(valor_p = pt(q = tc, df = n - 1, lower.tail = FALSE) * 2)

t.test(amostra, mu = mu, alternative = "two.sided")

jackal <- read.csv('datasets/jackal.csv', sep = ';', header = TRUE)
(jackal)

jackal %>% group_by(Sexo) %>% 
    summarize(Medias = mean(Comprimento))

jackal %>% group_by(Sexo) %>% 
    summarize(Desvio = sd(Comprimento))

ggplot(jackal, aes(x = Sexo, y = Comprimento)) +
    geom_boxplot()

t.test(Comprimento ~ Sexo, data = jackal, alternative = 'two.sided', var.equal = TRUE)

