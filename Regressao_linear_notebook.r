library(tidyverse)
library(palmerpenguins)
options(repr.plot.width=6, repr.plot.height=4)

data(penguins)
penguins %>% head()

penguins %>% 
  distinct(species, island)

Gentoo = penguins %>% 
  filter(species == 'Gentoo') %>% 
  na.omit() 
Gentoo %>% dim()

plt1 = ggplot(data = Gentoo, mapping = aes(x = flipper_length_mm, y = bill_length_mm)) +
    geom_point() +
    geom_smooth(method = 'lm', se = F)
plt1

m1 = lm(bill_length_mm ~ flipper_length_mm, data = Gentoo)
m1

(beta0 = m1$coefficients[1])
(beta1 = m1$coefficients[2])

Xextra = 210
(bico_predito = beta0 + beta1 * Xextra)

Xextra = 210
(bico_predito = beta0 + beta1 * Xextra)
plt1 +
    geom_point(aes(x = Xextra, y = bico_predito), colour="red", size = 8) +
    geom_segment(aes(x = 200, xend = Xextra, y = bico_predito, yend = bico_predito), linetype = 2, col = 2, size = 2) +
    geom_segment(aes(x = Xextra, xend = Xextra, y = 40, yend = bico_predito), linetype = 2, col = 2, size = 2)


plt1 +
    xlim(c(150, 290)) + ylim(c(30, 80)) +
    geom_abline(intercept = beta0, slope = beta1, color = "red", linetype = 'dashed')


summary(m1)

(beta1 = summary(m1)$coefficients[2,1])
(s_beta1 = summary(m1)$coefficients[2,2])
(BETA1 = 0)
(tc = (beta1 - BETA1) / s_beta1)
(gl = df.residual(m1))

pt(tc, df = gl, lower.tail = F) * 2

N = 10000
s_x = sd(Gentoo$flipper_length_mm)/1.96
BETA0 = mean(Gentoo$bill_length_mm)
BETA1 = 0
X = runif(N, min(Gentoo$flipper_length_mm), max(Gentoo$flipper_length_mm))
Y = rnorm(n = N, mean = BETA0 + BETA1 * X, sd = s_x)
df = data.frame(Y,X)
head(df)
dim(df)

plt2 = ggplot(df, mapping = aes(x = X, y = Y)) +
    geom_point(alpha = 0.3, color = 'grey') +
    ggtitle(label = expression(H[0])) +
    theme(plot.title = element_text(hjust = 0.5))

plt2

amostra = df %>% 
    sample_n(5)
amostra

amostra = df %>% 
    sample_n(120)

m_sim = lm(Y ~ X, data = amostra)
b0a = as.numeric(summary(m_sim)$coefficients[1,1])
b1a = summary(m_sim)$coefficients[2,1]
texto = bquote(Y == .(round(b0a,1)) + .(round(b1a,2)) * X)

plt2 +
    geom_point(data = amostra, color = 1) +
    geom_smooth(data = amostra, method = 'lm', se = F) +
    xlab(texto)


b0a = as.numeric(summary(m1)$coefficients[1,1])
b1a = summary(m1)$coefficients[2,1]
texto = bquote(Y == .(round(b0a,1)) + .(round(b1a,2)) * X)

plt2 +
    geom_point(data = Gentoo, mapping = aes(x = flipper_length_mm, y = bill_length_mm)) +
    geom_smooth(data = Gentoo, mapping = aes(x = flipper_length_mm, y = bill_length_mm), 
                method = 'lm', se = F) +
    xlab(texto)

summary(m1)
plt1 +
    geom_segment(aes(x = flipper_length_mm, 
                     xend = flipper_length_mm, 
                     y = predict(m1), 
                     yend = bill_length_mm), color = 2, linetype = 2)

plot(rstudent(m1) ~ m1$fitted)
abline(h = 0, col = 2)

hist(rstudent(m1), freq = F, breaks = 15)
curve(dnorm(x),-5,5,add=T,col="blue")
shapiro.test(rstudent(m1))

qqnorm(rstudent(m1), pch = 1, frame = FALSE)
qqline(rstudent(m1), col = "steelblue", lwd = 2)

plt1

m1 = lm(bill_length_mm ~ flipper_length_mm, data = Gentoo)
summary(m1)

options(repr.plot.width=7, repr.plot.height=7)
par(mfrow = c(2,2))
plot(m1)
