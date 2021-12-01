## Pacotes
library(rdd)
library(dplyr)
library(ggplot2)
library(rddtools)

## Gerando variaveis aleatorias

# A descontinuidade ocorre em X
x <- runif(1000,-1,1)
# Covariadas
cov1 <- rnorm(1000)
cov2 <- rnorm(1000)
cov3 <- rnorm(1000)

# Note que a variavel dependendente possui uma descontinuidade em x = 0
y <- 3 + 2 * x + 3 * cov1 + 4 * cov2 + 5 * cov3 + 20 * (x>=0) + rnorm(1000)

# Transformando em um data frame
df <- data.frame(y, x, cov1, cov2, cov3)

## Plotando a descontinuidade
df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_vline(xintercept = 0, color = "red", size = 1, linetype = "dashed")

## Estimacao usando o pacote rdd
# Regressao sem covariaveis
rdd_regression1 <- RDestimate(y ~ x, cutpoint = 0,
                             kernel = "gaussian", se.type = "HC1")
summary(rdd_regression1)

# EIncluindo covariaveis para ganho de eficiencia
rdd_regression2 <- RDestimate(y ~ x|cov1 + cov2 + cov3)
summary(rdd_regression2)

# Plotando a descontinuidade
plot(rdd_regression2, bin_size = "shade")


