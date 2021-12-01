##### Dados em painel

##### Carregando os pacotes
library(plm)
library(splm)
library(spdep)
library(sp)
library(Matrix)
library(Formula)
library(maptools)
library(gdata)
library(spData)
library(rgeos)
library(sphet)
library(rgdal)
library(readxl)
library(foreign)
library(geobr)


#### Carregando dados sf dos estatos brasileiros
data <- readr::read_csv("https://raw.githubusercontent.com/tauamagalhaes/Econometria_Espacial/main/dados_painel.csv")
states <- geobr::read_state(code_state = "all", year = 2010)
data <- left_join(data, states, by = "code_state")
data <- data %>% select(code_state, year, y, a, b, c, d, w_y, w_a, w_b, w_c, w_d)

#### Matriz queen de pesos espaciais
w_queen <- nb2listw(poly2nb(states, queen = TRUE), zero.policy = TRUE)

#### Modelos em painel sem correcao espacial

## Pooled data
pool <- plm(y ~ a + b + c + d, data = data, model = "pooling")
summary(pool)
# AIC
AIC_adj <- function(mod){
  n.N   <- nrow(mod$model)
  u.hat <- residuals(mod)
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  p     <-  length(coef(mod)) + 1
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  
  return(aic)
}
AIC_adj(pool)
# Teste de Pesaran CD Breusch-Pagan para avaliar cross-sectional dependence
cd <- pcdtest(y ~ a + b + c + d, data = data)
print(cd) # Rejeita-se a hipotese nula de independence cross-sectional

## Fixed effect
fe <- plm(y ~ a + b + c + d, data = data)
summary(fe)
AIC_adj(fe)

## Random effect
re <- plm(y ~ a + b + c + d, data = data, model = "random")
summary(re)
AIC_adj(re)

## Teste de Hausman   
# O teste de Hausman e usado para comparar os estimadores de efeitos fixos
# com os aleatorios para saber se a pressuposicao de efeitos aleatorios e aceita pelos dados.
ph <- phtest(fe, re)
print(ph)

phtest(y ~ a + b + c + d, data = data, method = "aux", vcov = vcovHC)

## Baltagi, Song and Koh LM teste para autocorrelacao espacial
bsktest(y ~ a + b + c + d, data = data, listw = w_queen, test = "CLMlambda")

#### Modelos em painel com correcao espacial

# Modelo SAR
modSAR <- spml(y ~ a + b + c + d, data = data, listw = w_queen, lag = TRUE,
               model = "within", effect = "individual", spatial.error = "none")
summary(modSAR)
impSAR <- impacts(modSAR, listw = w_queen, time=2)
summary(impSAR)

# Modelo SEM
modSEM <- spml(y ~ a + b + c + d, data = data, listw = w_queen, lag = FALSE,
               model = "within", effect = "individual", spatial.error = "kkp", quiet = FALSE)
summary(modSEM)

# Modelo SAC
modSAC <- spml(y ~ a + b + c + d, data = data, listw = w_queen, lag = TRUE,
               model = "within", effect = "individual", spatial.error = "b")
summary(modSAC)
impSAC <- impacts(modSAC, listw = w_queen, time = 2)
summary(impSAC)

# Modelo SDM
modSDM <- spml(y ~ a + b + c + d + w_a + w_b + w_c + w_d, data = data, listw = w_queen, 
               lag = TRUE, model = "within", effect ="individual", spatial.error = "none")
summary(modSDM)
impSDM <- impacts(modSDM, listw = w_queen, time = 2)
summary(impSDM)

# Modelo SDEM
modSDEM <- spml(y ~ a + b + c + d + w_a + w_b + w_c + w_d, data = data, listw = w_queen, 
                lag = FALSE, model = "within", effect = "individual", spatial.error = "b")
summary(modSDEM)

# Modelo SLX
modSLX <- plm(y ~ a + b + c + d + w_a + w_b + w_c + w_d, data = data)
summary(modSLX)
AIC_adj(modSLX)

##### Diff-in-diff
# Separando os dados
data_2015 <- data %>% filter(year == 2015)
data_2010 <- data %>% filter(year == 2010)

# Criando a primeira diferenca dos dados
delta_y = data_2015$y - data_2010$y
delta_a = data_2015$a - data_2010$a
delta_b = data_2015$b - data_2010$b
delta_c = data_2015$c - data_2010$c
delta_wa = data_2015$w_a - data_2010$w_a
delta_wb = data_2015$w_b - data_2010$w_b
delta_wc = data_2015$w_c - data_2010$w_c

# Transformando em um data frame
did_df <- data.frame(delta_y, delta_a, delta_b, delta_c,
                     delta_wa, delta_wb, delta_wc)

# DID classico em dois periodos estimado por OLS
did_regression <- lm(delta_y ~ delta_a + delta_b + delta_c, data = did_df)
summary(did_regression)

# Teste dos resíduos: Moran I
i_moran <- lm.morantest(model = did_regression, listw = w_queen); i_moran

# Teste dos resíduos: Multiplicador de Lagrange
lagrange <- lm.LMtests(model = did_regression, listw = w_queen,
                       test = c("LMerr","RLMerr","LMlag","RLMlag")); lagrange
# DID espacial: SLX
did_regression <- lm(delta_y ~ delta_a + delta_b + delta_c + delta_wa, data = did_df)
summary(did_regression)
