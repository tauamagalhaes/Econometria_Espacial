##### Monitoria Econometria Espacial #####

##### Introducao aos Dados Espaciais #####

### Carregandos os pacotes
library(dplyr)
library(sf)
library(spdep)
library(tmap)
library(rgdal)

### Definindo o diretorio
getwd()
setwd("/Users/tauamagalhaes/Documents/Spatial_Econometrics/Monitoria/Dados_Airbnb/airbnb")

### Carregando os dados
# Spatial Polygons Data Frame
chicago_data <- rgdal::readOGR(dsn = "/Users/tauamagalhaes/Documents/Spatial_Econometrics/Monitoria/Dados_Airbnb/airbnb",
                         layer = "airbnb_Chicago 2015")
# sf
chicago_data_sf <- st_read(dsn = "/Users/tauamagalhaes/Documents/Spatial_Econometrics/Monitoria/Dados_Airbnb/airbnb",
                        layer = "airbnb_Chicago 2015") %>% st_as_sf() %>% st_transform(crs = 32723)
# Manipulacoes com dados sf
crime_alto <- chicago_data_sf %>%
  select(community, num_theft) %>%
  filter(num_theft >= mean(num_theft))

### Plots
theft <- tm_shape(chicago_data_sf) + 
  tm_polygons(col = "num_theft", style = "fisher", title = "Número de roubos") +
  tm_grid(lwd = 0); theft

###Spatial Matrix

# Queen
w_queen <- nb2listw(poly2nb(chicago_data, queen = TRUE), zero.policy = TRUE)
summary(w_queen, zero.policy = TRUE)
# Queen padronizada na linha
w_queen_std <- nb2listw(poly2nb(chicago_data, queen = TRUE), style = "W", zero.policy = TRUE)
summary(w_queen_std, zero.policy = TRUE)
# Rook
w_rook <- nb2listw(poly2nb(chicago_data, queen = FALSE), zero.policy = TRUE)
summary(w_rook, zero.policy = TRUE)
# Rook padronizada
w_rook_std <- nb2listw(poly2nb(chicago_data, queen = FALSE), style = "C", zero.policy = TRUE)
summary(w_rook_std, zero.policy = TRUE)
# Distancia inversa
coords <- coordinates(chicago_data)
nb <- dnearneigh(coords, 0, 1000)
dlist <- nbdists(nb, coords)
dlist <- lapply(dlist, function(x) 1/x)
w_inv_dist <- nb2listw(nb, glist=dlist, zero.policy = TRUE)
summary(w_inv_dist, zero.policy = TRUE)
chicago_data$num_theft <- as.numeric(chicago_data$num_theft)

## Vizinhos mais proximos
IDs <- row.names(chicago_data@data)
# 3 vizinhos mais proximos
w_3 <- nb2listw(knn2nb(knearneigh(coords, k = 3), row.names=IDs), style="W")

# Plot of the conections
plot(chicago_data, border = "grey60")
plot(w_3, coordinates(chicago_data), pch = 19, cex = 0.6, add = TRUE)

##### Modelo linear classico nao espacial
formula <- (num_theft ~ poverty + income_pc)
modelo_classico <- lm(formula, data = chicago_data_sf, na.action = na.omit)
summary(modelo_classico)

# Procedimento de Baumont
baumont <- function(modelo, dados){
  per <- 999 # numero de simulacoes de MC
  kv <- 20 # numero de matrizes de vizinhos mais proximos a serem testadas
  IDs <- row.names(dados)
  res.pesos <- data.frame()
  model <- lm(formula = modelo, data = dados)
  dados$residuos <- model$residuals
  for(k in 1:kv){
    res.pesos[k,1] <- k
    moran.k <- moran.mc(dados$residuos,
                        listw=nb2listw(knn2nb(knearneigh(coords, k = k),
                                              row.names = IDs), style = "W", zero.policy = TRUE), nsim = per)
    res.pesos[k,2] <- moran.k$statistic
    res.pesos[k,3] <- moran.k$p.value
  }
  res.pesos 
  
}

baumont(formula, dados = chicago_data_sf)
w_5 <- nb2listw(knn2nb(knearneigh(coords, k = 5), row.names=IDs), style="W")

# Teste dos resíduos: Moran I
i_moran <- lm.morantest(model = modelo_classico, listw = w_5); i_moran

# Teste dos resíduos: Multiplicador de Lagrange
lagrange <- lm.LMtests(model = modelo_classico, listw = w_5,
                            test = c("LMerr","RLMerr","LMlag","RLMlag")); lagrange

#### SAR
modelo_sar <- lagsarlm(formula, data = chicago_data_sf, listw = w_5)
summary(modelo_sar)
impacts(modelo_sar, listw = w_5)

#### CAR
modelo_car <- spautolm(formula, data = chicago_data_sf, listw = w_5, family = "CAR")
summary(modelo_car)

#### SEM
modelo_sem <- errorsarlm(formula, data = chicago_data_sf, listw = w_5)
summary(modelo_sem)

#### SEM estimado usando GMM
modelo_sem_GMM <- GMerrorsar(formula, data = chicago_data_sf, listw = w_5)
summary(modelo_sem_GMM)

#### SAC
modelo_sac <- sacsarlm(formula, data = chicago_data_sf, listw = w_5)
summary(modelo_sac)
impacts(modelo_sac, listw = w_5)

#### SAC estimado usando GMM
modelo_sac_GMM <- gstsls(formula, data = chicago_data_sf, listw = w_5)
summary(modelo_sac_GMM)
impacts(modelo_sac_GMM, listw = w_5)

#### SDM
modelo_sdm <- lagsarlm(formula, data = chicago_data_sf, listw = w_5, type = "mixed")
summary(modelo_sdm)
impacts(modelo_sdm, listw = w_5)

#### SDEM
modelo_sdem <- errorsarlm(formula, data = chicago_data_sf, listw = w_5, etype = "emixed")
summary(modelo_sdem)

#### SMA
modelo_sma <- spautolm(formula, data = chicago_data_sf, listw = w_5, family = "SMA")
summary(modelo_sma)

#### SLX
## Variaveis defasadas espacialmente
# Transcormando para numerico
chicago_data_sf$income_pc <- as.numeric(chicago_data_sf$income_pc)
chicago_data_sf$w_income_pc <- lag.listw(w_5, chicago_data_sf$income_pc, NAOK=TRUE)

modelo_slx <- lm(num_theft ~ poverty + income_pc + w_income_pc, data = chicago_data_sf, na.action = na.omit)
summary(modelo_slx)
