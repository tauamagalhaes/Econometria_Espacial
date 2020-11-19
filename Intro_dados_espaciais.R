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

## Variaveis defasadas espacialmente
# Transcormando para numerico
chicago_data$num_theft <- as.numeric(chicago_data$num_theft)
chicago_data@data$wnum_theft <- lag.listw(w_3, chicago_data@data$num_theft, NAOK=TRUE)

##### LISA Map
# I de Moran local
locm <- localmoran(as.vector(chicago_data$num_theft), w_3)
# Padronizando
chicago_data$snum_theft <- scale(chicago_data$num_theft)
# Defasagem espacial
chicago_data$wsnum_theft <- lag.listw(w_3, chicago_data$snum_theft, NAOK=TRUE)
# Scatter plot
plot(x = chicago_data$snum_theft, y = chicago_data$wsnum_theft, main = " Moran Scatterplot Number of thefts Chicago 2015")
abline(h = 0, v = 0)
abline(lm(chicago_data$wsnum_theft ~ chicago_data$snum_theft), lty = 3, lwd = 4, col = "red")
# Definindo os quadrantes
chicago_data$quad_sig <- NA
chicago_data@data[(chicago_data$snum_theft >= 0 & chicago_data$snum_theft >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
chicago_data@data[(chicago_data$snum_theft <= 0 & chicago_data$snum_theft <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
chicago_data@data[(chicago_data$snum_theft >= 0 & chicago_data$snum_theft <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
chicago_data@data[(chicago_data$snum_theft >= 0 & chicago_data$snum_theft <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
chicago_data@data[(chicago_data$snum_theft <= 0 & chicago_data$snum_theft >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 5 

breaks <- seq(1, 5, 1)
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(chicago_data$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(chicago_data, col = colors[np], bty = "n")
mtext("Local Moran's Number of thefts Chicago 2015", cex = 1, side = 3, line = 0.1)
legend("bottomleft", legend = labels, fill = colors, bty = "n", cex = 0.5)

#### Procedimento de Baumont para a escolha de W

## Passo 1: estima-se o modelo classico de regressao linear
## Passo 2: testam-se os residuos desse modelo para autocorrelacao espacial, usando
# o I de Moran para um conjunto de matrizes W. Na funcao criada abaixo testa-se para k = 1 a k = 20 vizinhos 
# mais proximos
## Passo 3: seleciona-se a matriz de pesos espaciais que tenha gerado o mais alto valor do teste de I de Moran,
# e que seja significativo estatisticamente.

## Criando uma funcao para realizar o procedimento
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

## Exemplo de como usar a funcao para um modelo de regressao linear simples
formula <- num_theft ~ poverty
baumont(formula, chicago_data@data)
# Pelos resultados temos que a matriz de k = 2 apresentou o maior I de Moran estatisticamente significativo a niveis
# convencionais, o que indica que neste modelo seria a matriz mais adequada seguindo o procedimento de Baumont.
