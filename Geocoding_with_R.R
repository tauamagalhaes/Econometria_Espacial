######## Geocoding using Google API

##### Bibliotecas
library(googleway)
library(ggmap)
library(tidyverse)
library(httr)
library(sf)
library(mapview)

##### Geocodificar um unico endereco
#token = "###### INSIRA A API KEY DO GOOGLE AQUI ######"
res <- google_geocode(address = "rua nair furtado de souza 381 teixeiras juiz de fora mg", key = token)
res$results$geometry$location$lat
res$results$geometry$location$lng
devtools::install_github("dkahle/ggmap")

##### Georreferenciando mais de um endereco

# Registre a API do Google
# Atente-se para o numero maximo de requisicoes
#register_google(key = "###### INSIRA A API KEY DO GOOGLE AQUI ######")

## Dados de enderecos para serem georreferenciados
# Definindo diretorio
setwd("/Users/tauamagalhaes/Documents/Spatial_Econometrics/Monitoria/Georreferenciamento")
# Carregandos os dados
enderecos_df <- read_excel("enderecos.xlsx")
# Filtrando para que nao georreferencie duas vezes o mesmo endereco
enderecos_df <- distinct(enderecos_df)
locations_df <- mutate_geocode(enderecos_df, address); locations_df

#### Plots
# Converter para um objeto "sf"
# crs = 4326 refere-se ao World Geographic System 1984 projection
locations_sf <- st_as_sf(locations_df, coords = c("lon", "lat"), crs = 4326)
mapview(locations_sf)


#### Georreferenciamento utilizando CEPaberto

token <- "#### Insira seu token do CEPaberto ####"

#Funcao para consultar o endereco

cepaberto <- function(zipcode, token){
  require(httr)
  url <- "http://www.cepaberto.com/api/v3/cep?cep="
  
  require <- GET(paste0(url,zipcode),
                    add_headers(Authorization = paste0('Token token="' , token , '"')))
  res <- content(require, as = "parsed")
  
  tries <- 0
  while(length(res) <= 2){
    require <- GET(paste0(url,cep),
                      add_headers(Authorization = paste0('Token token="' , token , '"')))
    res <- content(require, as = "parsed")
    Sys.sleep(5)
    tries <- tries + 1
    if(tries == 10){
      break
    }
  }
  
  coords <- data.frame( 
    lat = ifelse(is.null(res$lat), NA, as.numeric(res$lat)), 
    lng = ifelse(is.null(res$long), NA, as.numeric(res$long))
  )
  
  return(coords)
} 
#The function cepaberto takes two arguments
#First the brazillian zipcode. Second the token needed to consult the url CEPaberto.com
#The output of the function is the lat/long of the zipcode

## For a single zipcode
cepaberto(36640000, token)

