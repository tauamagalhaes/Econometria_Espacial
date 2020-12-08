##### Controle Sintetico

##### Carregando as bibliotecas
library(Synth)

##### Dados
# Exemplo utilizando os dados de 
#Abadie, Alberto, and Javier Gardeazabal.
#"The economic costs of conflict: A case study of the Basque Country"
# American economic review 93.1 (2003): 113-132.
data("basque")
View(basque)

# Preparando os dados para serem utilizados nas estimacoes
dataprep.out <- dataprep(
   foo = basque,
   predictors = c("school.illit", "school.prim", "school.med",
                    "school.high", "school.post.high", "invest"),
   predictors.op = "mean",
   time.predictors.prior = 1964:1969,
   special.predictors = list(
     list("gdpcap", 1960:1969 , "mean"),
     list("sec.agriculture", seq(1961, 1969, 2), "mean"),
     list("sec.energy", seq(1961, 1969, 2), "mean"),
     list("sec.industry", seq(1961, 1969, 2), "mean"),
     list("sec.construction", seq(1961, 1969, 2), "mean"),
     list("sec.services.venta", seq(1961, 1969, 2), "mean"),
     list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
     list("popdens", 1969, "mean")),
   dependent = "gdpcap",
   unit.variable = "regionno",
   unit.names.variable = "regionname",
   time.variable = "year",
   treatment.identifier = 17,
   controls.identifier = c(2:16, 18),
   time.optimize.ssr = 1960:1969,
  time.plot = 1955:1997)
# Variaveis nas regioes que sofreram intervencao no periodo pre-intervencao
dataprep.out$X1
# Variaveis nas regioes que nao sofreram intervencao no periodo pre-intervencao
dataprep.out$X0
# Variavel de interesse nas regiao de intervencao
# Z1: the Basque GDP per-capita for the pre-intervention period
dataprep.out$Z1
# Variavel de interesse nas regioes sem intervencao
# Z0: the synthecti GDP per-capita for the pre-intervention period
dataprep.out$Z0

# Consolidando os dados de escolaridade como os autores fizeram
dataprep.out$X1["school.high",] <- dataprep.out$X1["school.high",]
dataprep.out$X1["school.post.high",]
dataprep.out$X1 <- as.matrix(dataprep.out$X1[-which(rownames(dataprep.out$X1) == "school.post.high"),])
dataprep.out$X0["school.high",] <- dataprep.out$X0["school.high",]
dataprep.out$X0["school.post.high",]
dataprep.out$X0 <- dataprep.out$X0[-which(rownames(dataprep.out$X0) == "school.post.high"),]
lowest <- which(rownames(dataprep.out$X0) == "school.illit")
highest <- which(rownames(dataprep.out$X0) == "school.high")
dataprep.out$X1[lowest:highest,] <- (100 * dataprep.out$X1[lowest:highest,]) /sum(dataprep.out$X1[lowest:highest,])
dataprep.out$X0[lowest:highest,] <- 100 * scale(dataprep.out$X0[lowest:highest,], center = FALSE,
                 scale = colSums(dataprep.out$X0[lowest:highest,]))

#### Running 
# O processo baseia-se em um metodo de otimizacao em que o objetivo e solucionar a matriz diagonal V
# que minimiza o MSPE no periodo pre intervencao
# BFGS quasi-Newton algorithm
synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")

# Diferencas entre o GDP da regiao basca e o controle sintetico no periodo pre intervencao
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
gaps[1:3, 1]

## Produzindo tabelas
synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
# Tabela comparandos os valores das variaveis preditoras
synth.tables$tab.pred[1:5, ]

#### Plots
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "Real per-capita GDP (1986 USD, thousand)", Xlab = "year",
          Ylim = c(0, 12), Legend = c("Basque country",
                                        "Synthetic Basque country"), Legend.position = "bottomright")
# A trajetoria do GDP per capita do grupo de controle sintetico cresce de forma mais vertiginosa que nos
# paises bascos, o que indica que o terrorismo teve um impacto negativo no crescimento da regiao