
#idee: erstmal grafik; vllt für übersicht wöchentliche preise generieren?
#ggfs sheet mit verschiebbarem Zeitfenster. ggfs 
#entsoe für DA Wind + PV Prognose 
#https://data.open-power-system-data.org/time_series/
##https://transparency.entsoe.eu/
#http://www.open-power-system-data.org/ nur bis 2020
#https://energy-charts.info/charts/price_spot_market/chart.htm?l=de&c=DE&interval=week&legendItems=00000000001000

library("dplyr")
library("lubridate")
library("ggplot2")
library("tidyr")
library("jsonlite")
setwd("P:/zeug/statistik usw/R/20230913_DA_Strom_fur_CV")

source("Entsoe_einlesen.R")
source("Fraunhofer_DL.R")

###### EUA ###
EUA <- read.csv2("data/EUA-sg-zertifikate-CU3RPS.csv")[,c(1,3)]
EUA$Zeit <- as.POSIXct(EUA$Zeit)
EUA$EUA <- lag(EUA$Brief, 2)


df <- left_join(df_entsoe, Fraunhofer, by = c("hour" = "hour", "day" = "Tag"))
df <- left_join(df, Gas, by = c("day" = "Date"))
df <- left_join(df, temp2m, by = c("hour" = "hour", "day" = "day"))
df <- left_join(df, EUA, by = c("day" = "Zeit" )) %>%  select(-Brief)

df <- df %>% fill(EUA, .direction = "down")
df <- df %>% fill(Gas2, .direction = "down")
df <- df %>% fill(Gas1, .direction = "down")

df$ID1uSp <- df$ID1_FH - df$Spot
df$ID1uSp_FH <- df$ID1_FH - df$DA_FH

#df <- df %>% select(-load_actual_H   , -load_FH , -Zeitr, -t_begin, -NCG_sameday, -AE_H, -AEuSp  )




##### 
#Variablen Durschnittswerte pro Tag oder pro Woche
df <- df %>% group_by(day) %>%
  mutate(Spot_day = mean(Spot))

df <- df %>% group_by(weekyear) %>%
  mutate(Spot_week = mean(Spot))







summary(testlm)
df$Spot_pred <- predict(testlm, df)

plot(df$Spot_pred, df$ID1uSp)


library(neuralnet)
print(net <- neuralnet(ID1uSp ~ Gas2, data = df, hidden=0, rep=10,  linear.output=FALSE))
nntrain = df[complete.cases(df),]
#nntrainneutest <- scale(nntrain[5:16])

nntrainscaled <- nntrain %>%
  mutate_at(c(5:16), funs(scale(.)))

nntrainscaled <-  nntrain %>% mutate_at(c(5:16), scale)
colnames(nntrainscaled) <- colnames(nntrain)

#net <- neuralnet(ID1uSp ~ Gas2, data = df)
net <- neuralnet(ID1uSp ~ load_H + WindOn_H + WindOff_H + Solar_H + EUA_FH  + Gas2, data = nntrain)
net <- neuralnet(ID1uSp ~ load_H + WindOn_H + WindOff_H + Solar_H + EUA_FH  + Gas2, data = nntrainscaled)
net <- neuralnet(ID1uSp ~ load_H + EUA_FH  + Gas2, data = nntrainscaled)
net <- neuralnet(ID1uSp ~ load_H + EUA_FH  + Gas2, data = nntrainscaled, stepmax=1e7)
predict(net, nntrain)

predict(net, nntrainscaled)
net <- neuralnet(ID1uSp ~ load_H + EUA_FH  + Gas2, data = nntrainscaled,  hidden=10)



#to do:
#EUA noch mit Tag vorher; aktuell noch normal?
#entsoe Daten direkt per API holen
#entsoe hat auch länderflüsse..
#REBAP tatsächliche werte pro monat? dann vorhersage auch nur monatlich?
#https://www.netztransparenz.de/Daten-zur-Regelenergie/reBAP/reBAP  jährlich downloadbar
#normaldaten selber mmachen??
#Pegas?
#Temperatur forecast und normal? dwd daten der letzten 15 JAhre und durchschnittstage +/- 7? dann müsste ich ja extreme ausdünnen.
#stichtag: Heizungsperiode als Datum?
#
#Normal von Consumption? Normal von Wind?
#Gas: Daten ist für den ktuellen Tag wohl schon da.. Ist das der vortages DA? Dann müsste ich ja nur d-1 nutzen
#https://www.smard.de/page/home/marktdaten/78?marketDataAttributes=%7B%22resolution%22:%22hour%22,%22region%22:%22DE%22,%22from%22:1695420000000,%22to%22:1696543199999,%22moduleIds%22:%5B6000411,2003791%5D,%22selectedCategory%22:null,%22activeChart%22:true,%22style%22:%22color%22,%22categoriesModuleOrder%22:%7B%7D%7D