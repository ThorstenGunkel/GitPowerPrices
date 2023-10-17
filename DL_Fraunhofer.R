#https://energy-charts.info/charts/price_spot_market/data/de/

library("dplyr")
library("lubridate")
#library("ggplot2")
#library("tidyr")
library("jsonlite")
#Fraunhofer Daten für Last, 
FH_raw23 <- read_json("https://energy-charts.info/charts/price_spot_market/data/de/year_2023.json")

FH_raw23[[5]]$data[lengths(FH_raw23[[5]]$data) == 0] <- NA
FH_raw23[[7]]$data[lengths(FH_raw23[[7]]$data) == 0] <- NA
FH_raw23[[12]]$data[lengths(FH_raw23[[12]]$data) == 0] <- NA
FH_raw23[[13]]$data[lengths(FH_raw23[[13]]$data) == 0] <- NA
#FH_raw23[[13]]$data[lengths(FH_raw23[[14]]$data) == 0] <- NA

FH_23 <- data.frame(as.POSIXct(unlist(FH_raw23[[1]]$xAxisValues)/1000),
                    unlist(FH_raw23[[5]]$data),
                    unlist(FH_raw23[[7]]$data),
                    unlist(FH_raw23[[12]]$data),
                    unlist(FH_raw23[[13]]$data),
                    unlist(FH_raw23[[14]]$data))

colnames(FH_23) <- c("Date","load_FH","DA_FH","ID3_FH", "ID1_FH","EUA_FH")
#FH_23$Tag <- date(FH_23$Date)
#FH_23$hour <- as.factor(hour(FH_23$Date) + 1)

#######2022

FH_raw22 <- read_json("https://energy-charts.info/charts/price_spot_market/data/de/year_2022.json")

FH_22 <- data.frame(as.POSIXct(unlist(FH_raw22[[1]]$xAxisValues)/1000),
                    unlist(FH_raw22[[5]]$data),
                    unlist(FH_raw22[[6]]$data),
                    unlist(FH_raw22[[11]]$data),
                    unlist(FH_raw22[[12]]$data),
                    unlist(FH_raw22[[13]]$data))
colnames(FH_22) <- c("Date","load_FH","DA_FH","ID3_FH", "ID1_FH","EUA_FH")

####2021
FH_raw21 <- read_json("https://energy-charts.info/charts/price_spot_market/data/de/year_2021.json")

FH_21 <- data.frame(as.POSIXct(unlist(FH_raw21[[1]]$xAxisValues)/1000),
                    unlist(FH_raw21[[5]]$data),
                    unlist(FH_raw21[[6]]$data),
                    unlist(FH_raw21[[11]]$data),
                    unlist(FH_raw21[[12]]$data),
                    unlist(FH_raw21[[13]]$data))
colnames(FH_21) <- c("Date","load_FH","DA_FH","ID3_FH", "ID1_FH","EUA_FH")


#####2020
FH_raw20 <- read_json("https://energy-charts.info/charts/price_spot_market/data/de/year_2020.json")
FH_raw20[[11]]$data[lengths(FH_raw20[[11]]$data) == 0] <- NA

FH_20 <- data.frame(as.POSIXct(unlist(FH_raw20[[1]]$xAxisValues)/1000),
                    unlist(FH_raw20[[5]]$data),
                    unlist(FH_raw20[[6]]$data),
                    unlist(FH_raw20[[11]]$data),
                    unlist(FH_raw20[[12]]$data),
                    unlist(FH_raw20[[13]]$data))
colnames(FH_20) <- c("Date","load_FH","DA_FH","ID3_FH", "ID1_FH","EUA_FH")




Fraunhofer <- rbind(FH_23,FH_22, FH_21, FH_20) 
rm(FH_23,FH_22, FH_21, FH_20, FH_raw23, FH_raw22, FH_raw21, FH_raw20)

Fraunhofer$Tag <- date(Fraunhofer$Date)
Fraunhofer$hour <- as.factor(hour(Fraunhofer$Date) + 1)

Fraunhofer <- Fraunhofer %>%
  distinct(Tag, hour,  .keep_all = TRUE) %>%
  select(-Date)



####temp
temp2m_raw23 <- read_json("https://energy-charts.info/charts/climate_hours/data/de/year_air_temperature_2023.json")
temp2m_raw23 <- temp2m_raw23[[1]]$data
temp2m_raw23_unlist <- unlist(lapply(temp2m_raw23, `[`, 3))

#tetstcol <- cbind("wert"      = unlist(lapply(temp2m_raw23, `[`, 3)))
length(temp2m_raw23_unlist) <-  length(temp2m_raw23)

temp2m23 <- data.frame(cbind("daynumber" = unlist(lapply(temp2m_raw23, `[`, 1))+1),
                       cbind("hour"      = unlist(lapply(temp2m_raw23, `[`, 2)))+1,
                       "temp2m" = temp2m_raw23_unlist)

temp2m23 <- temp2m23 %>% mutate(day = as.Date(paste(daynumber,2023), format="%j %Y"))

##2022
temp2m_raw22 <- read_json("https://energy-charts.info/charts/climate_hours/data/de/year_air_temperature_2022.json")
temp2m_raw22 <- temp2m_raw22[[1]]$data
temp2m_raw22_unlist <- unlist(lapply(temp2m_raw22, `[`, 3))

#tetstcol <- cbind("wert"      = unlist(lapply(temp2m_raw22, `[`, 3)))
#length(temp2m_raw22_unlist) <-  length(temp2m_raw22)

temp2m22 <- data.frame(cbind("daynumber" = unlist(lapply(temp2m_raw22, `[`, 1))+1),
                       cbind("hour"      = unlist(lapply(temp2m_raw22, `[`, 2)))+1,
                       "temp2m" = temp2m_raw22_unlist)

temp2m22 <- temp2m22 %>% mutate(day = as.Date(paste(daynumber,2022), format="%j %Y"))

#2021
##2021
temp2m_raw21 <- read_json("https://energy-charts.info/charts/climate_hours/data/de/year_air_temperature_2021.json")
temp2m_raw21 <- temp2m_raw21[[1]]$data
temp2m_raw21_unlist <- unlist(lapply(temp2m_raw21, `[`, 3))

#tetstcol <- cbind("wert"      = unlist(lapply(temp2m_raw21, `[`, 3)))
#length(temp2m_raw21_unlist) <-  length(temp2m_raw21)

temp2m21 <- data.frame(cbind("daynumber" = unlist(lapply(temp2m_raw21, `[`, 1))+1),
                       cbind("hour"      = unlist(lapply(temp2m_raw21, `[`, 2)))+1,
                       "temp2m" = temp2m_raw21_unlist)

temp2m21 <- temp2m21 %>% mutate(day = as.Date(paste(daynumber,2021), format="%j %Y"))


##2020
temp2m_raw20 <- read_json("https://energy-charts.info/charts/climate_hours/data/de/year_air_temperature_2020.json")
temp2m_raw20 <- temp2m_raw20[[1]]$data
temp2m_raw20_unlist <- unlist(lapply(temp2m_raw20, `[`, 3))

#tetstcol <- cbind("wert"      = unlist(lapply(temp2m_raw20, `[`, 3)))
#length(temp2m_raw20_unlist) <-  length(temp2m_raw20)

temp2m20 <- data.frame(cbind("daynumber" = unlist(lapply(temp2m_raw20, `[`, 1))+1),
                       cbind("hour"      = unlist(lapply(temp2m_raw20, `[`, 2)))+1,
                       "temp2m" = temp2m_raw20_unlist)

temp2m20 <- temp2m20 %>% mutate(day = as.Date(paste(daynumber,2020), format="%j %Y"))

###2019
temp2m_raw19 <- read_json("https://energy-charts.info/charts/climate_hours/data/de/year_air_temperature_2019.json")
temp2m_raw19 <- temp2m_raw19[[1]]$data
temp2m_raw19_unlist <- unlist(lapply(temp2m_raw19, `[`, 3))

#tetstcol <- cbind("wert"      = unlist(lapply(temp2m_raw19, `[`, 3)))
#length(temp2m_raw19_unlist) <-  length(temp2m_raw19)

temp2m19 <- data.frame(cbind("daynumber" = unlist(lapply(temp2m_raw19, `[`, 1))+1),
                       cbind("hour"      = unlist(lapply(temp2m_raw19, `[`, 2)))+1,
                       "temp2m" = temp2m_raw19_unlist)

temp2m19 <- temp2m19 %>% mutate(day = as.Date(paste(daynumber,2019), format="%j %Y"))

###2018
temp2m_raw18 <- read_json("https://energy-charts.info/charts/climate_hours/data/de/year_air_temperature_2018.json")
temp2m_raw18 <- temp2m_raw18[[1]]$data
temp2m_raw18_unlist <- unlist(lapply(temp2m_raw18, `[`, 3))

#tetstcol <- cbind("wert"      = unlist(lapply(temp2m_raw18, `[`, 3)))
#length(temp2m_raw18_unlist) <-  length(temp2m_raw18)

temp2m18 <- data.frame(cbind("daynumber" = unlist(lapply(temp2m_raw18, `[`, 1))+1),
                       cbind("hour"      = unlist(lapply(temp2m_raw18, `[`, 2)))+1,
                       "temp2m" = temp2m_raw18_unlist)

temp2m18 <- temp2m18 %>% mutate(day = as.Date(paste(daynumber,2018), format="%j %Y"))

###2017
temp2m_raw17 <- read_json("https://energy-charts.info/charts/climate_hours/data/de/year_air_temperature_2017.json")
temp2m_raw17 <- temp2m_raw17[[1]]$data
temp2m_raw17_unlist <- unlist(lapply(temp2m_raw17, `[`, 3))

#tetstcol <- cbind("wert"      = unlist(lapply(temp2m_raw17, `[`, 3)))
#length(temp2m_raw17_unlist) <-  length(temp2m_raw17)

temp2m17 <- data.frame(cbind("daynumber" = unlist(lapply(temp2m_raw17, `[`, 1))+1),
                       cbind("hour"      = unlist(lapply(temp2m_raw17, `[`, 2)))+1,
                       "temp2m" = temp2m_raw17_unlist)

temp2m17 <- temp2m17 %>% mutate(day = as.Date(paste(daynumber,2017), format="%j %Y"))


###2016
temp2m_raw16 <- read_json("https://energy-charts.info/charts/climate_hours/data/de/year_air_temperature_2016.json")
temp2m_raw16 <- temp2m_raw16[[1]]$data
temp2m_raw16_unlist <- unlist(lapply(temp2m_raw16, `[`, 3))

#tetstcol <- cbind("wert"      = unlist(lapply(temp2m_raw16, `[`, 3)))
#length(temp2m_raw16_unlist) <-  length(temp2m_raw16)

temp2m16 <- data.frame(cbind("daynumber" = unlist(lapply(temp2m_raw16, `[`, 1))+1),
                       cbind("hour"      = unlist(lapply(temp2m_raw16, `[`, 2)))+1,
                       "temp2m" = temp2m_raw16_unlist)

temp2m16 <- temp2m16 %>% mutate(day = as.Date(paste(daynumber,2016), format="%j %Y"))




temp2m <- rbind(temp2m16, temp2m17, temp2m18, temp2m19, temp2m20,temp2m21,temp2m22,temp2m23)

#temp2m <- rbind(temp2m20,temp2m21,temp2m22,temp2m23)
temp2m$hour <- as.factor(temp2m$hour)
#temp2m <- temp2m %>% select(-daynumber)
rm(temp2m16,temp2m17,temp2m18, temp2m19, temp2m_raw16, temp2m_raw17, temp2m_raw18, temp2m_raw19)

rm(temp2m20,temp2m21,temp2m22,temp2m23, temp2m_raw20, temp2m_raw21, temp2m_raw22, temp2m_raw23)
rm(temp2m_raw16_unlist,temp2m_raw17_unlist,temp2m_raw18_unlist,temp2m_raw19_unlist)
rm(temp2m_raw20_unlist,temp2m_raw21_unlist,temp2m_raw22_unlist,temp2m_raw23_unlist)

#temp2m %>% group_by(hour, day) %>%
#  mutate(avgtemp = )

temp2m$year <- year(temp2m$day)

#stunde <- 1
#tag = as.POSIXct("2022-09-22")


starttime <- Sys.time()

d7 <- days(7) 
y1 <- years(1)
y2 <- years(2)
y3 <- years(3)
y4 <- years(4)
y5 <- years(5)


for (tag in unique(temp2m$day)[unique(temp2m$day) > "2020-01-01"]) {
  Datebetween <- between(temp2m$day, as.Date(tag) -d7 - y1, as.Date(tag) + d7 - y1 ) |
    between(temp2m$day, as.Date(tag) -d7 - y2, as.Date(tag) + d7 - y2 ) |
    between(temp2m$day, as.Date(tag) -d7 - y3, as.Date(tag) + d7 - y3 ) |
    between(temp2m$day, as.Date(tag) -d7 - y4, as.Date(tag) + d7 - y4 ) |
    between(temp2m$day, as.Date(tag) -d7 - y5, as.Date(tag) + d7 - y5 )
  for (stunde in 1:24) {
    temp2m$tempavg[temp2m$hour == stunde & temp2m$day == tag ] = mean(
      temp2m$temp2m[temp2m$hour == stunde & Datebetween]
    )
  }
}
endtime <- Sys.time() 
endtime - starttime 


#.internal(mean) . soll laut internet wahnsinnig schneller sein, macht aber dast keine untershied
#verbessern: 


#führt aktuell dazu, dass in 2020 nur die letzten Tage, die schon Ende 2018+7 Tage ziehen
temp2m <- temp2m %>% mutate(tempDiff = temp2m - tempavg)

#Gas 
#Gas_raw_2023 
Gas_raw23 <- read_json("https://energy-charts.info/charts/price_average/data/de/day_euro_mwh_2023.json")
Gas_raw22 <- read_json("https://energy-charts.info/charts/price_average/data/de/day_euro_mwh_2022.json")
Gas_raw21 <- read_json("https://energy-charts.info/charts/price_average/data/de/day_euro_mwh_2021.json")
Gas_raw20 <- read_json("https://energy-charts.info/charts/price_average/data/de/day_euro_mwh_2020.json")

#Gas_raw23[[13]]$data[lengths(Gas_raw23[[14]]$data) == 0] <- NA
Gas_raw23[[16]]$data[lengths(Gas_raw23[[16]]$data) == 0] <- NA

Gas_2023 <- data.frame(as.POSIXct(unlist(Gas_raw23[[1]]$xAxisValues), format = "%d.%m.%Y",),
                       unlist(Gas_raw23[[16]]$data))
colnames(Gas_2023) <- c("Date","NCG_sameday")


Gas_raw22[[16]]$data[lengths(Gas_raw22[[16]]$data) == 0] <- NA
Gas_2022 <- data.frame(as.POSIXct(unlist(Gas_raw22[[1]]$xAxisValues), format = "%d.%m.%Y",),
                       unlist(Gas_raw22[[16]]$data))
colnames(Gas_2022) <- c("Date","NCG_sameday")


Gas_raw21[[16]]$data[lengths(Gas_raw21[[16]]$data) == 0] <- NA
Gas_2021 <- data.frame(as.POSIXct(unlist(Gas_raw21[[1]]$xAxisValues), format = "%d.%m.%Y",),
                       unlist(Gas_raw21[[16]]$data))
colnames(Gas_2021) <- c("Date","NCG_sameday")

Gas_raw20[[16]]$data[lengths(Gas_raw20[[16]]$data) == 0] <- NA
Gas_2020 <- data.frame(as.POSIXct(unlist(Gas_raw20[[1]]$xAxisValues), format = "%d.%m.%Y",),
                       unlist(Gas_raw20[[16]]$data))
colnames(Gas_2020) <- c("Date","NCG_sameday")

Gas <- rbind(Gas_2023, Gas_2022, Gas_2021, Gas_2020)
Gas <- Gas[order(Gas$Date),]
Gas <- Gas %>% mutate(Gas2 = lag(NCG_sameday, 2))
Gas <- Gas %>% mutate(Gas1 = lag(NCG_sameday, 1))
rm(Gas_2023, Gas_2022, Gas_2021, Gas_2020, Gas_raw23, Gas_raw22, Gas_raw21, Gas_raw20)
#Gas noch jeweils einen Tag zurück; dann: fill down.


#boxplot(ID1_FH ~ hour, data = Fraunhofer)
#boxplot(load ~ hour, data = Gas)


