

#Preise rein
df20 <- read.csv("data/DAPrices_20-21.csv")[,1:2]
df21 <- read.csv("data/DAPrices_21-22.csv")[,1:2]
df22 <- read.csv("data/DAPrices_22-23.csv")[,1:2]
df23 <- read.csv("data/DAPrices_23-24.csv")[,1:2]
df_entsoe <- rbind(df20, df21, df22, df23) %>% 
  rename("Zeitr" = "MTU..CET.CEST.", "Spot" = "Day.ahead.Price..EUR.MWh.") %>%
  mutate(t_begin = strsplit(Zeitr, " -.*"))
rm(df20, df21, df22, df23)

df_entsoe$t_begin <- as.character(df_entsoe$t_begin)
df_entsoe$t_begin <- as.POSIXct(df_entsoe$t_begin, format	= "%d.%m.%Y %H:%M") 
df_entsoe$Spot <- as.numeric(df_entsoe$Spot) 
df_entsoe <- df_entsoe %>%filter(!is.na(Spot))

#achtung: Auf Netztranspparenz ist nur der ID AEP Index, nicht aber der zu zahlende ReBap (erst 03/2023) veröffentlicht
#ist der index vllt eher als Preise zu sehen? aber dafür auch sehr hoch; ggfs weil QH stunden mal eskalieren.. 
df_AEI_QH <- read.csv2("data/ID AEP_netztransparenz_nicht_REBAP.csv") %>% 
  mutate(hour = as.factor(hour(hm(von)) + 1))
df_AEI_QH$Datum <- as.POSIXct(df_AEI_QH$Datum, format	= "%d.%m.%Y")

df_AEI <-
  df_AEI_QH %>% group_by(Datum, hour) %>%
  summarize(AE_H = mean(ID.AEP.in....MWh))
rm(df_AEI_QH)


df_entsoe$day <- date(df_entsoe$t_begin)
df_entsoe$hour <- as.factor(hour(df_entsoe$t_begin) + 1) 
df_entsoe$weekyear <- paste0(week(df_entsoe$day),"-", year(df_entsoe$day))

df_entsoe <- left_join(df_entsoe, df_AEI, by = c("hour" = "hour", "day" = "Datum"))


df_entsoe$AEuSp <- df_entsoe$AE_H - df_entsoe$Spot
rm(df_AEI)


###Renewable forecasts
df20_RE <- read.csv("data/Generation Forecasts for Wind and Solar_202001010000-202101010000.csv")
df21_RE <- read.csv("data/Generation Forecasts for Wind and Solar_202101010000-202201010000.csv")
df22_RE <- read.csv("data/Generation Forecasts for Wind and Solar_202201010000-202301010000.csv")
df23_RE <- read.csv("data/Generation Forecasts for Wind and Solar_202301010000-202401010000.csv")  

df_RE <- rbind( df20_RE, df21_RE, df22_RE, df23_RE)
df_RE <- df_RE %>% 
  rename(Zeitr_QH = MTU..CET.CEST.,
         WindOn_DA_QH = Generation...Wind.Onshore...MW..Day.Ahead..BZN.DE.LU, 
         WindOff_DA_QH = Generation...Wind.Offshore...MW..Day.Ahead..BZN.DE.LU,
         Solar_DA_QH = Generation...Solar...MW..Day.Ahead..BZN.DE.LU,  ) %>%
  select(Zeitr_QH, WindOn_DA_QH, WindOff_DA_QH, Solar_DA_QH) %>%
  mutate(t_begin = unlist(strsplit(Zeitr_QH, " -.*")))
df_RE$t_begin <- as.POSIXct(df_RE$t_begin, format	= "%d.%m.%Y %H:%M" ,tz = "CET") 


df_RE$WindOn_DA_QH <- as.numeric(df_RE$WindOn_DA_QH)
df_RE$WindOff_DA_QH <- as.numeric(df_RE$WindOff_DA_QH)
df_RE$Solar_DA_QH <- as.numeric(df_RE$Solar_DA_QH)

df_RE$day <- date(df_RE$t_begin)
df_RE$hour <- as.factor(hour(df_RE$t_begin) + 1)

df_RE_H <-
  df_RE %>% group_by(day, hour) %>%
  summarize(WindOn_H = mean(WindOn_DA_QH ),
            WindOff_H = mean(WindOff_DA_QH ),
            Solar_H = mean(Solar_DA_QH ))

df_entsoe <- left_join(df_entsoe, df_RE_H, by = c("day", "hour"))
rm(df20_RE, df21_RE,df22_RE, df23_RE, df_RE_H, df_RE)



##### Load #####
load23 <- read.csv("data/Total Load - Day Ahead _ Actual_202301010000-202401010000.csv")
load22 <- read.csv("data/Total Load - Day Ahead _ Actual_202201010000-202301010000.csv")

load21 <- read.csv("data/Total Load - Day Ahead _ Actual_202101010000-202201010000.csv")
load20 <- read.csv("data/Total Load - Day Ahead _ Actual_202001010000-202101010000.csv")

#df_load <- rbind(load21, load22, load23) %>% 
df_load <- rbind(load20, load21, load22, load23 ) %>% 
  rename(Zeitr_QH = Time..CET.CEST.,
         load_DA_QH = Day.ahead.Total.Load.Forecast..MW....BZN.DE.LU, 
         load_actual_QH = Actual.Total.Load..MW....BZN.DE.LU) %>%
  mutate(t_begin = unlist(strsplit(Zeitr_QH, " -.*")))
df_load$t_begin <- as.POSIXct(df_load$t_begin, format	= "%d.%m.%Y %H:%M" ,tz = "CET") 

df_load$load_DA_QH <- as.numeric(df_load$load_DA_QH)
df_load$load_actual_QH <- as.numeric(df_load$load_actual_QH)


df_load$day <- date(df_load$t_begin)
df_load$hour <- as.factor(hour(df_load$t_begin) + 1)


df_load_H <-
  df_load %>% group_by(day, hour) %>%
  summarize(load_H = mean(load_DA_QH ),
            load_actual_H = mean(load_actual_QH ))

df_entsoe <- left_join(df_entsoe, df_load_H, by = c("day", "hour")) 
rm(load20, load21,load22, load23, df_load_H, df_load)

#########ReBaP


# 
# ReBap20_1 <- read.csv2("data/ReBap/reBAP unterdeckt [2020_01].csv")
# ReBap20_7 <- read.csv2("data/ReBap/reBAP unterdeckt [2020_07].csv")
# ReBap21_1 <- read.csv2("data/ReBap/reBAP unterdeckt [2021_01].csv")
# ReBap21_7 <- read.csv2("data/ReBap/reBAP unterdeckt [2021_07].csv")
# ReBap22_1 <- read.csv2("data/ReBap/reBAP unterdeckt [2022_01].csv")
# ReBap22_7 <- read.csv2("data/ReBap/reBAP unterdeckt [2022_07].csv")
# ReBap23_1 <- read.csv2("data/ReBap/reBAP unterdeckt [2023_01].csv")
# df_reBAP_QH <- rbind(ReBap20_1, ReBap20_7, ReBap21_1, ReBap21_7, ReBap22_1 , ReBap22_7 , ReBap23_1) %>% 
#   rename(reBAP = reBAP.unterdeckt) %>% 
#   mutate(hour = hour(hm(von)) + 1)
# df_reBAP_QH$Datum <- as.POSIXct(df_reBAP_QH$Datum, format	= "%Y-%m-%d")
# 
# df_reBAP <-
#   df_reBAP_QH %>% group_by(Datum, hour) %>%
#   summarize(reBAP_H = mean(reBAP))
# rm(df_reBAP_QH)
# rm(ReBap20_1, ReBap20_7, ReBap21_1, ReBap21_7, ReBap22_1, ReBap22_7, ReBap23_1)
# 
# df_entsoe <- left_join(df_entsoe, df_reBAP, by = c("hour" = "hour", "day" = "Datum"))
# df_entsoe$reBAPuSp <- df_entsoe$reBAP_H - df_entsoe$Spot
# rm(df_reBAP)
