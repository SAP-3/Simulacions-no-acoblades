library(datetime)
library(dplyr)
library(zoo)
library(tseries)
library(forecast)

wx <- read.csv('C:\\Users\\abril\\OneDrive\\UAB\\SAP\\Projecte\\WX.csv', header=TRUE)
x6 <- read.csv('C:\\Users\\abril\\OneDrive\\UAB\\SAP\\Projecte\\X6.csv', header=TRUE)

process_data <- function(data){ 
  datetime <- data$DATA_LECTURA
  datetime <- as.datetime(datetime, format = "%d/%m/%Y %I:%M:%S %p")
  data$DATA_LECTURA <- datetime
  print("Dates and time processed")
  
  temp <- subset(data, CODI_VARIABLE==32, select = c('VALOR_LECTURA', 'DATA_LECTURA'))
  vvent <- subset(data, CODI_VARIABLE==46, select = c('VALOR_LECTURA', 'DATA_LECTURA'))
  dirvent <- subset(data, CODI_VARIABLE==57, select = c('VALOR_LECTURA', 'DATA_LECTURA'))
  temp <- subset(temp, format(DATA_LECTURA, "%M")=="00")
  vvent <- subset(vvent, format(DATA_LECTURA, "%M")=="00")
  dirvent <- subset(dirvent, format(DATA_LECTURA, "%M")=="00")
  print("Variables detected")
  
  combined <- temp %>% full_join(vvent, by='DATA_LECTURA')
  combined <- combined %>% full_join(dirvent, by='DATA_LECTURA')
  combined <- combined %>% rename(TEMPERATURA = VALOR_LECTURA.x)
  combined <- combined %>% rename(VEL_VENT = VALOR_LECTURA.y)
  combined <- combined %>% rename(DIR_VENT = VALOR_LECTURA)
  data <- combined %>% select(DATA_LECTURA, TEMPERATURA, VEL_VENT, DIR_VENT)
  data <- subset(data, as.numeric(format(DATA_LECTURA, '%m'))>=5 & as.numeric(format(DATA_LECTURA, '%m'))<=9)
  print("Dataframe combined")
  
  inici <- first(data$DATA_LECTURA)
  final <- last(data$DATA_LECTURA)
  dates <- seq(from=inici, to=final, by=as.time('01:00'))
  dates <- dates[as.numeric(format(dates, '%m'))>=5 & as.numeric(format(dates,'%m'))<=9]
  diff_dates <- as.datetime(setdiff(as.character(dates), as.character(data$DATA_LECTURA)))
  
  new_dates <- data.frame(DATA_LECTURA = diff_dates, TEMPERATURA = NA, VEL_VENT = NA, DIR_VENT = NA)
  merged <- merge(new_dates, data, all=TRUE)
  print("Dataframe completed")
  
  return(merged)
}

discretitzar_direccio <- function(data){
  dirs <- rep('', length(data$DIR_VENT))
  dirs[which(data$DIR_VENT < 22.5 | data$DIR_VENT > 337.5)] <- 'N'
  dirs[which(data$DIR_VENT < 67.5 & data$DIR_VENT > 22.5)] <- 'NW'
  dirs[which(data$DIR_VENT < 112.5 & data$DIR_VENT > 67.5)] <- 'W'
  dirs[which(data$DIR_VENT < 157.5 & data$DIR_VENT > 112.5)] <- 'SW'
  dirs[which(data$DIR_VENT < 202.5 & data$DIR_VENT > 157.5)] <- 'S'
  dirs[which(data$DIR_VENT < 247.5 & data$DIR_VENT > 202.5)] <- 'SE'
  dirs[which(data$DIR_VENT < 292.5 & data$DIR_VENT > 247.5)] <- 'E'
  dirs[which(data$DIR_VENT < 337.5 & data$DIR_VENT > 292.5)] <- 'NE'
  return(dirs)
}

# PROCESSAMENT INICIAL DATASETS -------------------------------------------------
data_wx <- process_data(wx)
data_x6 <- process_data(x6)


# DATES INCENDI ------------------------------------------------------------------
ini_incendi <- as.datetime('15/06/2022 12', format = "%d/%m/%Y %I")
fin_incendi <- as.datetime('19/06/2022 11 pm', format = "%d/%m/%Y %I %p")

dates_incendi <- seq(ini_incendi, fin_incendi, by=as.time('01:00'))
data_incendi_wx <- subset(data_wx, DATA_LECTURA >= ini_incendi & DATA_LECTURA <= fin_incendi)
data_incendi_x6 <- subset(data_x6, DATA_LECTURA >= ini_incendi & DATA_LECTURA <= fin_incendi)


# PROBABILITAT DE TROBAR UNA TEMPERATURA --------------------------------------
mean_temps <- mapply(function(x,y) mean(c(x,y), na.rm=TRUE), data_wx$TEMPERATURA, data_x6$TEMPERATURA, SIMPLIFY = TRUE)
df_temps <- data.frame(list(TEMPERATURA = mean_temps,
                            YEAR = format(data_wx$DATA_LECTURA, '%y'),
                            MONTH = format(data_wx$DATA_LECTURA, '%m'),
                            DAY = format(data_wx$DATA_LECTURA, '%d'),
                            HOUR = format(data_wx$DATA_LECTURA, '%H'),
                            MDH = format(data_wx$DATA_LECTURA, '%m-%d %H')))
rm(mean_temps)
hist(df_temps$TEMPERATURA, main='Histograma de temperatures',
     xlab='Temperatura', ylab='Freqüència', col='lightblue')
new_year <- which(format(data_wx$DATA_LECTURA, '%m-%d %H')=='05-01 00')

year_temp <- aggregate(TEMPERATURA ~ MDH, df_temps, function(x){
  c(mean(x),sd(x))
})

mm24 <- rollmean(year_temp$TEMPERATURA[,1], 24, align='right', fill=NA)
idx_ini <- which(year_temp$MDH == '06-15 00')
idx_fin <- which(year_temp$MDH == '06-19 23')

plot(year_temp$TEMPERATURA[,1], type='l', col='lightgray', xaxt='n',
     ylim=c(7, 33), main='Temperatura mitjana per hora',
     xlab='Mes', ylab='Temperatura')
lines(mm24, col='blue', lwd=2)
ini_months <- c(which(substr(year_temp$MDH, 4, 8)=='01 00'), length(year_temp$MDH))
for (i in ini_months){
  abline(v=i, col='black',lty=2)
}
abline(v=idx_ini, col='red', lty=2)
abline(v=idx_fin, col='red', lty=2)
legend("topleft", 
       legend=c("Temperatura", "Tendència", "Incendi"),
       col = c("lightgray", "blue", 'red'),
       lty = c(1,1,2),
       cex = 1)
axis(1, at=ini_months, labels=c('Maig','Juny','Juliol','Agost','Setembre','Octubre'))


temp_incendi <- data.frame(list(MDH=year_temp$MDH[idx_ini:idx_fin],
                                TEMP_MITJANA = year_temp$TEMPERATURA[,1][idx_ini:idx_fin],
                                TEMP_SD = year_temp$TEMPERATURA[,2][idx_ini:idx_fin]))
plot(temp_incendi$TEMP_MITJANA, type='l', col='darkblue', xaxt='n', lwd=2,
     main = "Temperatura mitjana per hora a les dates de l'incendi",
     xlab='Dia del mes de juny', ylab='Temperatura')
days_axis <- seq(1,length(temp_incendi$MDH)+1, by=24)
days <- seq(15,20)
axis(1, at=days_axis, labels=as.character(days))



# DISCRETITZAR DIRECCIONS DEL VENT ---------------------------------------------
data_wx$DIR_DISCRETA <- discretitzar_direccio(data_wx)
data_x6$DIR_DISCRETA <- discretitzar_direccio(data_x6)
disc_dirs_wx <- aggregate(VEL_VENT~DIR_DISCRETA, data_wx, function(x){
  length(x)
})$VEL_VENT[2:9]
disc_dirs_x6 <- aggregate(VEL_VENT~DIR_DISCRETA, data_x6, function(x){
  length(x)
})$VEL_VENT[2:9]
barplot(disc_dirs_wx, names.arg = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
        main = "Histograma de les direccions del vent de l'estació WX",
        xlab = 'Direccions del vent', ylab='Freqüència', col = 'lightblue')
barplot(disc_dirs_x6, names.arg = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
        main = "Histograma de les direccions del vent de l'estació X6",
        xlab = 'Direccions del vent', ylab='Freqüència', col = 'lightblue')



# ANALISIS D'INTENSITAT DEL VENT -----------------------------------------------
hist(data_wx$VEL_VENT, breaks = 30, main='Histograma de velocitats del vent (Estació WX)',
     xlab='Velocitat (m/s)', ylab='Freqüència')
hist(data_x6$VEL_VENT, breaks = 38, main='Histograma de velocitats del vent (Estació X6)',
     xlab='Velocitat (m/s)', ylab='Freqüència')

# Sortida i posta del sol a mitjans de juliol 6:20 i 21:30
vel_dia_wx <- subset(data_wx, 
                     as.numeric(format(data_wx$DATA_LECTURA, '%H'))>=6 & as.numeric(format(data_wx$DATA_LECTURA, '%H'))<=21)
vel_nit_wx <- subset(data_wx, 
                     as.numeric(format(data_wx$DATA_LECTURA, '%H'))<6 | as.numeric(format(data_wx$DATA_LECTURA, '%H'))>21)
vel_dia_x6 <- subset(data_x6, 
                     as.numeric(format(data_x6$DATA_LECTURA, '%H'))>=6 & as.numeric(format(data_x6$DATA_LECTURA, '%H'))<=21)
vel_nit_x6 <- subset(data_x6, 
                     as.numeric(format(data_x6$DATA_LECTURA, '%H'))<6 | as.numeric(format(data_x6$DATA_LECTURA, '%H'))>21)



# CALCUL DE PROBABILITATS --------------------------------------------------------
probs_direccio_wx <- data.frame(list(N = length(data_wx$DIR_VENT[data_wx$DIR_DISCRETA=='N'])/length(data_wx$DIR_DISCRETA),
                                     NW = length(data_wx$DIR_VENT[data_wx$DIR_DISCRETA=='NW'])/length(data_wx$DIR_DISCRETA),
                                     W = length(data_wx$DIR_VENT[data_wx$DIR_DISCRETA=='W'])/length(data_wx$DIR_DISCRETA),
                                     SW = length(data_wx$DIR_VENT[data_wx$DIR_DISCRETA=='SW'])/length(data_wx$DIR_DISCRETA),
                                     S = length(data_wx$DIR_VENT[data_wx$DIR_DISCRETA=='S'])/length(data_wx$DIR_DISCRETA),
                                     SE = length(data_wx$DIR_VENT[data_wx$DIR_DISCRETA=='SE'])/length(data_wx$DIR_DISCRETA),
                                     E = length(data_wx$DIR_VENT[data_wx$DIR_DISCRETA=='E'])/length(data_wx$DIR_DISCRETA),
                                     NE = length(data_wx$DIR_VENT[data_wx$DIR_DISCRETA=='NE'])/length(data_wx$DIR_DISCRETA)))

probs_direccio_x6 <- data.frame(list(N = length(data_x6$DIR_VENT[data_x6$DIR_DISCRETA=='N'])/length(data_x6$DIR_DISCRETA),
                                     NW = length(data_x6$DIR_VENT[data_x6$DIR_DISCRETA=='NW'])/length(data_x6$DIR_DISCRETA),
                                     W = length(data_x6$DIR_VENT[data_x6$DIR_DISCRETA=='W'])/length(data_x6$DIR_DISCRETA),
                                     SW = length(data_x6$DIR_VENT[data_x6$DIR_DISCRETA=='SW'])/length(data_x6$DIR_DISCRETA),
                                     S = length(data_x6$DIR_VENT[data_x6$DIR_DISCRETA=='S'])/length(data_x6$DIR_DISCRETA),
                                     SE = length(data_x6$DIR_VENT[data_x6$DIR_DISCRETA=='SE'])/length(data_x6$DIR_DISCRETA),
                                     E = length(data_x6$DIR_VENT[data_x6$DIR_DISCRETA=='E'])/length(data_x6$DIR_DISCRETA),
                                     NE = length(data_x6$DIR_VENT[data_x6$DIR_DISCRETA=='NE'])/length(data_x6$DIR_DISCRETA)))


pie(c(probs_direccio_wx$N, probs_direccio_wx$NW, probs_direccio_wx$W, probs_direccio_wx$SW,
      probs_direccio_wx$S, probs_direccio_wx$SE, probs_direccio_wx$E, probs_direccio_wx$NE),
    labels = c('N', 'NW', 'W', 'SW', 'S', 'SE', 'E', 'NE'),
    col = colorRampPalette(c("lightblue", "darkblue"))(length(probs_direccio_wx)),
    main = "Distribució d'aparició de direccions del vent (Estació WX)")

pie(c(probs_direccio_x6$N, probs_direccio_x6$NW, probs_direccio_x6$W, probs_direccio_x6$SW,
      probs_direccio_x6$S, probs_direccio_x6$SE, probs_direccio_x6$E, probs_direccio_x6$NE),
    labels = c('N', 'NW', 'W', 'SW', 'S', 'SE', 'E', 'NE'),
    col = colorRampPalette(c("lightblue", "darkblue"))(length(probs_direccio_x6)),
    main = "Distribució d'aparició de direccions del vent (Estació X6)")


# CREEM TEMPERATURA PERCENTIL 75, 85, 95, 98 -------------------------------------
centils <- c(0.75, 0.85, 0.95, 0.98)
temperatures_centils <- matrix(0, nrow=length(dates_incendi), ncol=length(centils))
for (j in 1:length(centils)){
  for (i in 1:length(dates_incendi)){
    temperatures_centils[i,j] <- qnorm(centils[j], temp_incendi$TEMP_MITJANA[i], temp_incendi$TEMP_SD[i])
  }
}
temperatures_centils <- data.frame(temperatures_centils)
names(temperatures_centils) <- paste('C', as.character(centils), sep='')
temperatures_centils$DATA <- dates_incendi


# CREEM VELOCITAT PER PERCENTILS ------------------------------------------------
velocitats_centils_wx <- matrix(0, nrow=length(dates_incendi), ncol=length(centils))
velocitats_centils_x6 <- matrix(0, nrow=length(dates_incendi), ncol=length(centils))
for (j in 1:length(centils)){
  for (i in 1:length(dates_incendi)){
    centil <- centils[j]
    dia_wx <- quantile(vel_dia_wx$VEL_VENT, centil, na.rm=TRUE)
    nit_wx <- quantile(vel_nit_wx$VEL_VENT, centil, na.rm=TRUE)
    dia_x6 <- quantile(vel_dia_x6$VEL_VENT, centil, na.rm=TRUE)
    nit_x6 <- quantile(vel_nit_x6$VEL_VENT, centil, na.rm=TRUE)
    if (as.numeric(format(dates_incendi[i], '%H'))>=6 & as.numeric(format(dates_incendi[i], '%H'))<=21){ #DIA
      velocitats_centils_wx[i,j] <- dia_wx
      velocitats_centils_x6[i,j] <- dia_x6
    }
    else{ #NIT
      velocitats_centils_wx[i,j] <- nit_wx
      velocitats_centils_x6[i,j] <- nit_x6
    }
  }
}
velocitats_centils_wx <- data.frame(velocitats_centils_wx)
velocitats_centils_x6 <- data.frame(velocitats_centils_x6)
names(velocitats_centils_wx) <- paste('C', as.character(centils), sep='')
names(velocitats_centils_x6) <- paste('C', as.character(centils), sep='')
velocitats_centils_wx$DATA <- dates_incendi
velocitats_centils_x6$DATA <- dates_incendi



#================================================================================
#                                 CREACIO CSVs
#================================================================================
estacio <- c('wx', 'x6')
dia <- c(1,0)

ests_dataframe <- c()
dies_dataframe <- c()
centils_dataframe <- c()
vels_dataframe <- c()

for (centil in centils){
  print(paste('Centil:', as.character(centil)))
  for (d in dia){
    print(paste('Dia', as.character(d)))
    for (est in estacio){
      print(paste('Estacio:',est))
      ests_dataframe <- c(ests_dataframe, est)
      dies_dataframe <- c(dies_dataframe, d)
      centils_dataframe <- c(centils_dataframe, centil)
      if (est=='wx'){
        vels_dataframe <- c(vels_dataframe, velocitats_centils_wx[6+d,paste('C',as.character(centil),sep="")])
      }
      else{
        vels_dataframe <- c(vels_dataframe, velocitats_centils_x6[6+d,paste('C',as.character(centil),sep="")])
      }
    }
  }
}

velocitats <- data.frame(
  dia <- dies_dataframe,
  centil <- centils_dataframe,
  estacio <- ests_dataframe,
  velocitat <- vels_dataframe
)
names(velocitats) = c('DIA', 'CENTIL', 'ESTACIO', 'VELOCITAT')

write.csv(velocitats, file='velocitats.csv', row.names=FALSE)




#================================================================================
#                               PREPARACIO .wtr
#================================================================================
days <- c(15, 16, 17)
tminmax75 <- rep(0,6)
tminmax85 <- rep(0,6)
tminmax95 <- rep(0,6)
tminmax98 <- rep(0,6)
hminmax75 <- rep(0,6)
hminmax85 <- rep(0,6)
hminmax95 <- rep(0,6)
hminmax98 <- rep(0,6)

for (day in 1:3){
  subset_dia <- subset(temperatures_centils, format(DATA, "%d")==as.character(days[day]))
  daymax <- day+3
  tminmax75[day] <- min(subset_dia$C0.75)
  tminmax85[day] <- min(subset_dia$C0.85)
  tminmax95[day] <- min(subset_dia$C0.95)
  tminmax98[day] <- min(subset_dia$C0.98)
  tminmax75[daymax] <- max(subset_dia$C0.75)
  tminmax85[daymax] <- max(subset_dia$C0.85)
  tminmax95[daymax] <- max(subset_dia$C0.95)
  tminmax98[daymax] <- max(subset_dia$C0.98)
  hminmax75[day] <- format(subset_dia$DATA[which(subset_dia$C0.75==tminmax75[day])], '%H')
  hminmax85[day] <- format(subset_dia$DATA[which(subset_dia$C0.85==tminmax85[day])], '%H')
  hminmax95[day] <- format(subset_dia$DATA[which(subset_dia$C0.95==tminmax95[day])], '%H')
  hminmax98[day] <- format(subset_dia$DATA[which(subset_dia$C0.98==tminmax98[day])], '%H')
  hminmax75[daymax] <- format(subset_dia$DATA[which(subset_dia$C0.75==tminmax75[daymax])], '%H')
  hminmax85[daymax] <- format(subset_dia$DATA[which(subset_dia$C0.85==tminmax85[daymax])], '%H')
  hminmax95[daymax] <- format(subset_dia$DATA[which(subset_dia$C0.95==tminmax95[daymax])], '%H')
  hminmax98[daymax] <- format(subset_dia$DATA[which(subset_dia$C0.98==tminmax98[daymax])], '%H')
}

temps <- data.frame(
  dia <- c(days, days, days, days),
  hmin <- c(hminmax75[1:3], hminmax85[1:3], hminmax95[1:3], hminmax98[1:3]),
  hmax <- c(hminmax75[4:6], hminmax85[4:6], hminmax95[4:6], hminmax98[4:6]),
  tmin <- c(tminmax75[1:3], tminmax85[1:3], tminmax95[1:3], tminmax98[1:3]),
  tmax <- c(tminmax75[4:6], tminmax85[4:6], tminmax95[4:6], tminmax98[4:6]),
  centil <- c(0.75,0.75,0.75,0.85,0.85,0.85,0.95,0.95,0.95,0.98,0.98,0.98)
)

names(temps) <- c('DIA', 'H_MIN', 'H_MAX', 'T_MIN', 'T_MAX', 'CENTIL')

write.csv(temps, file='temperatures.csv', row.names=FALSE)
