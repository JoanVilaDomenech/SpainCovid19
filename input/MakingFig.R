library(gdata)
library(Hmisc)
library(curl)
x <- read.csv( curl("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
startdate <- "2020-02-01"
# selecting Spain
s <- subset(x, Country.Region=="Spain")
dat <- data.frame(t(s))
names(dat) <- "CumCases"
dat$date <- as.Date(sub("X", "", rownames(dat)), format="%m.%d.%y")
dat <- dat[!is.na(dat$date), ]
# change factor to numeric
dat$CumCases <- as.numeric(as.character(dat$CumCases))
# subseting since 1st date
dat <- subset(dat, date >=startdate)
# Daily deaths
dat$DailyCases <- with(dat, CumCases-c(0, dat$CumCases[1:nrow(dat)-1]))
dat$id <- seq(1, nrow(dat))
xdat <- subset(dat, id < 51)
################################################################################
jpeg(file="./fig/fig01.jpg", width = 480*1.5, height = 480)
par(las=1)
with(xdat, plot(DailyCases~ id, type="l", col="white", axes = FALSE, ylab="", xlab="Sequential day"))
for (i in 1:nrow(xdat)){
 segments(i, 0, i, xdat$DailyCases[i]) 
}
axis(1, seq(0, 210, 10))
axis(2, seq(0, 6000, 500))
dev.off()
################################################################################
jpeg(file="./fig/fig02.jpg", width = 480*1.5, height = 480,)
par(las=1)
with(xdat, plot(DailyCases~ id, type="l", col="white", axes = FALSE, ylab="", xlab="Sequential day"))
for (i in 1:nrow(xdat)){
 segments(i, 0, i, xdat$DailyCases[i]) 
}
with(xdat, lines(DailyCases~ id, type="l", col="blue"))
axis(1, seq(0, 210, 10))
axis(2, seq(0, 6000, 500))
dev.off()
################################################################################
jpeg(file="./fig/fig03.jpg", width = 480*1.5, height = 480)
par(las=1)
with(xdat, plot(DailyCases~ id, type="l", col="blue", axes = FALSE, ylab="", xlab="Sequential day"))
axis(1, seq(0, 210, 10))
axis(2, seq(0, 6000, 500))
dev.off()
################################################################################
x <- read.csv( curl("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
# selecting Spain
s <- subset(x, Country.Region=="Spain")
dat <- data.frame(t(s))
names(dat) <- "CumDeaths"
dat$date <- as.Date(sub("X", "", rownames(dat)), format="%m.%d.%y")
dat <- dat[!is.na(dat$date), ]

# approx 250000

xx <- curve(dnorm(x,mean=0,sd=1), xlim=c(-3,3))
dat1 <- data.frame(id=seq(1, length(xx$y)), dens= xx$y)
dat1$dens1 <- dat1$dens/sum(dat1$dens)
dat1$cases1 <- round(250000* dat1$dens1)



yy <- c(curve(dnorm(x,mean=0,sd=2), xlim=c(-5,0))$y, curve(dnorm(x,mean=0,sd=2), xlim=c(0,5))$y)
dat2 <- data.frame(id=seq(1, length(yy)), dens= yy)
dat2$dens2 <- dat2$dens/sum(dat2$dens)
dat2$cases2 <- round(250000* dat2$dens2)
jpeg(file="./fig/fig04.jpg", width = 480*1.5, height = 480)
par(las=1)
with(dat1, plot(cases1~id, type ="l", xlim=c(1, 210), axes=FALSE, xlab= "Sequential day", 
     col="blue", main = "Daily Cases (up to 250000)", lty=2, ylab=""))
with(dat2, lines(cases2~id, type ="l", col = "slateblue4", lty=2))
axis(1, seq(0, 210, 10))
axis(2, seq(0, 6000, 500))
dev.off()
################################################################################
jpeg(file="./fig/fig05.jpg", width = 480*1.5, height = 480)
par(las=1)
with(dat1, plot(cases1~id, type ="l", xlim=c(1, 210), axes=FALSE, xlab= "Sequential day", 
     col="blue", main = "Daily Cases (up to 250000)", lty=2, ylab=""))
with(dat2, lines(cases2~id, type ="l", col = "slateblue4", lty=2))
axis(1, seq(0, 210, 10))
axis(2, seq(0, 6000, 500))
abline(h=2920, lty =2, col ="orange", lwd=2)
temp2 <- subset(dat1, cases1>=2920)
polygon(temp2$id, temp2$cases1,col="orange", border= 1)
dev.off()
################################################################################
temp <- subset(dat1, cases1>=4000)
jpeg(file="./fig/fig06.jpg", width = 480*1.5, height = 480)
par(las=1)
with(dat1, plot(cases1~id, type ="l", xlim=c(1, 210), axes=FALSE, xlab= "Sequential day", 
     col="blue", main = "Daily Cases (up to 250000)", lty=2, ylab=""))
with(dat2, lines(cases2~id, type ="l", col = "slateblue4", lty=2))
axis(1, seq(0, 210, 10))
axis(2, seq(0, 6000, 500))
abline(h=2920, lty =2, col ="orange", lwd=2)
temp2 <- subset(dat1, cases1>=2920)
polygon(temp2$id, temp2$cases1,col="orange", border= 1)
abline(h=4000, lty =2, col ="red", lwd=2)
polygon(temp$id, temp$cases1,col="lightpink", border= 1)
text(mean(dat1$id), 4500, sum(temp$cases1-4000))
text(mean(dat1$id), 4200, paste(round(sum(temp$cases1-4000)/250000*100, 1), "%", sep=""))
dev.off()
################################################################################
startdate <- "2020-02-20"
# reading raw data
x <- read.csv( curl("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
# selecting Spain
s <- subset(x, Country.Region=="Spain")
# making data frame
dat <- data.frame(t(s))
names(dat) <- "CumDeaths"
#  making date
dat$date <- as.Date(sub("X", "", rownames(dat)), format="%m.%d.%y")
# removing Lattitude and others (i.e not a date)
dat <- dat[!is.na(dat$date), ]
# change factor to numeric
dat$CumDeaths <- as.numeric(as.character(dat$CumDeaths))
# subseting since 1st date
dat <- subset(dat, date >=startdate)

# Daily deaths
dat$DailyDeaths <- with(dat, CumDeaths-c(0, dat$CumDeaths[1:nrow(dat)-1]))

# WeekDay
dat$weekday <- weekdays(dat$date)

# png(filename = "/Users/jvila/xxx/SpainDeaths.png", width = 480*2, height = 480)
# par(las=1, mar= c(5, 3, 4, 2) + 0.1, mfrow= c(1, 2))
# with(dat, plot(date, CumDeaths, type ="l", axes = FALSE, ylab="", main = "Accumulated Deaths"))
# axis(2,dat$CumDeaths,  cex.axis= 0.7)
# axis(1,dat$date, sub("2020-", "", dat$date), cex.axis=0.7)
# 
# with(dat, plot(date, DailyDeaths, type ="l", axes = FALSE, ylab="", main = "Daily Deaths"))
# axis(2,dat$DailyDeaths,  cex.axis= 0.7)
# axis(1,dat$date, sub("2020-", "", dat$date), cex.axis=0.7)
# abline(v=subset(dat, weekday=="lunes")$date, lty =2 ,col="red")
# 
# dev.off()
# getwd()
germanPob <- 83019200
spainPob <- 47007367
francePob <- 67076000
ukPob <- 67886004
italyPob <- 60317116  
usPob <- 325719178
  
################################################################################
################################################################################
################################################################################
# selecting Germany
s2 <- subset(x, Country.Region=="Germany")
# making data frame
GE <- data.frame(t(s2))
names(GE) <- "CumDeathsGE"
#  making date
GE$date <- as.Date(sub("X", "", rownames(GE)), format="%m.%d.%y")
# removing Lattitude and others (i.e not a date)
GE <- GE[!is.na(GE$date), ]
# change factor to numeric
GE$CumDeathsGE <- as.numeric(as.character(GE$CumDeathsGE))
# subseting since 1st date
GE <- subset(GE, date >=startdate)
# Daily deaths
GE$DailyDeathsGE <- with(GE, CumDeathsGE-c(0, GE$CumDeathsGE[1:nrow(GE)-1]))

# Ajusted Daily death
GE$DailyAdjGE <- round(GE$DailyDeathsGE* spainPob/germanPob)

################################################################################
################################################################################
################################################################################
# selecting France
s3 <- subset(x, Country.Region=="France" & Province.State=="")
# making data frame
FR <- data.frame(t(s3))
names(FR) <- "CumDeathsFR"
#  making date
FR$date <- as.Date(sub("X", "", rownames(FR)), format="%m.%d.%y")
# removing Lattitude and others (i.e not a date)
FR <- FR[!is.na(FR$date), ]
# change factor to numeric
FR$CumDeathsFR <- as.numeric(as.character(FR$CumDeathsFR))
# subseting since 1st date
FR <- subset(FR, date >=startdate)
# Daily deaths
FR$DailyDeathsFR <- with(FR, CumDeathsFR-c(0, FR$CumDeathsFR[1:nrow(FR)-1]))

# Ajusted Daily death
FR$DailyAdjFR <- round(FR$DailyDeathsFR* spainPob/francePob)


################################################################################
################################################################################
################################################################################
# selecting UK
s4 <- subset(x, Country.Region=="United Kingdom" & Province.State=="")
# making data frame
UK <- data.frame(t(s4))
names(UK) <- "CumDeathsUK"
#  making date
UK$date <- as.Date(sub("X", "", rownames(UK)), format="%m.%d.%y")
# removing Lattitude and others (i.e not a date)
UK <- UK[!is.na(UK$date), ]
# change factor to numeric
UK$CumDeathsUK <- as.numeric(as.character(UK$CumDeathsUK))
# subseting since 1st date
UK <- subset(UK, date >=startdate)
# Daily deaths
UK$DailyDeathsUK <- with(UK, CumDeathsUK-c(0, UK$CumDeathsUK[1:nrow(UK)-1]))

# Ajusted Daily death
UK$DailyAdjUK <- round(UK$DailyDeathsUK* spainPob/ukPob)

################################################################################
################################################################################
################################################################################
# selecting IT
s5 <- subset(x, Country.Region=="Italy" & Province.State=="")
# making data frame
IT <- data.frame(t(s5))
names(IT) <- "CumDeathsIT"
#  making date
IT$date <- as.Date(sub("X", "", rownames(IT)), format="%m.%d.%y")
# removing Lattitude and others (i.e not a date)
IT <- IT[!is.na(IT$date), ]
# change factor to numeric
IT$CumDeathsIT <- as.numeric(as.character(IT$CumDeathsIT))
# subseting since 1st date
IT <- subset(IT, date >=startdate)
# Daily deaths
IT$DailyDeathsIT <- with(IT, CumDeathsIT-c(0, IT$CumDeathsIT[1:nrow(IT)-1]))

# Ajusted Daily death
IT$DailyAdjIT <- round(IT$DailyDeathsIT* spainPob/italyPob)

################################################################################
################################################################################
################################################################################
# selecting US
s6 <- subset(x, Country.Region=="US" & Province.State=="")
# making data frame
US <- data.frame(t(s6))
names(US) <- "CumDeathsUS"
#  making date
US$date <- as.Date(sub("X", "", rownames(US)), format="%m.%d.%y")
# removing Lattitude and others (i.e not a date)
US <- US[!is.na(US$date), ]
# change factor to numeric
US$CumDeathsUS <- as.numeric(as.character(US$CumDeathsUS))
# subseting since 1st date
US <- subset(US, date >=startdate)
# Daily deaths
US$DailyDeathsUS <- with(US, CumDeathsUS-c(0, US$CumDeathsUS[1:nrow(US)-1]))

# Ajusted Daily death
US$DailyAdjUS <- round(US$DailyDeathsUS* spainPob/usPob)

datall <- merge(dat, GE[, c("date", "CumDeathsGE", "DailyDeathsGE", "DailyAdjGE")], by="date", all.x = TRUE)
datall <- merge(datall, FR[, c("date", "CumDeathsFR", "DailyDeathsFR", "DailyAdjFR")], by="date", all.x = TRUE)
datall <- merge(datall, UK[, c("date", "CumDeathsUK", "DailyDeathsUK", "DailyAdjUK")], by="date", all.x = TRUE)
datall <- merge(datall, IT[, c("date", "CumDeathsIT", "DailyDeathsIT", "DailyAdjIT")], by="date", all.x = TRUE)
datall <- merge(datall, US[, c("date", "CumDeathsUS", "DailyDeathsUS", "DailyAdjUS")], by="date", all.x = TRUE)
  
################################################################################
jpeg(file="./fig/fig07.jpg", width = 480*1.5, height = 480)
par(las=1, mar= c(5, 3, 2, 4) + 0.1, mfrow= c(1, 1), xpd = FALSE)
with(datall, plot(date, DailyDeaths, type ="l", axes = FALSE, ylab="", 
                  main = "Daily Deaths (*) according to \n CSSE at Johns Hopkins University", cex.main=0.8,
                  ))
axis(2,datall$DailyDeaths,  cex.axis= 0.7)
axis(1,datall$date, sub("2020-", "", datall$date), cex.axis=0.7)
abline(v= subset(datall, sub("2020-", "", datall$date)=="03-12")$date, col="blue", lty=2)
abline(v= subset(datall, sub("2020-", "", datall$date)=="03-20")$date, col="red", lty=2)
par(xpd =TRUE)
text(max(datall$date), datall$DailyDeaths[nrow(datall)], "SP", pos = 4, col = "black", cex= 0.7)
dev.off()
################################################################################
jpeg(file="./fig/fig08.jpg", width = 480*1.5, height = 480)
par(las=1, mar= c(5, 3, 2, 4) + 0.1, mfrow= c(1, 1), xpd = FALSE)
with(datall, plot(date, DailyDeaths, type ="l", axes = FALSE, ylab="", 
                  main = "Daily Deaths according to \n CSSE at Johns Hopkins University", cex.main=0.8,
                  ))
axis(2,datall$DailyDeaths,  cex.axis= 0.7)
axis(1,datall$date, sub("2020-", "", datall$date), cex.axis=0.7)
with(datall, lines(date, DailyAdjIT, type ="l", lty=1, col="turquoise4"))
par(xpd =TRUE)
text(max(datall$date), datall$DailyDeaths[nrow(datall)], "SP", pos = 4, col = "black", cex= 0.7)
text(max(datall$date), datall$DailyAdjIT[nrow(datall)], "IT", pos = 4, col = "turquoise4", cex= 0.7)
legend(min(datall$date)-1, max(datall$DailyDeaths)*1.12, bty="n", 
       title ="Cumulated deaths", cex = 0.8, title.adj = 0.1, 
       c(paste("SP: ", datall$CumDeaths[nrow(datall)], sep =""),
       paste("IT: ", round(datall$CumDeathsIT[nrow(datall)] * spainPob/italyPob),"(*) / real = ",
             datall$CumDeathsIT[nrow(datall)], sep ="")
       )
       )
par(xpd =FALSE)
text(min(datall$date)+10, 900, 
     paste("(*) expected if the country had ", spainPob, " inhabitants", sep=""), cex=0.7)
dev.off()
################################################################################
jpeg(file="./fig/fig09.jpg", width = 480*1.5, height = 480)
par(las=1, mar= c(5, 3, 2, 4) + 0.1, mfrow= c(1, 1), xpd = FALSE)
with(datall, plot(date, DailyDeaths, type ="l", axes = FALSE, ylab="", 
                  main = "Daily Deaths (*) according to \n CSSE at Johns Hopkins University", cex.main=0.8,
                  ))
axis(2,datall$DailyDeaths,  cex.axis= 0.7)
axis(1,datall$date, sub("2020-", "", datall$date), cex.axis=0.7)
# abline(v=subset(datall, weekday=="lunes")$date, lty =2 ,col="grey")
with(datall, lines(date, DailyAdjGE, type ="l", lty=1, col="red"))
with(datall, lines(date, DailyAdjFR, type ="l", lty=2, col="blue"))
with(datall, lines(date, DailyAdjUK, type ="l", lty=2, col="orangered3"))
with(datall, lines(date, DailyAdjIT, type ="l", lty=1, col="turquoise4"))
with(datall, lines(date, DailyAdjUS, type ="l", lty=1, col="slateblue4"))
par(xpd =TRUE)
text(max(datall$date), datall$DailyDeaths[nrow(datall)], "SP", pos = 4, col = "black", cex= 0.7)
text(max(datall$date), datall$DailyAdjGE[nrow(datall)], "GE", pos = 4, col = "red", cex= 0.7)
text(max(datall$date), datall$DailyAdjFR[nrow(datall)], "FR", pos = 4, col = "blue", cex= 0.7)
text(max(datall$date), datall$DailyAdjUK[nrow(datall)], "UK", pos = 4, col = "orangered3", cex= 0.7)
text(max(datall$date), datall$DailyAdjIT[nrow(datall)], "IT", pos = 4, col = "turquoise4", cex= 0.7)
text(max(datall$date), datall$DailyAdjUS[nrow(datall)], "US", pos = 4, col = "slateblue4", cex= 0.7)

legend(min(datall$date)-1, max(datall$DailyDeaths)*1.12, bty="n", 
       title ="Cumulated deaths", cex = 0.8, title.adj = 0.1, 
       c(paste("SP: ", datall$CumDeaths[nrow(datall)], sep =""),
       paste("GE: ", round(datall$CumDeathsGE[nrow(datall)] * spainPob/germanPob), "(*) / real = ",
             datall$CumDeathsGE[nrow(datall)], sep =""),
       paste("FR: ", round(datall$CumDeathsFR[nrow(datall)] * spainPob/francePob),"(*) / real = ",
             datall$CumDeathsFR[nrow(datall)], sep =""),
       paste("UK: ", round(datall$CumDeathsUK[nrow(datall)] * spainPob/ukPob), "(*) / real = ",
             datall$CumDeathsUK[nrow(datall)], sep =""),
       paste("IT: ", round(datall$CumDeathsIT[nrow(datall)] * spainPob/italyPob),"(*) / real = ",
             datall$CumDeathsIT[nrow(datall)], sep =""), 
       paste("US: ", round(datall$CumDeathsUS[nrow(datall)] * spainPob/usPob),"(*) / real = ",
             datall$CumDeathsUS[nrow(datall)], sep ="")
       )
       )
par(xpd =FALSE)
text(min(datall$date)+10, 800, 
     paste("(*) expected if the country had ", spainPob, " inhabitants", sep=""), cex=0.7)
dev.off()

################################################################################
jpeg(file="./fig/fig10.jpg", width = 480*1.5, height = 480)
CAT <- read.csv("./dat/t13259CAT.csv", header = TRUE, sep = ";")
SP <- datall[, c("date", "CumDeaths",  "DailyDeaths")]
CAT$date <- as.Date(as.character(CAT$date), format="%d/%m/%y")
CAT <- rename.vars(CAT, c("Total.defuncions", "Defuncions.diàries"), c("CumDeathsCAT", "DailyDeathsCAT"))
CAT <- CAT[, c("date", "CumDeathsCAT", "DailyDeathsCAT")]
CAT$date[CAT$date%nin%SP$date]
SP$date[SP$date%nin%CAT$date]
SP <- merge(SP, CAT, by="date", all.x =TRUE)
SP$CumDeathsCAT <- ifelse(is.na(SP$CumDeathsCAT), 0, SP$CumDeathsCAT)
SP$DailyDeathsCAT <- ifelse(is.na(SP$DailyDeathsCAT), 0, SP$DailyDeathsCAT)
SP$DailyDeathsNoCAT <- with(SP, DailyDeaths-DailyDeathsCAT)
catPob <- 7619494
SP$DailyAdjNOCAT <- round(SP$DailyDeathsNoCAT*catPob/spainPob)
SP$id <- seq(1, nrow(SP))
SP$CumDeathsNOCAT <- with(SP, CumDeaths - CumDeathsCAT)
SP <- subset(SP, CumDeaths>0)



par(las=1, mar= c(5, 3, 2, 4) + 0.1, mfrow= c(1, 1), xpd = FALSE)
with(SP, plot(date, DailyDeathsCAT, type ="l", axes = FALSE, ylab="", 
                  main = "Daily Deaths (*) according to \n CSSE at Johns Hopkins University & IDESCAT", 
              cex.main=0.8, ylim=c(0, max(DailyDeathsCAT))
                  ))
axis(2,SP$DailyDeathsCAT,  cex.axis= 0.7)
axis(1,SP$date, sub("2020-", "", SP$date), cex.axis=0.7)
abline(v= subset(SP, sub("2020-", "", SP$date)=="03-12")$date, col="blue", lty=2)
abline(v= subset(SP, sub("2020-", "", SP$date)=="03-20")$date, col="red", lty=2)
with(SP, lines(date, DailyAdjNOCAT, type ="l", lty=1, col="turquoise4"))
par(xpd =TRUE)
text(max(SP$date), SP$DailyDeathsCAT[nrow(SP)], "CAT", pos = 4, col = "black", cex= 0.7)
text(max(SP$date), SP$DailyAdjNOCAT[nrow(SP)], "No-CAT", pos = 4, col = "turquoise4", cex= 0.7)

legend(min(SP$date)-1, max(SP$DailyDeathsCAT)*1.12, bty="n", 
       title ="Cummulated deaths", cex = 0.8, title.adj = 0.1, 
       c(paste("CAT: ", SP$CumDeathsCAT[nrow(SP)], sep =""),
       paste("No-CAT: ", round(SP$CumDeathsNOCAT[nrow(SP)] * catPob/spainPob), "(*) / real = ",
             SP$CumDeathsNOCAT[nrow(SP)], sep ="")
       )
       )
par(xpd =FALSE)
text(min(SP$date)+6, 250, 
     paste("(*) expected if the country had ", catPob, " inhabitants", sep=""), cex=0.7)
dev.off()

