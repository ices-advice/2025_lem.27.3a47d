### ------------------------------------------------------------------------ ###
### Prepare plots and tables for report ####
### ------------------------------------------------------------------------ ###


## Before: bootstrap/data/StockOverview.txt
##         bootstrap/data/NumbersAtAgeLength.txt
##         bootstrap/data/StockOverview_prior.txt
##         bootstrap/data/NumbersAtAgeLength_prior.txt
##         output/advice.Rdata
## After:  report/InterCatch LandPercent_2024.tiff
##         report/InterCatch DiscProvided_2024.tiff
##         report/InterCatch DisWt_2024.tiff
##         report/InterCatch LandWt_2024.tiff
##         

source("utilities_report.R") 

library(TAF)
library(cat3advice)
library(ggplot2)



################################## READ IN INTERCATCH DATA ###############################

### ------------------------------------------------------------------------ ###
### Intercatch data ####
### ------------------------------------------------------------------------ ###

StockOverviewFile <- "bootstrap\\data\\StockOverview.txt"
NumbersAtAgeLengthFile <- "bootstrap\\data\\NumbersAtAgeLength.txt"
WtData <- readStockOverview(StockOverviewFile,NumbersAtAgeLengthFile) 

stock <- "Lemon sole"

################################## PLOT LANDINGS DATA ####################################


windows(height = 7, width = 20)
par(mfrow = c(1,1))
plotStockOverview(WtData,plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE)
savePlot(filename = "report/InterCatch LandWt_2024",type = "tiff")
dev.off()

windows(height = 7, width = 20)
par(mfrow = c(1,1))
plotStockOverview(WtData,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE)
savePlot(filename = "report/InterCatch LandPercent_2024",type = "tiff")
dev.off()

################################## PLOT DISCARD DATA ####################################

windows(height = 7, width = 20)
par(mfrow = c(1,1))
plotStockOverview(WtData,plotType="DisWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE)
savePlot(filename = "report/InterCatch DisWt_2024",type = "tiff")
dev.off()

################################## READ IN INTERCATCH DATA 2 ###############################

# Alternative Intercatch data required for final DiscProvided plot

StockOverviewFile <- "bootstrap/data/StockOverview_prior.txt"
NumbersAtAgeLengthFile <- "bootstrap/data/NumbersAtAgeLength_prior.txt"
WtData <- readStockOverview(StockOverviewFile,NumbersAtAgeLengthFile) 
stock <- "Lemon sole"

 windows(height = 7, width = 20)
par(mfrow = c(1,1))
plotStockOverview(WtData,plotType="DiscProvided",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE)
savePlot(filename = "report/InterCatch DiscProvided_2024",type = "tiff")

dev.off()


### ------------------------------------------------------------------------ ###
### Stock weights at age plot ####
### ------------------------------------------------------------------------ ###


x <- read.csv("data/Weight Estimation.csv", header = TRUE)
x[x$survey == "q1",]$year <- x[x$survey == "q1",]$year + 0.25
x[x$survey == "q3",]$year <- x[x$survey == "q3",]$year + 0.75

# Fit and plot age-based loess smoothers
fitwt <- data.frame(array(dim = c(21, 9))) #change dim 1
rownames(fitwt) <- 2005:2025               #extend by one year
colnames(fitwt) <- 1:9



windows(width = 10, height = 7)
par(mfrow = c(3,3), mar = c(3,4,2,1))
for (i in 1:9)
{	if(i<4)maxW<-0.3 else maxW<-0.8
  
  
  plot(x$year, x$wt, pch = 16, xlab = "", ylab = "Weight (kg)",
       type = "n", ylim = c(0, maxW), xlim = c(2004, 2026))   # extend by one year
  x.i <- x[x$age == i,]
  
  # Add mean of first two non-zero estimates as additional point at the start (enabling extrapolation)
  # Extend backwards to 2005 to match index data
  x.iq <- x.i[!is.na(x.i$wt),]
  x.iq <- x.iq[order(x.iq$year),]
  x.iq.mean1 <- mean(x.iq[1:2,]$wt)
  x.temp <- data.frame(survey = "q0", year = min(x.i$year) - rev(c(0.5,1.5,2.5)), age = x.i$age[1], 
                       wt = x.iq.mean1)
  x.i <- rbind(x.temp, x.i)
  # Repeat for the last two estimates
  x.iq.len <- dim(x.iq)[1]
  x.iq.mean2 <- mean(x.iq[(x.iq.len-1):x.iq.len,]$wt)
  x.temp <- data.frame(survey = "q0", year = max(x.i$year) + 0.5, age = x.i$age[1], 
                       wt = x.iq.mean2)
  x.i <- rbind(x.i, x.temp)
  
  x.lm <- loess(x.i$wt ~ x.i$year, span = 1)
  x.new <- seq(min(x.i$year), max(x.i$year), length = 100)
  x.fit <- predict(x.lm, new = x.new, se = TRUE)
  polygon(x = c(x.new, rev(x.new)), y = c(x.fit$fit + (2 * x.fit$se.fit), rev(x.fit$fit - (2 * x.fit$se.fit))), 
          density = -1, col = "lightgrey", border = NA)
  lines(x = x.new, y = x.fit$fit, lwd = 2, col = "black")
  points(x.i$year, x.i$wt, pch = 16, col = "red")
  points(x.i$year[1:3], x.i$wt[1:3], pch = 16, col = "blue")
  points(x.i$year[length(x.i$year)], x.i$wt[length(x.i$year)], pch = 16, col = "blue")
  legend(x = "topright", legend = paste0("Age = ",i), bty = "n")
  fitwt[,i] <- predict(x.lm, new = 2005:2023, se = FALSE)  # extend by one year
}	

savePlot(filename = "report/smoothed stock mean weight at age",type = "tiff")

dev.off()

write.taf(fitwt, file="Smoothed mean stock weights at age.csv", dir="report")






### ------------------------------------------------------------------------ ###
### load advice ####
### ------------------------------------------------------------------------ ###
load(file = "output/advice.Rdata")

### I - biomass index trend/ratio 
plot(advice@I)
ggsave("report/chr_index.png", width = 10, height = 6, units = "cm",
       dpi = 300, type = "cairo")

### b - biomass safeguard
plot(advice@b)
ggsave("report/chr_b.png", width = 10, height = 6, units = "cm",
       dpi = 300, type = "cairo")



### f - annual length distributions
plot(lmean)
ggsave("report/chr_Lmean.png", width = 15, height = 10, units = "cm",
       dpi = 300, type = "cairo")

### f - length indicator
resf<-data.frame(as.numeric(as.character(fi@indicator$year)), fi@indicator$indicator)

png("report/chr_f.png", pointsize=5,units="cm", width=6, height=6, res = 300)
graphics::plot(resf[,1], resf[,2], type="l", ylim=c(0,1.5), xlab="Year", ylab="F indicator")
abline(h=1, col="red")
dev.off()

### ------------------------------------------------------------------------ ###
### chr rule - advice table ####
### ------------------------------------------------------------------------ ###
### create ICES advice style table
### numbers are rounded following ICES rounding rules


### print to screen
advice(advice)
advice
### save in file
capture.output(advice(advice), advice, file = "report/advice_table.txt")
