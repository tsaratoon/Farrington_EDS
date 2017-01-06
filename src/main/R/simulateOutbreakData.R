# Script to simulate outbreak data for the EDS using a negative binomial
# model with dispersion parameter >= 1
# (Note that a Poission distribution if dispersion parameter == 1)

# Follows the method outlined in
# Noufaily et al., Statist. Med. 2013 (32) 1206-1222

# Author:    Teedah Saratoon
# Date:      18/02/2015
# Last edit: 02/03/2015


# Inputs ------------------------------------------------------------------
#
# nYears         No. of years for which to simulate data
# timeUnits      Intervals at which to simulate data ("weekly" or "monthly")
#
# alpha        
# beta           Linear trend
# m              Seasonality (0 = none, 1 = annual, 2 = biannual)
# gamma_1        Controls magnitude of peaks
# gamma_2        Controls magnitude of troughs
# dispersion     Dispersion parameter
#
# kRange         Range of values of magnitude of outbreak


# Outputs -----------------------------------------------------------------
#
# dataBaseline   Simulated baseline data
# dataOutbreak   Simulated outbreak data
# idxOutbreak    Indices of outbreak months
# outbreakHist   Outbreak counts


# User-defined parameters -------------------------------------------------

# Number of years for which to simulate data
nYears <- 38.5

# Final year for which to simulate data
endYear <- 2014

# Choose short or long outbreaks (3 units or 6 units of time)
# outbreakLength <- "short"
outbreakLength <- "long"

# outbreakShape <- "logNormal"
outbreakShape <- "epidemicCurve"

# Define end of each period
#Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak
endBaseline <- 146
endPreOutbreak <- 182
endOutbreak <- 282

# Set of parameters to control baseline data
alpha <- 1.5
beta <- 0
m <- 1
gamma_1 <- 0.2
gamma_2 <- -0.4
dispersion <- 1

# Set of parameters to control outbreak-related data
k <- 10

# Simulate baseline data --------------------------------------------------------------

source("generateOutbreakData.R")
source("epidemicCurveOutbreak.R")

# Total no. of time units in simulations
nData <- nYears*12

# Preallocate space for data:
year <- rep(0, nData)
month <- rep(0, nData)

startYear = round(endYear - (nData/12))
for (i in 1:nData)
{
  year[i] = startYear + ((i-1) %/% 12)
  month[i] = (i-1) %% 12 + 1
}

# Simulate outbreak data --------------------------------------------------

if (outbreakShape == "logNormal") {
  data = generateOutbreakData(
    nData, outbreakLength, endPreOutbreak, endOutbreak,
    alpha, beta, m, gamma_1, gamma_2, dispersion, k )
} else {
  data = epidemicCurveOutbreak(
    nData, outbreakLength, endPreOutbreak, endOutbreak,
    alpha, beta, m, gamma_1, gamma_2, dispersion, k )
}

dataBaseline = data$baseline
dataOutbreak = data$outbreak
idxOutbreak  = data$idx
outbreakHist = data$hist

# Visualisation -----------------------------------------------------------

# Output to pdf (A7 landscape paper)
#pdf("simulatedOutbreakData.pdf", width=4.13, height=2.91)

cmin = min(c(dataBaseline,dataOutbreak))
cmax = max(c(dataBaseline,dataOutbreak))

plot(1:nData,dataBaseline,"l",
     ylim = c(cmin,cmax),
     main = "Simulated baseline data",
     xlab = "Months",
     ylab = "No. of cases")

plot(1:nData,dataOutbreak,"l",
     ylim = c(cmin,cmax),
     main = "Simulated outbreak data",
     xlab = "Months",
     ylab = "No. of cases")

barplot(outbreakHist,
        names.arg=as.character(idxOutbreak),
        main = "Outbreak",
        xlab = "Months",
        ylab = "No. of cases")

#dev.off()