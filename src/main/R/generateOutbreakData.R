# Function simulates outbreak data for the EDS using a negative binomial
# model with dispersion parameter >= 1
# (Note that a Poission distribution if dispersion parameter == 1)

# Follows the method outlined in
# Noufaily et al., Statist. Med. 2013 (32) 1206-1222

# Author:    Teedah Saratoon
# Date:      16/02/2015
# Last edit: 02/03/2015


# Inputs ------------------------------------------------------------------
#
# nData           No. of months for which to simulate data
# outbreakLength  Choose "short" or "long" outbreak
# endPreOutbreak  End of pre-outbreak period (month number)
# endOutbreak     End of outbreak period (month number)
#
# alpha        
# beta            Linear trend
# m               Seasonality (0 = none, 1 = annual, 2 = biannual)
# gamma_1         Controls magnitude of peaks
# gamma_2         Controls magnitude of troughs
# dispersion      Dispersion parameter
#
# k               Magnitude of outbreak


# Outputs -----------------------------------------------------------------
#
# baseline        Simulated baseline data
# outbreak        Simulated data with outbreak
# idx             Indices of outbreak months
# hist            Outbreak counts

generateOutbreakData <- function(
  nData = 462,
  outbreakLength = "long",
  endPreOutbreak = 182,
  endOutbreak = 282,
  alpha = 1.5,
  beta = 0,
  m = 1,
  gamma_1 = 0.2,
  gamma_2 = -0.4,
  dispersion = 1,
  k = 10
) {
    
  # Simulate baseline data --------------------------------------------------------------
  
  # Preallocate space for data:
  baseline <- rep(0, nData)
  
  # Choose number of bins for log normal data
  if (outbreakLength == "short") {
    nShort = 10
  } else {
    nLong = 20
  }
  
  # Calculate the summation term in expression for mean:
  sum_term <- 0
  for (j in 1:m)
  {
    sum_term <- sum_term + gamma_1*cos(2*pi*j*nYears) + gamma_2*sin(2*pi*j*nYears)
  }
  
  # Calculate mean number of cases
  mean_baseline = exp(alpha + beta*nData + sum_term)
  
  # If dispersion == 1, sample from Poisson distribution
  # else use negative binomial
  if (dispersion == 1) {
    baseline <- rpois(nData,mean_baseline)
  } else {
    n <- mean_baseline / (dispersion - 1)
    prob <- 1 - (dispersion^{-1})
    baseline <- rnbinom(nData, n, prob)
  }
  
  # Simulate outbreak data --------------------------------------------------
  
  # Choose random time point in outbreak period at which to simulate outbreaks
  tOutbreak <- sample(endPreOutbreak+1:endOutbreak,1)
  
  # Calculate standard deviation of the baseline count at each tOutbreak
  stDev <- sd(baseline)
  
  # Calculate no. of outbreak cases to simulate
  nCases <- rpois(1,k*stDev)
  while (nCases == 0) { 
    nCases <- rpois(1,k*stDev)
  }
  
  # Outbreak data
  outbreak <- baseline
  
  outbreakShape <- rlnorm(nCases,0,0.5)
  if (outbreakLength == "short") {
    outbreakHist <- hist(floor(2*outbreakShape), right=FALSE, plot=FALSE)$counts
    idxOutbreak <- tOutbreak + c(1:length(outbreakHist)) - 1
  } else {
    outbreakHist <- hist(floor(2*outbreakShape), right=FALSE, plot=FALSE)$counts
    idxOutbreak <- tOutbreak + c(1:length(outbreakHist)) - 1
  }
  for (i in 1:length(outbreakHist)) {
    outbreak[idxOutbreak[i]] <- outbreak[idxOutbreak[i]] + outbreakHist[i]
  }
  
  return(list(
    "baseline" = baseline,
    "outbreak" = outbreak,
    "idx" = idxOutbreak,
    "hist" = outbreakHist)
  )
  
}