library(quantmod)
library(BLCOP)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(fPortfolio)

#alphavantage API = Y474

#### Read CSV for Symbols List ###
symblist <- read.csv("schfixedmf.csv", header = FALSE)
symbol <- t(symblist)

####Download MONTHLY ADJUSTED PRICE DATA from AlphaVantage
###outputsize=c(full,compact) full= 20 years of data, compact = 100 datapoints
#### Monthly Adjusted Data = adjusted.close, Daily Adjusted Data = adjusted_close

#### CREATE COMPONENTS FOR API CALL ####
apikey <- "&outputsize=full&apikey=Y474&datatype=csv"
URLbase <- "http://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY_ADJUSTED&symbol="

cu <-NULL
ru <-NULL

### Loop to download dats for all symbols in list as it's own object ###
dataEnv <- new.env()

for(i in 1:length(symbol)){
  cu[i] <- paste0(URLbase, symbol[i])
  ru[i] <- paste0(cu[i],apikey)
  assign(paste(symbol[i]), read.csv(ru[i]), env = dataEnv)
}

### IDENTIFY THE LEAST SYMBOL WITH THE LEAST NUMBER OF ROWS IN ENVIRONMENT ###
RowCount <- matrix(0, ncol = ncol(symbol), nrow = 1)
for(i in 1:length(symbol)) {
  RowCount[,i] <- cbind(nrow(get(symbol[,i], envir = dataEnv)))
}

RowMin <- min(RowCount)

#### SET NEW ENVIRONMENT FOR ADJUSTED CLOSE DATA ####
qmodEnv <- new.env()

for(i in 1:length(symbol)) {
  assign(symbol[i], head(cbind(get(symbol[,i], envir = dataEnv)$adjusted.close), RowMin), envir = qmodEnv)
}

#### Create a Matrix of Adjusted Close Values ####
AC <- matrix(0, ncol = ncol(symbol), nrow = RowMin)
for(i in 1:length(symbol)){
  AC[,i] <- cbind(get(symbol[,i], envir = qmodEnv))
}


Dates <- get(symbol[1], envir = dataEnv)$timestamp
df <- matrix(unlist(Dates))

AC.df <- as.data.frame(AC)
colnames(AC.df) <- symbol
row.names(AC.df) <- head(df,RowMin)

#### Create the XTS Object to be used for analysis ####
AC.zoo <- as.zoo(AC.df)
MthlyClose <- as.xts(AC.zoo)

#write.csv(AC.df, file = "AdjClData.csv", row.names = TRUE)

############################################################################

####BEGIN BLACK LITTERMAN MODEL###
FIRet <- na.omit(Return.calculate(MthlyClose))

###Construct Equilibrium CAPM Portfolio###
###Generate Weights Using beta, beta of the assets relative to the Benchmark###
FICAPMbeta <- CAPM.beta(FIRet[,c(2:4)], FIRet$AGG, .03/12) #Portfolio Weights
#Apply Weigths to Returns of Individual Assets
FIEqLib <- Return.portfolio(FIRet[,c(2:4)], weights = as.numeric(FICAPMbeta))
#Calculate Excess Returns over Eq Lib Port
FIExcRet <- Return.excess(FIRet[,c(2:4)], FIEqLib)
#Create Cov Matrix of Excess Returns
FICovMat <- cov.mve(FIExcRet)$cov
nameList <- names(FIRet[,2:4])
colnames(FICovMat) <- nameList

##myPosterior Data##
priorMeans <- rep(0,3) #set means to 0 for the twelve assets
#priorVarcov <- cov.mve(FIExcRet)$cov

###Create a "pick" matrix.  Connects assets with a specific "view (limited to three (3) Views)"
Pick <- matrix(0, ncol = ncol(FICovMat), nrow = 3, dimnames = list(NULL,as.list(colnames(FICovMat))))

###Create a Vector Q which contains information on the Excess Return for the corresponding "view"
QVect <- c(.05,.05,.05) #EXCESS RETURNS

##Fill Matrix with Picks on over and underperformance (Relative or Absolute)###
##Relative views in a row must net to zero (0).
##Absolute view in a row must add up to one (1).
##Fill row1, col7 ==> pick[1,7]

Pick[1,1] <- -1
Pick[1,2] <- 1

# tau = scalar = set to as close to zero as possible (in practice) .025?
##Calculate the confidence of the Views (recipricols of the Variances of View Portfolios * tau)

ViewConf <- c(.01,.01,.01) #Between .01(No confidence) and 100(High confidence)

Views <- BLViews(Pick, QVect, confidences = ViewConf, assetNames = colnames(FICovMat))

#### Generate Posterior Expected Values (Mu) and Posterior Covariance Matrix (Sigma) ####
AvgRet <- t(colMeans(FIRet[,2:4])) # R = Returns
Q.Vector <- t(QVect) #P Excess Returns
AvgER <- as.numeric(t(colMeans(FIExcRet))) # Mu
CovMat <- matrix(FICovMat, nrow = 3, ncol = 3) #Covariance Matrix

PABLPost <- black.litterman(R = AvgRet, P = Q.Vector, Mu = AvgER, Sigma = CovMat, Views = ViewConf)
AvgMu <- PABLPost$BLMu[,1]
PostSigma <- matrix(PABLPost$BLSigma, nrow = 3, ncol = 3)

###Generate "posterior" estimates using "prior" inputs and Investors Views and confidences
ExRetPosterior <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = FICovMat)

### Estimate generated using posterion results from PortfolioAnalytics blacklitterman function
AltPosterior <- posteriorEst(Views, priorMeans, tau = .025, PostSigma)

optimalPortfolios(ExRetPosterior)
optimalPortfolios(AltPosterior)
