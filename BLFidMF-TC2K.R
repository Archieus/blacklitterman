library(quantmod)
library(BLCOP)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(fPortfolio)

#### reads/loads text into R and converts to XTS ####
BLbenchtxt <- read.table('blbench', header = FALSE, sep = ',')
Blbenchzoo <- read.zoo(BLbenchtxt, sep = ",", format = "%m/%d/%Y", split = 3)
BLBench <- na.omit(as.xts(Blbenchzoo))

ComponentRet <- as.data.frame(lapply(BLBench, function(x) monthlyReturn(x)))
colnames(ComponentRet) <- colnames(BLBench)

CompRet.x <- as.xts(ComponentRet)

#### Generate Monthly Benchmark Returns 60% ACWI 40% AGG ####
BenchmarkRet <- Return.portfolio(CompRet.x, weights = c(.60,.40))

#### BEGIN IMPORTING DATA DOWNLOADED FROM TC2000 WEB APPLICATION ####
filelist = list.files(pattern = ".*.txt") #Create a list of txt files to be used in batch conversion
Symnames <- gsub(".txt", ".z", filelist) #Remove .txt extension with .z for zoo

#NOTE: tab separated values with a header "\t"    
datalist = lapply(filelist, function(x)read.table(x, header=TRUE, sep = ","))

####Convert datalist components into separate zoo objects####
zooEnv <- new.env()

for(i in 1:length(filelist)) {
  assign(Symnames[i], read.zoo(datalist[[i]], sep = ',', format = "%m/%d/%Y"), envir = zooEnv)
}

zoolist <- t(as.data.frame(Symnames)) #Used to read symbols into loop

###Extract Close data from zoo objects###
qmodEnv <- new.env()

for(i in 1:length(zoolist)) {
  assign(zoolist[i], head(cbind(get(zoolist[,i], envir = zooEnv)$Close), 500), envir = qmodEnv)
}

#### Create a Matrix of Close Values ####
ClPx <- matrix(0, ncol = ncol(zoolist), nrow = 500)
for(i in 1:length(zoolist)){
  ClPx[,i] <- cbind(get(zoolist[,i], envir = qmodEnv))
}

XTSNames <- gsub(".z", "", zoolist) # Generate column names for xts object

Dates <- index(qmodEnv$bpavx.z) #Extract Zoo index for use in xts object

ClPx.df <- as.data.frame(ClPx)
colnames(ClPx.df) <- XTSNames
row.names(ClPx.df) <- Dates
#### Create the XTS Object to be used for analysis ####
Close.zoo <- as.zoo(ClPx.df)
Closedata <- as.xts(Close.zoo)

############################################################################

####BEGIN BLACK LITTERMAN MODEL###
FIRet <- as.data.frame(lapply(Closedata, function(x) monthlyReturn(x)))
colnames(FIRet) <- colnames(Closedata)
FIRet.x <- as.xts(FIRet)

###Construct Equilibrium CAPM Portfolio###
###Generate Weights Using beta, beta of the assets relative to the Benchmark###
FICAPMbeta <- CAPM.beta(FIRet.x, BenchmarkRet, .03/12) #Portfolio Weights
#Apply Weigths to Returns of Individual Assets
FIEqLib <- Return.portfolio(FIRet.x, weights = as.numeric(FICAPMbeta))
#Calculate Excess Returns over Eq Lib Port
FIExcRet <- Return.excess(FIRet.x, FIEqLib)
#Create Cov Matrix of Excess Returns
FICovMat <- cov.mve(FIExcRet)$cov
nameList <- names(FIRet.x)
colnames(FICovMat) <- nameList

##myPosterior Data##
priorMeans <- rep(0,ncol(FICovMat)) #set means to 0 for the respective number of assets
#priorVarcov <- cov.mve(FIExcRet)$cov

###Create a "pick" matrix.  Connects assets with a specific "view (limited to three (3) Views)"
Pick <- matrix(0, ncol = ncol(FICovMat), nrow = 6, dimnames = list(NULL,as.list(colnames(FICovMat))))

###Create a Vector Q which contains information on the Excess Return for the corresponding "view"
QVect <- c(.0,.0,.0,.0,.0,.0) #EXCESS RETURNS

##Fill Matrix with Picks on over and underperformance (Relative or Absolute)###
##Relative views in a row must net to zero (0).
##Absolute view in a row must add up to one (1).
##Fill row1, col7 ==> pick[1,7]

Pick[1,1] <- -1
Pick[1,2] <- 1

# tau = scalar = set to as close to zero as possible (in practice) .025?
##Calculate the confidence of the Views (recipricols of the Variances of View Portfolios * tau)

ViewConf <- c(.01,.01,.01,.01,.01,.01) #Between .01(No confidence) and 100(High confidence)

Views <- BLViews(Pick, QVect, confidences = ViewConf, assetNames = colnames(FICovMat))

# #### Generate Posterior Expected Values (Mu) and Posterior Covariance Matrix (Sigma) ####
# AvgRet <- t(colMeans(FIRet[,c(1:10,12)])) # R = Returns
# Q.Vector <- t(QVect) #P Excess Returns
# AvgER <- as.numeric(t(colMeans(FIExcRet))) # Mu
# CovMat <- matrix(FICovMat, nrow = 11, ncol = 11) #Covariance Matrix
# 
# PABLPost <- black.litterman(R = AvgRet, P = Q.Vector, Mu = AvgER, Sigma = CovMat, Views = ViewConf)
# AvgMu <- PABLPost$BLMu[,1]
# PostSigma <- matrix(PABLPost$BLSigma, nrow = 11, ncol = 11)

###Generate "posterior" estimates using "prior" inputs and Investors Views and confidences
ExRetPosterior <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = FICovMat)

# ### Estimate generated using posterion results from PortfolioAnalytics blacklitterman function
# AltPosterior <- posteriorEst(Views, priorMeans, tau = .025, PostSigma)
optimalPortfolios.fPort(ExRetPosterior, spec = NULL, constraints = "LongOnly", 
                        optimizer = "minriskPortfolio", inputData = NULL, 
                        numSimulations = 100)

optimalPortfolios(ExRetPosterior)
# optimalPortfolios(AltPosterior)
