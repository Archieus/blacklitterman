library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(quantmod)
library(BLCOP)

#### Close data from TC2000 Convert TXT to XTS ####
ETFtxt <- read.table('etfb', header = FALSE, sep = ",")

ETFzoo <- read.zoo(ETFtxt, sep = ",", format = "%m/%d/%Y", split = 3)
ETFxts <- as.xts(ETFzoo)

ETFRet <- na.omit(Return.calculate(ETFxts))

###Convert to Monthly Data
monthend <- endpoints(ETFRet, on = "months", k = 1)
Ret.mo <- ETFRet[monthend]

CAPMbeta <- CAPM.beta(Ret.mo[,c(1:8,10:21)], Ret.mo$SPY, .03/12)
#Apply Weigths to Returns of Individual Assets
ETFEqLib <- Return.portfolio(Ret.mo[,c(1:8,10:21)], weights = as.numeric(CAPMbeta))
#Calculate Excess Returns over Eq Lib Port
ETFExcRet <- Return.excess(Ret.mo[,c(1:8,10:21)], ETFEqLib)
#Create Cov Matrix of Excess Returns
CovMat <- cov.mve(ETFExcRet)$cov
nameList <- names(Ret.mo[,c(1:8,10:21)])
colnames(CovMat) <- nameList

##myPosterior Data##
priorMeans <- rep(0,20) #set means to 0 for the twelve assets

###Create a "pick" matrix.  Connects assets with a specific "view"
Pick <- matrix(0, ncol = ncol(CovMat), nrow = 6, dimnames = list(NULL,as.list(colnames(CovMat))))

###Create a Vector Q which contains information on the Excess Return for the corresponding "view"
QVect <- c(0,0,0,0,0,0)

##Fill Matrix with Picks on over and underperformance (Relative or Absolute)###
##Relative views in a row must net to zero (0).
##Absolute view in a row must add upt to one (1).
##Fill row1, col7 ==> pick[1,7]

Pick[1,1] <- 1
Pick[1,11] <- -1

# tau = scalar = set to as close to zero as possible (in practice) .025?
##Calculate the confidence of the Views (recipricols of the Variances of View Portfolios * tau)

ViewConf <- c(.01,.01,.01,.01,.01,.01) #Between .01(No confidence) and 100(High confidence)

Views <- BLViews(Pick, QVect, confidences = ViewConf, assetNames = colnames(CovMat))

###Generate "posterior" estimates using "prior" inputs and Investors Views and confidences
CAPMPosterior <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = CovMat)

optimalPortfolios(CAPMPosterior)

#optimalPortfolios.fPort(myPosterior, constraints = 'maxW[1:12] = .2',optimizer = "minriskPortfolio", numSimulations = 100)
