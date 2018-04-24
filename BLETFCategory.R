library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(quantmod)
library(BLCOP)

#### Close data from TC2000 Convert TXT to XTS ####
ETFtxt <- read.table('etfc', header = FALSE, sep = ",")

ETFzoo <- read.zoo(ETFtxt, sep = ",", format = "%m/%d/%Y", split = 3)
ETFxts <- as.xts(ETFzoo)

ETFRet <- na.omit(Return.calculate(ETFxts))

###Convert to Monthly Data
monthend <- endpoints(ETFRet, on = "months", k = 1)
Ret.mo <- ETFRet[monthend]

CAPMbeta <- CAPM.beta(Ret.mo[,c(21,22,25,26,28)], Ret.mo$`SP-500`, .03/12)
#Apply Weigths to Returns
EQEqlibPort <- Return.portfolio(Ret.mo[,c(21,22,25,26,28)], weights = as.numeric(CAPMbeta))
#Calculate Excess Returns over Eq Lib Port
EQExcRet <- Return.excess(Ret.mo[,c(21,22,25,26,28)], EQEqlibPort)
#Create Cov Matrix of Excess Returns
CovMat <- cov.mve(EQExcRet)$cov
EQnames <- colnames(Ret.mo[,c(21,22,25,26,28)])
colnames(CovMat) <- EQnames

####FIXED INCOME BL Components####
FICAPMbeta <- CAPM.beta(Ret.mo[,c(20,23,24,27)], Ret.mo$AGG, .03/12)
#Apply Weigths to Returns
FIEqlibPort <- Return.portfolio(Ret.mo[,c(20,23,24,27)], weights = as.numeric(FICAPMbeta))
#Calculate Excess Returns over Eq Lib Port
FIExcRet <- Return.excess(Ret.mo[,c(20,23,24,27)], FIEqlibPort)
#Create Cov Matrix of Excess Returns
FICovMat <- cov.mve(FIExcRet)$cov
FInames <- colnames(Ret.mo[,c(20,23,24,27)])
colnames(FICovMat) <- FInames

##myPosterior Data##
priorMeans <- rep(0,5) #set means to 0 for the five equity ETFs
#priorVarcov <- cov.mve(ExcRet)$cov

####FIXED INCOME BL Components####
FIpriormeans <- rep(0,4)
#FIPriorVarCov <- cov.mve(FIexcRet)$cov

###Create a "pick" matrix.  Connects assets with a specific "view"
Pick <- matrix(0, ncol = ncol(CovMat), nrow = 6, dimnames = list(NULL,as.list(colnames(CovMat))))
FIPick <- matrix(0, ncol = ncol(FICovMat), nrow = 6, dimnames = list(NULL,as.list(colnames(FICovMat))))

###Create a Vector Q which contains information on the Excess Return for the corresponding "view"
QVect <- c(0,0,0,0,0,0)
FIQVec <- c(0,0,0,0,0,0)

##Fill Matrix with Picks on over and underperformance (Relative or Absolute)###
##Relative views in a row must net to zero (0).
##Absolute view in a row must add upt to one (1).
##Fill row1, col7 ==> pick[1,7]

Pick[1,1] <- 1
Pick[1,5] <- -1

# tau = scalar = set to as close to zero as possible (in practice) .025?
##Calculate the confidence of the Views (recipricols of the Variances of View Portfolios * tau)

ViewConf <- c(.01,.01,.01,.01,.01,.01) #Between .01(No confidence) and 100(High confidence)
FIConfidence <- c(.01,.01,.01,.01,.01,.01) #Between .01(No confidence) and 100(High confidence)

Views <- BLViews(Pick, QVect, confidences = ViewConf, assetNames = colnames(CovMat))
FIViews <- BLViews(FIPick,FIQVec, FIConfidence, assetNames = colnames(FICovMat))

###Generate "posterior" estimates using "prior" inputs and Investors Views and confidences
#ExRetPosterior <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = priorVarcov)
CAPMPosterior <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = CovMat)

#FIExcRetPost <- posteriorEst(FIViews,FIpriormeans, tau = .025, FIPriorVarCov)
FICAPMPost <- posteriorEst(FIViews, FIpriormeans, tau = .025, FICovMat)

#AltCAPMPost <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = CAPMCov)

#optimalPortfolios(ExRetPosterior)
optimalPortfolios(CAPMPosterior)

#optimalPortfolios(FIExcRetPost)
optimalPortfolios(FICAPMPost)

#optimalPortfolios.fPort(myPosterior, constraints = 'maxW[1:12] = .2',optimizer = "minriskPortfolio", numSimulations = 100)
