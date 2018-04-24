library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(quantmod)
library(BLCOP)

#### Close data from TC2000 Convert TXT to XTS ####
FItxt <- read.table('blbond', header = FALSE, sep = ",")

FIzoo <- read.zoo(FItxt, sep = ",", format = "%m/%d/%Y", split = 3)
FIxts <- as.xts(FIzoo)

FIRet <- na.omit(Return.calculate(FIxts))

###Convert to Monthly Data
monthend <- endpoints(FIRet, on = "months", k = 1)
Ret.mo <- FIRet[monthend]

FIETFCAPMbeta <- CAPM.beta(Ret.mo[,2:13], Ret.mo$AGG, .03/12)
#Apply Weigths to Returns of Individual Assets
FIETFEqLib <- Return.portfolio(Ret.mo[,2:13], weights = as.numeric(FIETFCAPMbeta))
#Calculate Excess Returns over Eq Lib Port
FIETFExcRet <- Return.excess(Ret.mo[,2:13], FIETFEqLib)
#Create Cov Matrix of Excess Returns
CovMat <- cov.mve(FIETFExcRet)$cov
nameList <- names(Ret.mo[,2:13])
colnames(CovMat) <- nameList

##myPosterior Data##
priorMeans <- rep(0,12) #set means to 0 for the twelve assets

###Create a "pick" matrix.  Connects assets with a specific "view"
Pick <- matrix(0, ncol = ncol(CovMat), nrow = 6, dimnames = list(NULL,as.list(colnames(nameList))))

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
