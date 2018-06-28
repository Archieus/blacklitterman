library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(quantmod)
library(BLCOP)

#### Close data from TC2000 Convert TXT to XTS ####
ETFtxt <- read.table('fidetfbl', header = FALSE, sep = ",")

ETFzoo <- read.zoo(ETFtxt, sep = ",", format = "%m/%d/%Y", split = 3)
ETFxts <- as.xts(ETFzoo)

ETFRet <- na.omit(Return.calculate(ETFxts))
# ETFPx <- na.omit(ETFxts[,c(1:15,17:18)])

###Convert to Monthly Data
monthend <- endpoints(ETFRet, on = "months", k = 1)
Ret.mo <- ETFRet[monthend]

#ETFPx.mo <- ETFPx[monthend]

###Construct Equilibrium CAPM Portfolio###
###Generate Weights Using beta, beta of the assets relative to the S&P500###
FidCAPMbeta <- CAPM.beta(Ret.mo[,c(1:15,17:18)], Ret.mo$`SP-500`, .03/12) #Portfolio Weights

###Generate Regressed Betas using price data###
# slope.b <- do.call(merge, lapply(ETFPx.mo[,c(1:4,6:17)], function(x) na.omit(rollSFM(log(x), ETFPx.mo$`SP-500`,12)$beta)))
# Beta.names <- colnames(ETFPx.mo[,c(1:15,17:18)])
# colnames(slope.b) <- Beta.names

#Apply Weigths to Returns
EqlibPort <- Return.portfolio(Ret.mo[,c(1:15,17:18)], weights = as.numeric(FidCAPMbeta))
#Calculate Excess Returns over Eq Lib Port
FidExcRet <- Return.excess(Ret.mo[,c(1:15,17:18)], EqlibPort)
#Create Cov Matrix of Excess Returns
FidCovMat <- cov.mve(FidExcRet)$cov

##myPosterior Data##
FidpriorMeans <- rep(0,17) #set means to 0 for the five equity ETFs

Fidpick <- matrix(0, ncol = ncol(FidCovMat), nrow = 6, dimnames = list(NULL,as.list(colnames(FidCAPMbeta))))

FidQViews <- c(0,0,0,0,0,0) #Excess Returns based on User "Views"

####Fill in Pick Matrix with over- and under-performance####
##Fill row1, col7 ==> pick[1,7]
Fidpick[1,16] <- 0
Fidpick[1,17] <- 0

FidConfidence <- c(.01,.01,.01,.01,.01,.01) #Confidence for Views 1 - 6 (if any) [Low = 0, High = 100]
FidViews <- BLViews(Fidpick,FidQViews, FidConfidence, assetNames = colnames(FidCAPMbeta))

FidExcRetPosterior <- posteriorEst(FidViews, mu = FidpriorMeans, tau = 0.025, sigma = FidCovMat)

optimalPortfolios(FidExcRetPosterior)
