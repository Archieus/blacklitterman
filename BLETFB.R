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
CAPM.roll <- na.omit(rollapply(Ret.mo[,c(1:8,10:21)], 12, function(x) CAPM.beta(x, Ret.mo$SPY, .03/12)))
CovMat <- cov(CAPM.roll)

ExcRet <- Return.excess(Ret.mo[,c(1:8,10:21)], Ret.mo$SPY) #mu
ERAnnl <- Return.annualized.excess(Ret.mo[,c(1:8,10:21)], Ret.mo$SPY)
#ER.mu <- as.numeric(colMeans(ExcRet))

########## Risk Premium Divided by Excess Returns########
###Risk Premium###
# RiskPrem <- Return.excess(Ret.mo$SPY, Rf = .029/12)
# PremData <- na.omit(cbind(ExcRet,RiskPrem))
# 
# RiskAver <- matrix(0, nrow = nrow(PremData), ncol = ncol(PremData))
# for (i in 1:ncol(PremData)) {
#   RiskAver[i] <- PremData[,20] / PremData[,i]
# }
# 
# RiskAv.df <- as.data.frame(RiskAver)
# rownames(RiskAv.df) <- index(PremData)
# names(RiskAv.df) <- colnames(PremData)
# RiskAv.x <- as.xts(RiskAv.df[,1:19])
# CAPMCov <- cov(RiskAv.x)

##myPosterior Data##
priorMeans <- rep(0,20) #set means to 0 for the twelve assets
priorVarcov <- cov.mve(ExcRet)$cov

###Create a "pick" matrix.  Connects assets with a specific "view"
Pick <- matrix(0, ncol = ncol(priorVarcov), nrow = 6, dimnames = list(NULL,as.list(colnames(priorVarcov))))

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

Views <- BLViews(Pick, QVect, confidences = ViewConf, assetNames = colnames(priorVarcov))

###Generate "posterior" estimates using "prior" inputs and Investors Views and confidences
ExRetPosterior <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = priorVarcov)
CAPMPosterior <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = CovMat)

#AltCAPMPost <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = CAPMCov)

optimalPortfolios(ExRetPosterior)
optimalPortfolios(CAPMPosterior)

#optimalPortfolios.fPort(myPosterior, constraints = 'maxW[1:12] = .2',optimizer = "minriskPortfolio", numSimulations = 100)

#write.csv(CAPMbeta, file = "ETFBeta.csv")
#write.csv(ERAnnl, file = "ETFExcRet.csv")