library(PortfolioAnalytics)
library(fPortfolio)
library(quantmod)
library(BLCOP)
Sys.setenv(TZ = 'GMT')

#### reads/loads text into R and converts to XTS ####
FIETFCtxt <- read.table('bletfcfi', header = FALSE, sep = ',')
FIETFCzoo <- read.zoo(FIETFCtxt, sep = ",", format = "%m/%d/%Y", split = 3)
FINCOMExts <- na.omit(as.xts(FIETFCzoo))

####EXPONENTIAL REGRESSION####
stkreg.d <- cbind(FINCOMExts, "Days" = 1:nrow(FINCOMExts))

reg.r2 <- do.call(merge, lapply(stkreg.d, function(x) na.omit(rollSFM(log(x), stkreg.d$Days, 90)$r.squared)))
r.sqrd <- (reg.r2[,1:ncol(stkreg.d)-1])
names(r.sqrd) <- colnames(FINCOMExts)

slope.b <- do.call(merge, lapply(stkreg.d, function(x) na.omit(rollSFM(log(x), stkreg.d$Days,90)$beta)))
Be.ta <- slope.b[,1:ncol(stkreg.d)-1]
names(Be.ta) <- colnames(FINCOMExts)

Ann.sl <- ((exp(Be.ta)^250)-1)*100
names(Ann.sl) <- colnames(FINCOMExts)
Adj.sl <- round((r.sqrd * Ann.sl),4)
names(Adj.sl) <- colnames(FINCOMExts)

###Ranking in ascending order where 3 is the strongest (if 1 is strongest then###
#as.xts(t(apply(-Adj.sl[,c(2,5,10)],1,rank)))

FID.sl <- as.xts(t(apply(Adj.sl[,c(2,5,10)],1,rank)))
SCH.sl <- as.xts(t(apply(Adj.sl[,c(3,4,9)],1,rank)))
TDA.sl <- as.xts(t(apply(Adj.sl[,c(6:8)],1,rank)))

###Calculate Monthly Returns###
FINCOMEret.df <- as.data.frame(lapply(FINCOMExts, function(x) monthlyReturn(x)))
colnames(FINCOMEret.df) <- colnames(FINCOMExts)
Ret.mo <- as.xts(FINCOMEret.df)

#### BEGIN Calculations to try to determine the Prior Means as opposed to setting to zero (0)
#### Calculate CVaR aka Expected Shortfall (ES)  aka Expected Tail Loss (ETL) 
DailyRet.df <- as.data.frame(lapply(FINCOMExts, function(x) dailyReturn(x)))
colnames(DailyRet.df) <- colnames(FINCOMExts)
Ret.Daily <- as.xts(DailyRet.df)

ES.1Day <- t(as.data.frame(sapply(Ret.Daily, function(x) ES(x, p =0.99, method = "modified"))))
ES.10Day <- sqrt(10)*ES.1Day #Convert 1-day ES to 10-day based on Basel Committee FRTB
ES10.Dlr <- last(FINCOMExts)*ES.10Day ##Convert to Dollar Loss

####Weights derived by ES Values####
FID.Wts <- as.numeric(ES.10Day[,c(2,5,9)]/sum(ES.10Day[,c(2,5,9)]))
SCH.Wts <- as.numeric(ES.10Day[,c(3,4,9)]/sum(ES.10Day[,c(3,4,9)]))
TDA.Wts <- as.numeric(ES.10Day[,6:8]/sum(ES.10Day[,6:8]))

EMA.90 <- as.data.frame(lapply(Ret.Daily, function(x) round(last(EMA(x, n = 90)),6)))
colnames(EMA.90) <- colnames(FINCOMExts)
#### END Calculations to try to determine the Prior Means as opposed to setting to zero (0)

#FIDELITY BOND ETFS = ,c(2,5,10)
#SCHWAB BOND ETFS = ,c(3,4,9)
#TDA BOND ETFS = ,6:8

FINCOMEbeta <- CAPM.beta(Ret.mo[,6:8], Ret.mo$AGG, .03/12)
#Apply Weigths to Returns of Individual Assets
FINCOMEEqLib <- Return.portfolio(Ret.mo[,6:8], weights = as.numeric(FINCOMEbeta))
#Calculate Excess Returns over Eq Lib Port
FINCOMEExcRet <- Return.excess(Ret.mo[,6:8], FINCOMEEqLib)
#Create Cov Matrix of Excess Returns
CovMat <- cov.mve(FINCOMEExcRet)$cov
nameList <- names(Ret.mo[,6:8])
colnames(CovMat) <- nameList

##myPosterior Data##
# Hist.mu <- EMA.90[,6:8]
# priorMeans <- as.numeric(1/TDA.Wts)
                       
priorMeans <- rep(0,3) #set means to 0 for the respective assets

###Create a "pick" matrix.  Connects assets with a specific "view"
Pick <- matrix(0, ncol = ncol(CovMat), nrow = 3, dimnames = list(NULL,as.list(colnames(CovMat))))

###Create a Vector Q which contains information on the Excess Return for the corresponding "view"
QVect <- c(0,0,0)

##Fill Matrix with Picks on over and underperformance (Relative or Absolute)###
##Relative views in a row must net to zero (0).
##Absolute view in a row must add upt to one (1).
##Fill row1, col7 ==> pick[1,7]

Pick[1,3] <- 1
Pick[1,2] <- -1

# tau = scalar = set to as close to zero as possible (in practice) .025?
##Calculate the confidence of the Views (recipricols of the Variances of View Portfolios * tau)

ViewConf <- c(.01,.01,.01) #Between .01(No confidence) and 100(High confidence)

Views <- BLViews(Pick, QVect, confidences = ViewConf, assetNames = colnames(CovMat))

###Generate "posterior" estimates using "prior" inputs and Investors Views and confidences
CAPMPosterior <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = CovMat)

optimalPortfolios(CAPMPosterior)
