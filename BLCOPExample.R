library(BLCOP)
## example from Thomas M. Idzorek's paper "A STEP-BY-STEP GUIDE TO THE BLACK-LITTERMAN MODEL"
# http://corporate.morningstar.com/ib/documents/MethodologyDocuments/IBBAssociates/BlackLitterman.pdf

x <- c(0.001005,0.001328,-0.000579,-0.000675,0.000121,0.000128,-0.000445,-0.000437 ,
       0.001328,0.007277,-0.001307,-0.000610,-0.002237,-0.000989,0.001442,-0.001535 ,
       -0.000579,-0.001307,0.059852,0.027588,0.063497,0.023036,0.032967,0.048039 ,
       -0.000675,-0.000610,0.027588,0.029609,0.026572,0.021465,0.020697,0.029854 ,
       0.000121,-0.002237,0.063497,0.026572,0.102488,0.042744,0.039943,0.065994 ,
       0.000128,-0.000989,0.023036,0.021465,0.042744,0.032056,0.019881,0.032235 ,
       -0.000445,0.001442,0.032967,0.020697,0.039943,0.019881,0.028355,0.035064 ,
       -0.000437,-0.001535,0.048039,0.029854,0.065994,0.032235,0.035064,0.079958 )

varCov <- matrix(x, ncol = 8, nrow = 8)

#Expected Excess Return Vectors in Percentage terms
mu <- c(0.08, 0.67,6.41, 4.08, 7.43, 3.70, 4.80, 6.60) / 100

###Create pick Matrix to be filled with values
pick <- matrix(0, ncol = 8, nrow = 3, dimnames = list(NULL, letters[1:8]))

##Fill Pick matrix  with Views of which will outperform  or underperform versus a specific component
##Use relative weighting to adjust for capitalization & avoid unnecessary tracking error##

pick[1,7] <- 1 #fill row1, col7
pick[2,1] <- -1; pick[2,2] <- 1 #fill row2, col1
pick[3, 3:6] <- c(0.9, -0.9, .1, -.1) #fill row3, col3 to col6

##tau is set to as close to zero in practice, int this example tau = .025
# Error Term = Variance of View portfolios * tau

# .000709 = 2.836%/100 * .025
# .000141 = 0.563%/100 * .025
# .000866 = 3.462%/100 * .025

#confidence is the recipricol of the error term * tau
confidences <- 1 / c(0.000709, 0.000141, 0.000866)

myViews <- BLViews(pick, c(0.0525, 0.0025, 0.02), confidences, letters[1:8])

myPosterior <- posteriorEst(myViews, tau = 0.025, mu, varCov )

optimalPortfolios(myPosterior)
