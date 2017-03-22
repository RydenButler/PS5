library(devtools)
library(roxygen2)
setwd("~/Dropbox/Courses/Semester4/R/PS5")

# Run this once to create package structure
#package.skeleton('FitR')

## This must be run prior ot using the package
current.code <- as.package("FitR")
load_all(current.code)
document(current.code)

# Load data and create naive forecast
load('SupportingMaterial/RData/PredictedMatrix.RData')
load('SupportingMaterial/RData/TestData.RData')
Naive <- rep(50, nrow(PredictMatrix))

# Check that the default set-up works
fitstats(y=Test$Obamameter, P=PredictMatrix)
# Check that the default set-up still works w/ r
fitstats(y = Test$Obamameter, P = PredictMatrix, r = Naive)
# Check that error throws when asking for all fit stats w/o r
fitstats(y = Test$Obamameter, P = PredictMatrix, HideFits = NULL)
# Check that all fitstats print when r is provided and no fits are hidden
fitstats(y = Test$Obamameter, P = PredictMatrix, r = Naive, HideFits = NULL)
# Check that different fits can be hidden
fitstats(y = Test$Obamameter, P = PredictMatrix, r = Naive, 
         HideFits = c('MAPE', 'MEAPE'))

# Based on the forecasting model that minimizes the calculated fit statistics,
# it appears that the OLS provides the best predictions of the test data.


