library(devtools)
library(roxygen2)

# Run this once to create package structure
#package.skeleton('FitR')

## This can be run many times as the code is updates
current.code <- as.package("FitR")
load_all(current.code)
document(current.code)



fitstat(testy, testP)
