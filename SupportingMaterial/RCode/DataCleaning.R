# Code to clean ANES data and create training/test datasets
# this is provided for the sake of transparancy, but should not be run
library(foreign)

# Subset Training Data
FullData <- read.dta('ANES.dta')
Variables <- FullData[ , c('ft_dpc', 'presapp_track', 'presapp_job', 
                           'presapp_econ', 'presapp_foreign', 'presapp_health', 
                           'presapp_war', 'libcpre_self', 'pid_self')]

# clean data
# recode refused, don't know, inapplicable, and haven't thought much about this as NA
Variables <- apply(Variables, 2, function(x) ifelse(substr(x, 1, 1) == '-', NA, x))
# recode binary variables. answers in favor/approving coded as 1
Variables[ , 2:7] <- apply(Variables[ , 2:7], 2, 
                           function(x) ifelse(substr(x, 1, 1) == '1', 1, 0))
# recode ideology as numbers; collapse to 3 levels; increasing in conservatism
Variables[ , 8] <- ifelse(substr(Variables[ , 8], 1, 1) == '1', 1,
                          ifelse(substr(Variables[ , 8], 1, 1) == '2', 1,
                                 ifelse(substr(Variables[ , 8], 1, 1) == '3', 1,
                                        ifelse(substr(Variables[ , 8], 1, 1) == '4', 
                                               2, 3))))
# recode party id as numbers; Dem - Other - Rep
# independents, no preference, and other party combined in middle category. 
Variables[ , 9] <- ifelse(substr(Variables[ , 9], 1, 1) == '1', 1,
                           ifelse(substr(Variables[ , 9], 1, 1) == '2', 3, 2))

# create and name clean data set
CleanData <- as.data.frame(Variables)
names(CleanData) <- c('Obamameter', 'CountryTrack', 'PresJob', 'PresEcon', 
                      'PresForeign', 'PresHealth', 'PresAfghan', 'Ideology', 'PID3')
CleanData$Obamameter <- as.numeric(as.character(CleanData$Obamameter))

# split data into training and test halves
RandomHalf <- sample(1:nrow(CleanData), nrow(CleanData)/2)
Training <- CleanData[RandomHalf, ]
Test <- CleanData[-RandomHalf, ]

save(CleanData, file = 'PS5/SupportingMaterial/RData/CleanData.RData')
save(Training, file = 'PS5/SupportingMaterial/RData/TrainingData.Rdata')
save(Test, file = 'PS5/SupportingMaterial/RData/TestData.Rdata')
