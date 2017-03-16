library(foreign)

# Subset Training Data
setwd("~/Dropbox/Courses/Semester4/R/PS5")
FullData <- read.dta('anes_timeseries_2012_stata12.dta')
Variables <- FullData[ , c('ft_dpc', 'interest_whovote2008', 'presapp_track', 
                           'presapp_job', 'presapp_econ', 'presapp_foreign', 
                           'presapp_health', 'presapp_war', 'health_2010hcr', 
                           'libcpre_self', 'preswin_win', 'pid_self', 
                           'tea_supp', 'prevote_reg_state')]

# clean data
# recode refused, don't know, inapplicable, and haven't thought much about this as NA
Variables <- apply(Variables, 2, function(x) ifelse(substr(x, 1, 1) == '-', NA, x))
# recode binary variables. answers in favor/approving coded as 1
Variables[ ,c(3, 4, 5, 6, 7, 8)] <- apply(Variables[ ,c(3, 4, 5, 6, 7, 8)], 2, 
                                          function(x) ifelse(substr(x, 1, 1) == '1', 1, 0))
# recode trinary variables. 1 coded as favor/Obama; 2 as neither/other candidate; 3 as oppose/Rep candidate
Variables[ ,c(2, 9, 11, 13)] <- apply(Variables[ ,c(2, 9, 11, 13)], 2, 
                                      function(x) ifelse(substr(x, 1, 1) == '1', 1,
                                                         ifelse(substr(x, 1, 1) == '2', 3, 2)))
# combine levels 2 and 3 into single category
# variable now binary - will Obama win election? coded 1 for yes
Variables[, 11] <- ifelse(Variables[ , 11] == '1', 1, 0)
# recode ideology as numbers; collapse to 3 levels; increasing in conservatism
Variables[ , 10] <- ifelse(substr(Variables[ , 10], 1, 1) == '1', 1,
                           ifelse(substr(Variables[ , 10], 1, 1) == '2', 1,
                                  ifelse(substr(Variables[ , 10], 1, 1) == '3', 1,
                                         ifelse(substr(Variables[ , 10], 1, 1) == '4', 2, 3))))
# recode party id as numbers; increasing in conservatism
# independents, no preference, and other party combined in middle category. 
Variables[ , 12] <- ifelse(substr(Variables[ , 12], 1, 1) == '1', 1,
                           ifelse(substr(Variables[ , 12], 1, 1) == '2', 3, 2))

CleanData <- as.data.frame(Variables)
names(CleanData) <- c('Obamameter', 'Vote08', 'CountryTrack', 'PresJob', 
                      'PresEcon', 'PresForeign', 'PresHealth', 'PresAfghan', 
                      'SupportACA', 'Ideology', 'ObamaWins2012', 'PID3', 
                      'SupportTeaParty', 'StateVoteReg')
CleanData$Obamameter <- as.numeric(as.character(CleanData$Obamameter))


RandomHalf <- sample(1:nrow(CleanData), nrow(CleanData)/2)
Training <- CleanData[RandomHalf, ]
Test <- CleanData[-RandomHalf, ]

save(CleanData, file = 'FitR/data/CleanData.RData')
save(Training, file = 'FitR/data/TrainingData.Rdata')
save(Test, file = 'FitR/data/TestData.Rdata')
