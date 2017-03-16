#' Code to generate prediction models and save predicted data
#' 
#' Not to be run by user, but available for information as to how
#' predicted data were generated.
#'
#'
#' @author Ryden W. Butler: \email{r.butler@@wustl.edu}

OLS <- lm(Obamameter ~ PID3 + Ideology + CountryTrack + PresJob + PresEcon + 
            PresForeign + PresHealth + PresAfghan
            , data = Training)
PredictOLS <- predict(object = OLS, newdata = Test)

library(lme4)
MLM <- lmer(Obamameter ~ Ideology + CountryTrack + PresJob + PresEcon + 
              PresForeign + PresHealth + PresAfghan + (1 | PID3)
            , data = Training)
summary(MLM)
ranef(MLM)
PredictMLM <- predict(object = MLM, newdata = Test, na.action = na.pass, allow.new.levels = T)

library(randomForest)
trees <- randomForest(Obamameter ~ PID3 + Ideology + CountryTrack + PresJob + 
                        PresEcon + PresForeign + PresHealth + PresAfghan
                      , data = na.omit(Training), ntrees = 1000)
summary(trees)
PredictTrees <- predict(trees, Test)

PredictMatrix <- cbind(PredictOLS, PredictMLM, PredictTrees)
colnames(PredictMatrix) <- c('OLS', 'MLM', 'Tree')

save(PredictOLS, file = 'FitR/data/PredictedOLS.RData')
save(PredictMLM, file = 'FitR/data/PredictedMLM.RData')
save(PredictTrees, file = 'FitR/data/PredictedTrees.RData')
save(PredictMatrix, file = 'FitR/data/PredictedMatrix.RData')
