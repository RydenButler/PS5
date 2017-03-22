# Code to generate prediction models and save predicted data
# this is provided for the sake of transparency, but should not be run

# calculate OLS
OLS <- lm(Obamameter ~ PID3 + Ideology + CountryTrack + PresJob + PresEcon + 
            PresForeign + PresHealth + PresAfghan
            , data = Training)
PredictOLS <- predict(object = OLS, newdata = Test)

library(lme4)
# calculate MLM
MLM <- lmer(Obamameter ~ Ideology + CountryTrack + PresJob + PresEcon + 
              PresForeign + PresHealth + PresAfghan + (1 | PID3)
            , data = Training)
summary(MLM)
ranef(MLM)
PredictMLM <- predict(object = MLM, newdata = Test, na.action = na.pass, allow.new.levels = T)

library(randomForest)
# calculate random forest
trees <- randomForest(Obamameter ~ PID3 + Ideology + CountryTrack + PresJob + 
                        PresEcon + PresForeign + PresHealth + PresAfghan
                      , data = na.omit(Training), ntrees = 1000)
summary(trees)
PredictTrees <- predict(trees, Test)

# Creat matrix of predictions and save data
PredictMatrix <- cbind(PredictOLS, PredictMLM, PredictTrees)
colnames(PredictMatrix) <- c('OLS', 'MLM', 'Tree')
save(PredictMatrix, file = 'PS5/SupportingMaterial/RCode/PredictedMatrix.RData')
