OLS <- lm(Obamameter ~ PID3 + Ideology + Vote08 + ObamaWins2012 + CountryTrack + 
            PresJob + PresEcon + PresForeign + PresHealth + PresAfghan +
            SupportACA + SupportTeaParty
          , data = Training)
PredictOLS <- predict(object = OLS, newdata = Test)

library(lme4)
MLM <- lmer(Obamameter ~ PID3 + Ideology + Vote08 + ObamaWins2012 + CountryTrack + 
              PresJob + PresEcon + PresForeign + PresHealth + PresAfghan +
              SupportACA + SupportTeaParty + (1 | StateVoteReg)
            , data = Training)
summary(MLM)
ranef(MLM)
PredictMLM <- predict(object = MLM, newdata = Test, na.action = na.pass, allow.new.levels = T)

library(randomForest)
trees <- randomForest(Obamameter ~ PID3 + Ideology + Vote08 + ObamaWins2012 + 
                        CountryTrack + PresJob + PresEcon + PresForeign + 
                        PresHealth + PresAfghan + SupportACA + SupportTeaParty
                      , data = na.omit(Training), ntrees = 1000)
summary(trees)
PredictTrees <- predict(trees, Test)

save(PredictOLS, file = 'FitR/data/PredictedOLS.RData')
save(PredictMLM, file = 'FitR/data/PredictedMLM.RData')
save(PredictTrees, file = 'FitR/data/PredictedTrees.RData')