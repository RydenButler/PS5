# Subset Training Data
setwd("~/Dropbox/Courses/Semester4/R/PS5")
library(foreign)
data <- read.dta('anes_timeseries_2012_stata12.dta')
RandomHalf <- sample(1:nrow(data), nrow(data)/2)
Training <- data[RandomHalf, ]
Test <- data[!RandomHalf, ]
lm(ft_dpc ~ interest_attention # pol attentiveness
   + interest_whovote2008 # 2008 pres vote
   + prevote_presvtwho # intended 2012 vote
   + presapp_track # country on right track
   + presapp_job # Obama job approval
   + presapp_econ # Obama econ
   + presapp_foreign # Obama foreign policy
   + presapp_health # Obama health care
   + presapp_war # Obama on Afghanistan
   + health_2010hcr # Favor ACA
   + 
   , data = Training)
