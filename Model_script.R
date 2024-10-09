#Biological, social, and ecological correlates of social play in immature Bwindi mountain gorillas.


#importing packages
library(tidyverse)
library(Matrix)
library(TMB)
library(glmmTMB)
library("DHARMa")
library(pscl)
library(performance)
library(rstatix)
library(flexplot)
library(pivottabler)
library(ggplot2)
library(ggeffects)
library(effects)
library(emmeans)
library("ggiraphExtra")
library(sjlabelled)

#Adding Data
play_data2_8_clean_non_rank_no_KYAB<-read.csv("Research_data.csv")

#######RUNNING MODELS#######

#rename the group variable so it doesn't interact with function names
colnames(play_data2_8_clean_non_rank_no_KYAB)[which(names(play_data2_8_clean_non_rank_no_KYAB)=="group")]<-"grp"

#Model without group as random effect and with a quadratic term for partner availability
Play_model<-glmmTMB(play_secs ~ sex_z*poly(age_z,2) + mum_parity_z + weaning_status_z  + fruit_consumption_full_z 
                     + partner_availability_z + I(partner_availability_z^2) + offset(log(focal_secs_non_UN)) + (1|mother:id)   + (1|year),
                    family = nbinom1, ziformula = ~age_z, data=play_data2_8_clean_non_rank_no_KYAB) 


#Diagnostics
simulationOutput <- simulateResiduals(fittedModel = Play_model, plot = T)
testResiduals(Play_model)
check_collinearity(Play_model)
summary(Play_model)
testDispersion(Play_model)
testUniformity(Play_model) 
testOutliers(Play_model)
testQuantiles(Play_model)
testZeroInflation(Play_model)

#for individual p values for test Quantiles
x<-testQuantiles(simulationOutput)
x$pvals
testZeroInflation(Play_model)
drop1(Play_model)

summary(Play_model)

#full null comparison
#null model 
nullmodel<-glmmTMB(play_secs ~ offset(log(focal_secs_non_UN)) + (1|mother:id) + (1|year), family = nbinom1, ziformula = ~age_z, data=play_data2_8_clean_non_rank_no_KYAB) 

summary(nullmodel)
simulationOutput <- simulateResiduals(fittedModel = nullmodel, plot = T)

#Full-null model comparison
anova(Play_model,nullmodel, test = "Chisq")
