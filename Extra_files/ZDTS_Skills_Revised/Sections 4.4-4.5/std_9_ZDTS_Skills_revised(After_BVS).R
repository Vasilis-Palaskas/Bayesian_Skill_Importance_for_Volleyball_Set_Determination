######---------------------------------------------------------------------------- 
######---------- SECTION 4.4: Ordered logistic model with only skills (Model estimation after  BVS)
# Load the proper libraries.
library(rstan)
library(coda)
library(shinystan)
library(loo)
library(bayesplot)
library(coefplot)
library(reshape2)
library(gridExtra)
library(xtable)
library(ggmcmc)
# Choose the working directory of this file (...\\Submitted_Appendix\\Ordered\\)

#---Data Preparation
source("C:\\Users\\vasileios palaskas\\Desktop\\Github folder\\Bayesian_Variable_Selection_Volleyball\\Extra_Data_Processes\\1-Data_Preparation\\Data_Preparation.R")#-Extra_Data_Processes/Data_Preparation.R
source("C:\\Users\\vasileios palaskas\\Desktop\\Github folder\\Bayesian_Variable_Selection_Volleyball\\Extra_Data_Processes\\1-Data_Preparation\\Athletes_Data_Preparation.R")#-Extra_Data_Processes/Athletes_Data_Preparation.R

# Choose the working directory of this file (...\\Submitted_Appendix\\ZDTS\\)
# setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_TA_Skills")

#------Skills for both Home and Away Teams
X_home<-data_by_sets[c(
  "Home_perfect_serves","Home_very_good_serves",
  "Home_failed_serves","Home_perfect_passes","Home_very_good_passes",
  "Home_poor_passes","Home_failed_passes","Home_perfect_att1",
  "Home_blocked_att1","Home_failed_att1","Home_perfect_att2",
  "Home_blocked_att2","Home_failed_att2","Home_perfect_blocks",
  "Home_net_violation_blocks","Home_failed_blocks","Home_failed_settings")
]

X_away<-data_by_sets[c(
  "Away_perfect_serves","Away_very_good_serves",
  "Away_failed_serves","Away_perfect_passes","Away_very_good_passes",
  "Away_poor_passes","Away_failed_passes","Away_perfect_att1",
  "Away_blocked_att1","Away_failed_att1","Away_perfect_att2",
  "Away_blocked_att2","Away_failed_att2","Away_perfect_blocks",
  "Away_net_violation_blocks","Away_failed_blocks","Away_failed_settings")
]

# Load the properly prepared data ("Data_ordered_skills").
# load("datalist_ordered")

X_home_diff<-data.frame(X_home-X_away)
colnames(X_home_diff)<-c(
  "perfect_serves","very_good_serves",
  "failed_serves","perfect_passes","very_good_passes",
  "poor_passes","failed_passes","perfect_att1",
  "blocked_att1","failed_att1","perfect_att2",
  "blocked_att2","failed_att2","perfect_blocks",
  "net_violation_blocks","failed_blocks","failed_settings")
#----Rename properly the skill variables
##----Skill events selected via the BVS process based on PSI Median Threshold
#### Standardization of the Model Matrices for numerical convenience

X_home_std<-data.frame(scale(X_home,center=T,scale=T) )
X_away_std<-data.frame(scale(X_away,center=T,scale=T) )
X_home_diff_std<-data.frame(scale(X_home-X_away,center=T,scale=T) )
X_away_diff_std<-data.frame(scale(X_away-X_home,center=T,scale=T) )

#--Selected covariates via the Gibbs BVS Method

final_X_home_std<-X_home_std[,colnames(X_home_std)%in%c("Home_failed_serves","Home_poor_passes",
                                                        "Home_failed_passes","Home_blocked_att1",
                                                        "Home_failed_att1")]

final_X_away_std<-X_away_std[,colnames(X_away_std)%in%c("Away_failed_serves",
                                                        "Away_failed_passes",
                                                        "Away_blocked_att1","Away_failed_att1",
                                                        "Away_perfect_att2","Away_failed_att2",
                                                        "Away_net_violation_blocks","Away_failed_blocks")]


#---datalist required for the revised ZDTS with only+Skills
data_zdts_only_skills<-list(c_thres=2,c_std=9,
                            n_games=dim(data_by_sets)[1],
                            n_teams=length(levels(data_by_sets$home_Team)),
                            X_home=final_X_home_std,X_away=final_X_away_std,
                            K_home=ncol(final_X_home_std),
                            K_away=ncol(final_X_away_std),
                            home_sets=data_by_sets$home_sets,
                            away_sets=data_by_sets$away_sets)

#---Set appropriate working directory
setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_Skills_Revised/Sections 4.4-4.5")## Run ZDTS_only_Skills_after_BVS.stan

ZDTS_only_Skills_after_BVS<-stan("ZDTS_Skills_after_BVS.stan",
                                 data=data_zdts_only_skills,chains=2,
                                 init_r=0.5,cores=2,
                                 iter=12000,
                                 warmup=2000)### R

save(ZDTS_only_Skills_after_BVS,file="revised_ZDTS_only_Skills_after_BVS_with_std_9")
load(file="revised_ZDTS_only_Skills_after_BVS_with_std_9")
###--------------Predictive Model Performance Evaluation-------------------------########

# Calculation of the DIC (Gelman,2004)
DIC_Gelman<-function(dev){
  res<-mean(dev)+0.5*var(dev)
  return(res)
}
###----Extraction of several posterior quantities (deviances, log-likelihoods)

deviance_ZDTS_only_Skills_after_BVS<-rstan::extract(ZDTS_only_Skills_after_BVS,pars="dev")$dev
log_lik_ZDTS_only_Skills_after_BVS<- extract_log_lik(ZDTS_only_Skills_after_BVS)
r_eff_log_lik_ZDTS_only_Skills_after_BVS<- relative_eff(exp(log_lik_ZDTS_only_Skills_after_BVS),chain_id=rep(1:2,each=10000))

# ---WAIC, DIC, LOOIC Bayesian Information Criteria for the ordered logistic with only skills as covariates

waic(log_lik_ZDTS_only_Skills_after_BVS)####  241.3 18.7
loo(log_lik_ZDTS_only_Skills_after_BVS,
    r_eff=r_eff_log_lik_ZDTS_only_Skills_after_BVS)#   242.2 18.8
DIC_Gelman(deviance_ZDTS_only_Skills_after_BVS)#  243.5591

###--------------Posterior Summary Sonlytistics-Analysis------------------------########

#####----------------------Posterior summary----------------------------######

names(ZDTS_only_Skills_after_BVS)[1:5]<-c("(Home) failed serves",
                                          "(Home) poor passes",
                                          "(Home) failed passes",
                                          "(Home) blocked att1",
                                          "(Home) failed att1")
names(ZDTS_only_Skills_after_BVS)[6:13]<-c("(Away) failed serves",
                                           "(Away) failed passes",
                                           "(Away) blocked att1",
                                           "(Away) failed att1",
                                           "(Away) perfect att2",
                                           "(Away) failed att2",
                                           "(Away) block net violations",
                                           "(Away) failed blocks" )


print(ZDTS_only_Skills_after_BVS,
      pars=c("mu","home",
             "beta_home","beta_away","dev"),probs = c(0.025,0.5,0.975), digits=2)

#-Access summary statistics
# df_of_draws <- as.data.frame(ZDTS_only_Skills_after_BVS)
# print(colnames(df_of_draws))
# fit_summary <- summary(ZDTS_only_Skills_after_BVS)
betas_summary <- summary(ZDTS_only_Skills_after_BVS, pars = c("mu","home","beta_home","beta_away"), probs = c(0.025, 0.5,0.95))$summary
print(round(betas_summary[,c(1,3,4,5,6)],2))
betas_summary_main<-round(betas_summary[,c(1,3,4,5,6)],2)
xtable(betas_summary_main)
