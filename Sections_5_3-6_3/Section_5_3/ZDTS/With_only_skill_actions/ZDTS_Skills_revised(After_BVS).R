# Load the proper libraries.
library(rstan)
library(coda)
library(bayesplot)
library(skellam)
options(mc.cores = parallel::detectCores())# Activate multiple cores for stan models

################Data Preparation
#---------
source("C:\\Users\\vasileios palaskas\\Documents\\GitHub\\Bayesian_Skill_Importance_for_Volleyball_Set_Determination\\Section_2_1_Data_Processing\\Data_Preparation.R")#-Data_Preparation.R

# Dataframes with Skill actions of both Home and Away Teams
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


#### Standardization of the numeric features (frequencies of skill actions) to avoid numeric overflow and speed mcmc convergence

X_home_std<-data.frame(scale(X_home,center=T,scale=T) )
X_away_std<-data.frame(scale(X_away,center=T,scale=T) )
#---------
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
data_zdts_only_skills<-list(c_thres=2,c_std=5,
                            n_games=dim(data_by_sets)[1],
                            n_teams=length(levels(data_by_sets$home_Team)),
                            X_home=final_X_home_std,X_away=final_X_away_std,
                            K_home=ncol(final_X_home_std),
                            K_away=ncol(final_X_away_std),
                            home_sets=data_by_sets$home_sets,
                            away_sets=data_by_sets$away_sets)

#---Set appropriate working directory
setwd("~/GitHub/Bayesian_Skill_Importance_for_Volleyball_Set_Determination/Sections_5_3-6_3/Section_5_3/ZDTS/With_only_skill_actions")
ZDTS_only_Skills_after_BVS<-stan("ZDTS_Skills_after_BVS.stan",
                                 data=data_zdts_only_skills,chains=2,
                                 init_r=0.5,cores=2,
                                 iter=12000,
                                 warmup=2000)### R

save(ZDTS_only_Skills_after_BVS,file="revised_ZDTS_only_Skills_after_BVS")
load(file="revised_ZDTS_only_Skills_after_BVS")
###--------------Predictive Model Performance Evaluation using DIC, WAIC, LOO
#----------
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

waic(log_lik_ZDTS_only_Skills_after_BVS)####   240.9 18.7
loo(log_lik_ZDTS_only_Skills_after_BVS,
    r_eff=r_eff_log_lik_ZDTS_only_Skills_after_BVS)#  241.8 18.7
DIC_Gelman(deviance_ZDTS_only_Skills_after_BVS)#  242.5
#----------
