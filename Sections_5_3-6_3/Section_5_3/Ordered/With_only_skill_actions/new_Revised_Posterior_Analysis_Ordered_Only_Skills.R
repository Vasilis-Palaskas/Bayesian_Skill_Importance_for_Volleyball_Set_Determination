# Load the proper libraries.
library(rstan)
library(coda)
library(bayesplot)
library(ggmcmc)
library(car)
library(xtable)
library(dplyr)
library(corrplot)
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
# Calculate the difference in frequencies of each skill actions between home and away teams per match.
X_home_diff<-data.frame(X_home-X_away)
# Rename the differences of skill actions using only the skill actions' names without using prefix or suffix of _diff (in terms of convenience)
colnames(X_home_diff)<-c(
  "perfect_serves","very_good_serves",
  "failed_serves","perfect_passes","very_good_passes",
  "poor_passes","failed_passes","perfect_att1",
  "blocked_att1","failed_att1","perfect_att2",
  "blocked_att2","failed_att2","perfect_blocks",
  "net_violation_blocks","failed_blocks","failed_settings")


# Transform set difference for  the fitting of ordered multinomial model (requires positive integers or factors)

data_by_sets = data_by_sets %>% dplyr::mutate(
  sets_difference_factor = case_when(
    (sets_difference==(-3)) ~ 1,
    (sets_difference ==(-2)) ~ 2,
    (sets_difference ==(-1))~ 3,
    (sets_difference ==(1))~ 4,
    (sets_difference ==(2))~ 5,
    (sets_difference==(3))~ 6
  )
)


##  Model 5: After inclusion only variables not being collinear with each other (by excluding them based on VIFs)
##  we will run a model including only skill actions emerged by the Gibbs Variable Selection implemented to those not-collinear variables
skill_events<-X_home_diff

X_ordered_Skills<-skill_events[,colnames(skill_events)%in%
                                 c("perfect_serves","failed_serves",
                                   "perfect_att1","failed_att1",
                                   "perfect_att2","failed_att2",
                                   "perfect_blocks",
                                   "failed_settings") ]
model_data<-list(Y=data_by_sets$sets_difference_factor,X=X_ordered_Skills,n_teams=
                   length(levels(data_by_sets$home_Team)),
                 N=dim(data_by_sets)[1],K=ncol(X_ordered_Skills),ncat=6)
#----Set working directory
setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/Sections_5_3-6_3/Section_5_3/Ordered/With_only_skill_actions")

ordered_skills_after_BVS_model<-stan("ordered_skills_after_BVS.stan",iter=14000, warmup=4000,chains=2,thin=2,
                                     data=model_data,control=list(max_treedepth=15),cores=2)


save(ordered_skills_after_BVS_model,file="revised_ordered_skills_after_BVS")
load("revised_ordered_skills_after_BVS")
###--------------Predictive Model Performance Evaluation using DIC, WAIC, LOO
#----------
# Calculation of the DIC (Gelman,2004)
DIC_Gelman<-function(dev){
  res<-mean(dev)+0.5*var(dev)
  return(res)
}
###----Extraction of several posterior quantities (deviances, log-likelihoods)

deviance_ordered_skills_after_BVS_model<-rstan::extract(ordered_skills_after_BVS_model,pars="dev")$dev
log_lik_ordered_skills_after_BVS_model<- extract_log_lik(ordered_skills_after_BVS_model)
r_eff_log_lik_ordered_skills_after_BVS_model<- relative_eff(exp(log_lik_ordered_skills_after_BVS_model),
                                                            chain_id=rep(1:2,each=5000))

# ---WAIC, DIC, LOOIC Bayesian Information Criteria for the ordered logistic with only skills as covariates

waic(log_lik_ordered_skills_after_BVS_model)####  247.1 19.9
loo(log_lik_ordered_skills_after_BVS_model,
    r_eff=r_eff_log_lik_ordered_skills_after_BVS_model)#    247.3 19.9
DIC_Gelman(deviance_ordered_skills_after_BVS_model)#245.2


#----------

