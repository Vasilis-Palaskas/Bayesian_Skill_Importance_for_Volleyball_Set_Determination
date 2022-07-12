# Load the proper libraries.

library(rstan)
library(coda)
library(shinystan)
library(loo)
library(bayesplot)
library(coefplot)
library(reshape2)
library(gridExtra)
library(ggmcmc)
# Choose the working directory where multiple model formulations exist
setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/Section_3_5_different_zdts_formulations")

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
X_home_diff_std<-data.frame(scale(X_home-X_away,center=T,scale=T) )
X_away_diff_std<-data.frame(scale(X_away-X_home,center=T,scale=T) )


# Datalists required running the R-Stan models. 
# Caution: Two different  datalists  incorporate the five (5) candidate model formulations. 

# Datalist1: Incorporates the model formulations 1, 4
data_separate_skills<-list(c_thres=2,c_std=5,
                          n_games=dim(data_by_sets)[1],
                          n_teams=length(levels(data_by_sets$home_Team)),
                          X_home=X_home_std,X_away=X_away_std,
                          K_home=ncol(X_home_std),
                          K_away=ncol(X_away_std),
                          home_sets=data_by_sets$home_sets,
                          away_sets=data_by_sets$away_sets)
# Datalist2: Incorporates the model formulations 2, 3,5
data_skills_differences<-list(c_thres=2,c_std=5,
                              n_games=dim(data_by_sets)[1],
                              n_teams=length(levels(data_by_sets$home_Team)),
                              X_home_diff=X_home_diff_std,
                              X_away_diff=X_away_diff_std,
                              K=ncol(X_home_std),
                              home_sets=data_by_sets$home_sets,
                              away_sets=data_by_sets$away_sets)

# R-stan runs for five different ZDTS with only skills formulations
#-----------

##1) Different betas for different model matrices corresponding to
## home and away teams skill actions, respectively (Run Model1.stan)

Model1<-stan("Model1.stan",
        data=data_separate_skills,chains=2,
                               init_r=0.5,
                               iter=5000,warmup=1000,cores=2,seed="1234")### R

save(Model1,file="Model1")

##2) Different betas for Differences between home and away skill actions corresponding to
## , respectively (Run Model1.stan)

Model2<-stan("Model2.stan",
        data=data_skills_differences,chains=2,
        init_r=0.5,
        iter=10000,warmup=1000,cores=2,seed="1234")### Bulk Effective Samples Size (ESS) is too low,

save(Model2,file="Model2")
##3) Same betas for Differences between home and away skill actions corresponding to
## , respectively (Run Model1.stan)

Model3<-stan("Model3.stan",
    data=data_skills_differences,chains=2,
    init_r=0.5,
    iter=5000,warmup=1000,cores=2,seed="1234")### R

save(Model3,file="Model3")

##4) 
model4<-stan("Model4.stan",
                   data=data_separate_skills,chains=2,
             init_r=0.5,
             iter=10000,warmup=1000,cores=2,seed="1234")### Bulk Effective Samples Size (ESS) is too low,

save(model4,file="model4")

##4) 
model5<-stan("Model5.stan",
             data=data_skills_differences,chains=2,
             init_r=0.5,
             iter=10000,warmup=1000,cores=2,seed="1234")### Bulk Effective Samples Size (ESS) is too low,

save(model5,file="model5")
#-----------

###Predictive Model Performance Evaluation

#--------------
# Calculation of the DIC (Gelman,2004)
DIC_Gelman<-function(dev){
  res<-mean(dev)+0.5*var(dev)
  return(res)
}
#Extraction of several posterior quantities (deviances, log-likelihoods)

deviance_Model1<-rstan::extract(Model1,pars="dev")$dev
log_lik_Model1<- extract_log_lik(Model1)
r_eff_log_lik_Model1<- relative_eff(exp(log_lik_Model1),
                                                       chain_id=rep(1:2,each=4000))


deviance_Model2<-rstan::extract(Model2,pars="dev")$dev
log_lik_Model2<- extract_log_lik(Model2)
r_eff_log_lik_Model2<- relative_eff(exp(log_lik_Model2),
                       chain_id=rep(1:2,each=4000))

deviance_Model3<-rstan::extract(Model3,pars="dev")$dev
log_lik_Model3<- extract_log_lik(Model3)
r_eff_log_lik_Model3<- relative_eff(exp(log_lik_Model3),
                       chain_id=rep(1:2,each=4000))

deviance_model4<-rstan::extract(model4,pars="dev")$dev
log_lik_model4<- extract_log_lik(model4)
r_eff_log_lik_model4<- relative_eff(exp(log_lik_model4),
        chain_id=rep(1:2,each=4000))

deviance_model5<-rstan::extract(model5,pars="dev")$dev
log_lik_model5<- extract_log_lik(model5)
r_eff_log_lik_model5<- relative_eff(exp(log_lik_model5),
                                    chain_id=rep(1:2,each=4000))
#WAIC, DIC, LOOIC Bayesian Information Criteria for the ordered logistic with only skills as covariates

waic1<-waic(log_lik_Model1)#### 248.0 
waic2<-waic(log_lik_Model2)####  298.7
waic3<-waic(log_lik_Model3)####  257.9
waic4<-waic(log_lik_model4)####   333.3
waic5<-waic(log_lik_model5)####   299.6

compare(waic1,waic2,waic3,waic4,waic5)


DIC_Gelman(deviance_Model1)# 252.5
DIC_Gelman(deviance_Model2)# 301.4
DIC_Gelman(deviance_Model3)#261.3
DIC_Gelman(deviance_model4)# 395.67
DIC_Gelman(deviance_model5)#302.44
#--------------
