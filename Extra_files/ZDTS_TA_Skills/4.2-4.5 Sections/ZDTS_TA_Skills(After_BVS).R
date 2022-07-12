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
library(ggmcmc)
# Choose the working directory of this file (...\\Submitted_Appendix\\Ordered\\)

#---Data Preparation
source(file.choose())#-Data_Preparation.R

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

final_X_home_std<-X_home_std[,colnames(X_home_std)%in%c( "Home_poor_passes","Home_failed_passes",
                                                        "Home_blocked_att1")]

final_X_away_std<-X_away_std[,colnames(X_away_std)%in%c("Away_failed_serves","Away_poor_passes",
                                                        "Away_failed_passes",
                                                        "Away_blocked_att1","Away_failed_att1",
                                                        "Away_net_violation_blocks","Away_failed_blocks")]


## Vector of teams names along with
## their ranking positions, points, abilities
teams <- teams <- c("Ethnikos Alexandroupolis", "Foinikas Syrou", "Iraklis Chalkidas",       
                    "Iraklis Petosfairishs" ,"Kifisia", "Kyzikos Peramou" ,        
                    "Olympiacos" ,"Orestiada" ,"Pamvochaikos" ,           
                    "Panachaiki",  "Panathinaikos","Paok") 
observed_positions<-c("(7)","(6)","(9)","(8)","(5)","(11)","(1)","(12)","(4)","(10)","(3)","(2)")
observed_points<-c("(36)","(37)","(16)","(28)","(38)","(14)","(62)","(7)","(39)","(16)","(50)","(53)")


  teams_attack<-paste0(teams," ","Attack")
  teams_defense<-paste0(teams," ","Defense")
  teams_over<-paste0(teams," ","Overall")
  
  teams_pos<-paste0(teams," ",observed_positions)
  teams_points<-paste0(teams," ",observed_points)



#----Rename properly the skill variables



#---Datalist required for the ZDTS with TA+Skills

data_zdts_ta_skills<-list(c_thres=2,c_std=5,
                            n_games=dim(data_by_sets)[1],
                            n_teams=length(levels(data_by_sets$home_Team)),
                            X_home=final_X_home_std,X_away=final_X_away_std,
                            K_home=ncol(final_X_home_std),
                            K_away=ncol(final_X_away_std),
                            home_sets=data_by_sets$home_sets,
                            away_sets=data_by_sets$away_sets,home_team=as.numeric(data_by_sets$home_Team),
                            away_team=as.numeric(data_by_sets$away_Team))

#----Set working directory
setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_TA_Skills/4.2-4.5 Sections")
ZDTS_TA_Skills_after_BVS<-stan("ZDTS_TA_Skills_after_BVS.stan",
                               data=data_zdts_ta_skills,chains=2,
                               init_r=0.5,
                               iter=12000,warmup=2000)### R

save(ZDTS_TA_Skills_after_BVS,file="ZDTS_TA_Skills_after_BVS")

###--------------Predictive Model Performance Evaluation-------------------------########
# Calculation of the DIC (Gelman,2004)
DIC_Gelman<-function(dev){
  res<-mean(dev)+0.5*var(dev)
  return(res)
}
###----Extraction of several posterior quantities (deviances, log-likelihoods)

deviance_ZDTS_TA_Skills_after_BVS<-rstan::extract(ZDTS_TA_Skills_after_BVS,pars="dev")$dev
log_lik_ZDTS_TA_Skills_after_BVS<- extract_log_lik(ZDTS_TA_Skills_after_BVS)
r_eff_log_lik_ZDTS_TA_Skills_after_BVS<- relative_eff(exp(log_lik_ZDTS_TA_Skills_after_BVS),chain_id=rep(1:2,each=10000))

# ---WAIC, DIC, LOOIC Bayesian Information Criteria for the ordered logistic with only skills as covariates

waic(log_lik_ZDTS_TA_Skills_after_BVS)####270.7 (20.7)
loo(log_lik_ZDTS_TA_Skills_after_BVS,
    r_eff=r_eff_log_lik_ZDTS_TA_Skills_after_BVS)# 274.3 (21.1)
DIC_Gelman(deviance_ZDTS_TA_Skills_after_BVS)#269.1

###--------------Posterior Summary Statistics-Analysis------------------------########

#####----------------------Posterior summary----------------------------######

names(ZDTS_TA_Skills_after_BVS)[35:46]<-c(teams_attack)[1:12]
names(ZDTS_TA_Skills_after_BVS)[47:58]<-c(teams_defense)[1:12]
names(ZDTS_TA_Skills_after_BVS)[719:730]<-c(teams_over)[1:12]
names(ZDTS_TA_Skills_after_BVS)[1:3]<-c( "(Home) poor passes",
                                         "(Home) failed passes",
                                         "(Home) blocked att1")
names(ZDTS_TA_Skills_after_BVS)[4:10]<-c("(Away) failed serves",
                                         "(Home) poor passes",
                                         "(Away) failed passes",
                                         "(Away) blocked att1",
                                         "(Away) failed att1",
                                         "(Away) block net violations",
                                         "(Away) failed blocks" )


print(ZDTS_TA_Skills_after_BVS,
      pars=c("mu","home","attack","defense","overall",
             "beta_home","beta_away","dev"),probs = c(0.025,0.5,0.975), digits=2)
