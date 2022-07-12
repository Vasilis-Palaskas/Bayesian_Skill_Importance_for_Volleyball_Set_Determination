######---------------------------------------------------------------------------- 
######---------- SECTION 4.4: Revised ZDTS model with only skills (Model estimation after  BVS)
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
library(dplyr)
# Choose the working directory of this file (...\\Submitted_Appendix\\Ordered\\)

#---Data Preparation
source("C:\\Users\\vasileios palaskas\\Desktop\\Github folder\\Bayesian_Variable_Selection_Volleyball\\Extra_Data_Processes\\1-Data_Preparation\\Data_Preparation.R")#-Extra_Data_Processes/Data_Preparation.R
source("C:\\Users\\vasileios palaskas\\Desktop\\Github folder\\Bayesian_Variable_Selection_Volleyball\\Extra_Data_Processes\\1-Data_Preparation\\Athletes_Data_Preparation.R")#-Extra_Data_Processes/Athletes_Data_Preparation.R

# Choose the working directory of this file (...\\Submitted_Appendix\\ZDTS\\)
# setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_TA_Skills")

#------Skills for both Home and Away Teams in team-level per match

X_home<-data_by_sets%>%dplyr::select(Home_perfect_serves,Home_very_good_serves,
                 Home_failed_serves,Home_perfect_passes,Home_very_good_passes,
                 Home_poor_passes,Home_failed_passes,Home_perfect_att1,
                 Home_blocked_att1,Home_failed_att1,Home_perfect_att2,
                 Home_blocked_att2,Home_failed_att2,Home_perfect_blocks,
                 Home_net_violation_blocks,Home_failed_blocks,Home_failed_settings
    )
X_away<-data_by_sets%>%dplyr::select(  Away_perfect_serves,Away_very_good_serves,
                                       Away_failed_serves,Away_perfect_passes,Away_very_good_passes,
                                       Away_poor_passes,Away_failed_passes,Away_perfect_att1,
                                       Away_blocked_att1,Away_failed_att1,Away_perfect_att2,
                                       Away_blocked_att2,Away_failed_att2,Away_perfect_blocks,
                                       Away_net_violation_blocks,Away_failed_blocks,Away_failed_settings
                                    )


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
colnames(X_home_std)<-paste0(colnames(X_home_std),"_","std")
colnames(X_away_std)<-paste0(colnames(X_away_std),"_","std")
colnames(X_away_diff_std)<-paste0(colnames(X_away_diff_std),"_","std")
#--Selected covariates via the Gibbs BVS Method

final_X_home_std<-X_home_std%>%dplyr::select(Home_failed_serves_std,Home_poor_passes_std,
                                             Home_failed_passes_std,Home_blocked_att1_std,
                                             Home_failed_att1_std
                                            )
final_X_away_std<-X_away_std%>%dplyr::select(Away_failed_serves_std,
                                             Away_failed_passes_std,
                                             Away_blocked_att1_std,Away_failed_att1_std,
                                             Away_perfect_att2_std,Away_failed_att2_std,
                                             Away_net_violation_blocks_std,Away_failed_blocks_std
                                             )

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
# setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_Skills_Revised/Sections 4.4-4.5")## Run ZDTS_only_Skills_after_BVS.stan
# 
# ZDTS_only_Skills_after_BVS<-stan("ZDTS_Skills_after_BVS.stan",
#                                  data=data_zdts_only_skills,chains=2,
#                                  init_r=0.5,
#                                  iter=12000,
#                                  warmup=2000)### R
# 
# save(ZDTS_only_Skills_after_BVS,file="revised_ZDTS_only_Skills_after_BVS")

###--------------Predictive Model Performance Evaluation-------------------------########
setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_Skills_Revised/Sections 4.4-4.5")## Run ZDTS_only_Skills_after_BVS.stan
load("C:\\Users\\vasileios palaskas\\Dropbox\\Egidi_Ntzoufras_Palaskas_Drikos_BVS1 Paper\\Models_Outputs_4_5_sections\\ZDTS\\revised\\Skills\\revised_ZDTS_only_Skills_after_BVS")#"ZDTS_only_Skills_after_BVS"

# Calculation of the DIC (Gelman,2004)
DIC_Gelman<-function(dev){
  res<-mean(dev)+0.5*var(dev)
  return(res)
}
###----Extraction of several posterior quantities (deviances, log-likelihoods)



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
betas_summary <- summary(ZDTS_only_Skills_after_BVS, pars = c("mu","home","beta_home","beta_away"), 
                         probs = c(0.025, 0.5,0.95))$summary
print(round(betas_summary[,c(1,3,4,5,6)],2))
betas_summary_main<-round(betas_summary[,c(1,3,4,5,6)],2)
xtable(betas_summary_main)

##### Extraction of model parameters
params_final_ZDTS_paper.v1<-rstan::extract(ZDTS_only_Skills_after_BVS)
dim(params_final_ZDTS_paper.v1$lambda1)
mu_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$mu
home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$home
beta_home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$beta_home
beta_away_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$beta_away
lambda1_star_home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$lambda1_star
lambda2_star_away_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$lambda2_star
#---average of MCMC fitted model'sparameters
avg_mu_params_final_ZDTS_paper.v1<-apply(mu_params_final_ZDTS_paper.v1,1,mean)
avg_home_params_final_ZDTS_paper.v1<-apply(home_params_final_ZDTS_paper.v1,1,mean)
avg_beta_home_params_final_ZDTS_paper.v1<-apply(beta_home_params_final_ZDTS_paper.v1,2,mean)
avg_beta_away_params_final_ZDTS_paper.v1<-apply(beta_away_params_final_ZDTS_paper.v1,2,mean)

avg_lambda1_star_home_params_final_ZDTS_paper.v1<-apply(lambda1_star_home_params_final_ZDTS_paper.v1,2,mean)
avg_lambda2_star_away_params_final_ZDTS_paper.v1<-apply(lambda2_star_away_params_final_ZDTS_paper.v1,2,mean)


median_lambda1_star_home_params_final_ZDTS_paper.v1<-apply(lambda1_star_home_params_final_ZDTS_paper.v1,2,median)
median_lambda2_star_away_params_final_ZDTS_paper.v1<-apply(lambda2_star_away_params_final_ZDTS_paper.v1,2,median)
#--- Visualize both average and median l1, l2 per regular season match
# boxplot(avg_lambda1_star_home_params_final_ZDTS_paper.v1,
#         main="Boxplot of the Posterior mean of lambda1")
# temp <-boxplot(avg_lambda1_star_home_params_final_ZDTS_paper.v1,main="Boxplot of the Posterior mean of lambda1")
# 
# id<-which(avg_lambda1_star_home_params_final_ZDTS_paper.v1%in% temp$out)
# text( rep(1.05,length(id)), temp$out, id,
#       cex=0.5 )

par(mfrow=c(2,2))
boxplot(avg_lambda1_star_home_params_final_ZDTS_paper.v1,main="Boxplot of the Posterior mean of lambda1")
boxplot(avg_lambda1_star_home_params_final_ZDTS_paper.v1,main="Boxplot of the Posterior mean of lambda2")
boxplot(median_lambda1_star_home_params_final_ZDTS_paper.v1,main="Boxplot of the Posterior median of lambda1")
boxplot(median_lambda2_star_away_params_final_ZDTS_paper.v1,main="Boxplot of the Posterior median of lambda2")

#------ Create coefficients columns (Home, Away and both l1, l2 star) by using as point estimates their average ones
data_by_sets$Home_failed_serves_coef<-avg_beta_home_params_final_ZDTS_paper.v1[1]
data_by_sets$Home_poor_passes_coef<-avg_beta_home_params_final_ZDTS_paper.v1[2]
data_by_sets$Home_failed_passes_coef<-avg_beta_home_params_final_ZDTS_paper.v1[3]
data_by_sets$Home_blocked_att1_coef<-avg_beta_home_params_final_ZDTS_paper.v1[4]
data_by_sets$Home_failed_att1_coef<-avg_beta_home_params_final_ZDTS_paper.v1[5]

data_by_sets$Away_failed_serves_coef<-avg_beta_away_params_final_ZDTS_paper.v1[1]
data_by_sets$Away_failed_passes_coef<-avg_beta_away_params_final_ZDTS_paper.v1[2]
data_by_sets$Away_blocked_att1_coef<-avg_beta_away_params_final_ZDTS_paper.v1[3]
data_by_sets$Away_failed_att1_coef<-avg_beta_away_params_final_ZDTS_paper.v1[4]
data_by_sets$Away_perfect_att2_coef<-avg_beta_away_params_final_ZDTS_paper.v1[5]
data_by_sets$Away_failed_att2_coef<-avg_beta_away_params_final_ZDTS_paper.v1[6]
data_by_sets$Away_blocked_net_violation_coef<-avg_beta_away_params_final_ZDTS_paper.v1[7]
data_by_sets$Away_failed_blocks_coef<-avg_beta_away_params_final_ZDTS_paper.v1[8]


#--Lambda 1, 2 star by using as point estimates their average ones
data_by_sets$avg_lambda1_star<-avg_lambda1_star_home_params_final_ZDTS_paper.v1
data_by_sets$avg_lambda2_star<-avg_lambda2_star_away_params_final_ZDTS_paper.v1

data_by_sets$median_lambda1_star<-median_lambda1_star_home_params_final_ZDTS_paper.v1
data_by_sets$median_lambda2_star<-median_lambda2_star_away_params_final_ZDTS_paper.v1

summary()
#-- Additional to the team-statistics, also include standardized selected covariates-statistics (in order to join later also player's 
#-- statistics and make calculations between team and athletes' based statistics)
data_by_sets<-cbind(data_by_sets,final_X_home_std,final_X_away_std)
head(data_by_sets)
#----Join those athletes' data to the match data (each match is a row)
volley_athletes_per_game_statistics_home_team<-volley_athletes_per_game_statistics
colnames(volley_athletes_per_game_statistics_home_team)[1]<-"home_Team"
volley_athletes_per_game_statistics_away_team<-volley_athletes_per_game_statistics
colnames(volley_athletes_per_game_statistics_away_team)[1]<-"away_Team"
head(volley_athletes_per_game_statistics_home_team)

#---Join them for both home and away cases of their own-In essence, for each competing team we join its athletes and their executions
data_by_sets_athletes<-left_join(data_by_sets,volley_athletes_per_game_statistics_home_team,by="home_Team")
data_by_sets_athletes<-left_join(data_by_sets_athletes,volley_athletes_per_game_statistics_away_team,by="away_Team")
head(data_by_sets_athletes)
#---Rename properly the skill of actions of athletes to identify which 
#---skills are from their home and which ones from their away games.
colnames(data_by_sets_athletes)[colnames(data_by_sets_athletes)%in%c("Player.x","Position.x",
                                                                     "total_serves.x","failed_serves.x",
                                                                     "very_good_serves.x","perfect_serves.x",
                                                                     "total_passes.x","failed_passes.x",
                                                                     "poor_passes.x","very_good_passes.x",
                                                                     "perfect_passes.x","total_att1.x",
                                                                     "failed_att1.x","blocked_att1.x",
                                                                     "perfect_att1.x", "total_att2.x",
                                                                     "failed_att2.x","blocked_att2.x",
                                                                     "perfect_att2.x","total_blocks.x",
                                                                     "failed_blocks.x","net_violation_blocks.x",
                                                                     "perfect_blocks.x","total_settings.x",
                                                                     "failed_settings.x")]<-paste0("Home",
                                                                                                   "_",c("Player.x","Position.x",
                                                                                                         "total_serves.x","failed_serves.x",
                                                                                                         "very_good_serves.x","perfect_serves.x",
                                                                                                         "total_passes.x","failed_passes.x",
                                                                                                         "poor_passes.x","very_good_passes.x",
                                                                                                         "perfect_passes.x","total_att1.x",
                                                                                                         "failed_att1.x","blocked_att1.x",
                                                                                                         "perfect_att1.x", "total_att2.x",
                                                                                                         "failed_att2.x","blocked_att2.x",
                                                                                                         "perfect_att2.x","total_blocks.x",
                                                                                                         "failed_blocks.x","net_violation_blocks.x",
                                                                                                         "perfect_blocks.x","total_settings.x",
                                                                                                         "failed_settings.x") )

#---Rename properly the skill of actions of athletes
colnames(data_by_sets_athletes)[colnames(data_by_sets_athletes)%in%c("Player.y","Position.y",
                                                                     "total_serves.y","failed_serves.y",
                                                                     "very_good_serves.y","perfect_serves.y",
                                                                     "total_passes.y","failed_passes.y",
                                                                     "poor_passes.y","very_good_passes.y",
                                                                     "perfect_passes.y","total_att1.y",
                                                                     "failed_att1.y","blocked_att1.y",
                                                                     "perfect_att1.y", "total_att2.y",
                                                                     "failed_att2.y","blocked_att2.y",
                                                                     "perfect_att2.y","total_blocks.y",
                                                                     "failed_blocks.y","net_violation_blocks.y",
                                                                     "perfect_blocks.y","total_settings.y","failed_settings.y")]<-paste0(
                                                                       "Away",
                                                                       "_",c("Player.y","Position.y",
                                                                             "total_serves.y","failed_serves.y",
                                                                             "very_good_serves.y","perfect_serves.y",
                                                                             "total_passes.y","failed_passes.y",
                                                                             "poor_passes.y","very_good_passes.y",
                                                                             "perfect_passes.y","total_att1.y",
                                                                             "failed_att1.y","blocked_att1.y",
                                                                             "perfect_att1.y", "total_att2.y",
                                                                             "failed_att2.y","blocked_att2.y",
                                                                             "perfect_att2.y","total_blocks.y",
                                                                             "failed_blocks.y","net_violation_blocks.y",
                                                                             "perfect_blocks.y","total_settings.y","failed_settings.y")
                                                                     )

#----Create a dataframe with differences among team and its own team's athletes skill executions
data_by_sets_athletes_actions_differences<-data_by_sets_athletes


#--Short overview of the data

head(data_by_sets_athletes_actions_differences)


#----Separate the unique home team's athletes along with their statistics from the unique away ones.


home_athletes_skills_coefs<-c("home_Team",paste0("Home_",c("Player.x","Position.x",
                                                           "failed_serves.x",
                                                           "poor_passes.x","failed_passes.x",
                                                           "blocked_att1.x","failed_att1.x",
                                                           "failed_serves_coef",
                                                           "poor_passes_coef","failed_passes_coef",
                                                           "blocked_att1_coef","failed_att1_coef")),
                              "avg_lambda1_star","median_lambda1_star","away_Team"
)

away_athletes_skills_coefs<-c("away_Team",paste0("Away_",c("Player.y","Position.y",
                                                           "failed_serves.y","failed_passes.y",
                                                           "blocked_att1.y","failed_att1.y",
                                                           "perfect_att2.y", "failed_att2.y",
                                                           "net_violation_blocks.y","failed_blocks.y",
                                                           "failed_serves_coef","failed_passes_coef",
                                                           "blocked_att1_coef","failed_att1_coef",
                                                           "perfect_att2_coef", "failed_att2_coef",
                                                           "blocked_net_violation_coef","failed_blocks_coef")),
                                                  "avg_lambda2_star","median_lambda2_star",
                              "home_Team" )
#--Select aboce columns
home_data_by_sets_athletes<-data_by_sets_athletes%>%select(all_of(home_athletes_skills_coefs))%>%as.data.frame()
away_data_by_sets_athletes<-data_by_sets_athletes%>%select(all_of(away_athletes_skills_coefs))%>%as.data.frame()

head(home_data_by_sets_athletes)
head(away_data_by_sets_athletes)
#---Keep distinct rows: Each row represents athletes' statistics per match for both his home and away matches, respectively
home_data_by_sets_athletes_distinct<-home_data_by_sets_athletes%>%distinct()
away_data_by_sets_athletes_distinct<-away_data_by_sets_athletes%>%distinct()
head(home_data_by_sets_athletes_distinct)

#example case: Mouclias all home games and away games
home_data_by_sets_athletes_distinct[home_data_by_sets_athletes_distinct$Home_Player.x%in%
                                      home_data_by_sets_athletes_distinct$Home_Player.x[1],]
away_data_by_sets_athletes_distinct[away_data_by_sets_athletes_distinct$Away_Player.y%in%
                                      home_data_by_sets_athletes_distinct$Home_Player.x[1],]
#---calculate the coefficients*skill actions per athlete per match after standardization
dim(home_data_by_sets_athletes)
dim(home_data_by_sets_athletes_distinct)#188 unique athletes*11
dim(away_data_by_sets_athletes)
dim(away_data_by_sets_athletes_distinct)#188 unique athletes*11

length(unique(home_data_by_sets_athletes_distinct$Home_Player.x))#187
length(unique(away_data_by_sets_athletes_distinct$Away_Player.y))#187

#--- Standardization of home athletes' skills based on their teams' avg, sd of specific team-level skills 
#--- in terms of consistency with the standardization already implemented for specific team-level skills.



home_skills_std<-NULL#---Initialization of std data frame
#----Standardization
home_skills_std$Home_failed_serves.x_std<-(home_data_by_sets_athletes_distinct$Home_failed_serves.x-mean(home_data_by_sets_athletes_distinct$Home_failed_serves.x))/
  sd(X_home$Home_failed_serves)
home_skills_std$Home_poor_passes.x_std<-(home_data_by_sets_athletes_distinct$Home_poor_passes.x-mean(home_data_by_sets_athletes_distinct$Home_poor_passes.x))/
  sd(X_home$Home_poor_passes)
home_skills_std$Home_failed_passes.x_std<-(home_data_by_sets_athletes_distinct$Home_failed_passes.x-mean(home_data_by_sets_athletes_distinct$Home_failed_passes.x) )/
  sd(X_home$Home_failed_passes)
home_skills_std$Home_blocked_att1.x_std<-(home_data_by_sets_athletes_distinct$Home_blocked_att1.x-mean(home_data_by_sets_athletes_distinct$Home_blocked_att1.x))/
  sd(X_home$Home_blocked_att1)
home_skills_std$Home_failed_att1.x_std<-(home_data_by_sets_athletes_distinct$Home_failed_att1.x-mean(home_data_by_sets_athletes_distinct$Home_failed_att1.x))/
  sd(X_home$Home_failed_att1)
#- Convert it to a dataframe
home_skills_std<-data.frame(Home_failed_serves.x_std=home_skills_std$Home_failed_serves.x_std,
                            Home_poor_passes.x_std=home_skills_std$Home_poor_passes.x_std,
                            Home_failed_passes.x_std= home_skills_std$Home_failed_passes.x_std,
                            Home_blocked_att1.x_std=home_skills_std$Home_blocked_att1.x_std,
                            Home_failed_att1.x_std=home_skills_std$Home_failed_att1.x_std)

#--Combine both raw and std skills of athletes.
home_data_by_sets_athletes_distinct_based_std<-cbind(home_data_by_sets_athletes_distinct,home_skills_std)
home_data_by_sets_athletes_distinct_based_std<-as.data.frame(home_data_by_sets_athletes_distinct_based_std)



#--- Standardization of away athletes' skills based on their teams' avg, sd of specific team-level skills 
#--- in terms of consistency with the standardization already implemented for specific team-level skills.


away_skills_std<-NULL#---Initialisation of std data frame
#----Standardisation

away_skills_std$Away_failed_serves.y_std<-(away_data_by_sets_athletes_distinct$Away_failed_serves.y-mean(away_data_by_sets_athletes_distinct$Away_failed_serves.y))/
  sd(X_away$Away_failed_serves)

away_skills_std$Away_failed_passes.y_std<-(away_data_by_sets_athletes_distinct$Away_failed_passes.y-mean(away_data_by_sets_athletes_distinct$Away_failed_passes.y))/
  sd(X_away$Away_failed_passes)

away_skills_std$Away_perfect_att2.y_std<-(away_data_by_sets_athletes_distinct$Away_perfect_att2.y-mean(away_data_by_sets_athletes_distinct$Away_perfect_att2.y))/
  sd(X_away$Away_perfect_att2)

away_skills_std$Away_failed_att2.y_std<-(away_data_by_sets_athletes_distinct$Away_failed_att2.y-mean(away_data_by_sets_athletes_distinct$Away_failed_att2.y))/
  sd(X_away$Away_failed_att2)

away_skills_std$Away_blocked_att1.y_std<-(away_data_by_sets_athletes_distinct$Away_blocked_att1.y-mean(away_data_by_sets_athletes_distinct$Away_blocked_att1.y))/
  sd(X_away$Away_blocked_att1)
away_skills_std$Away_failed_att1.y_std<-(away_data_by_sets_athletes_distinct$Away_failed_att1.y-mean(away_data_by_sets_athletes_distinct$Away_failed_att1.y))/
  sd(X_away$Away_failed_att1)
away_skills_std$Away_net_violation_blocks.y_std<-(away_data_by_sets_athletes_distinct$Away_net_violation_blocks.y-mean(away_data_by_sets_athletes_distinct$Away_net_violation_blocks.y))/
  sd(X_away$Away_net_violation_blocks)
away_skills_std$Away_failed_blocks.y_std<-(away_data_by_sets_athletes_distinct$Away_failed_blocks.y-mean(away_data_by_sets_athletes_distinct$Away_failed_blocks.y))/
  sd(X_away$Away_failed_blocks)
#--Combine both raw and std skills
away_skills_std<-data.frame(Away_failed_serves.y_std=away_skills_std$Away_failed_serves.y_std,
                            Away_failed_passes.y_std=away_skills_std$Away_failed_passes.y_std,
                            Away_perfect_att2.y_std=  away_skills_std$Away_perfect_att2.y_std,
                            Away_failed_att2.y_std=away_skills_std$Away_failed_att2.y_std,
                            Away_blocked_att1.y_std= away_skills_std$Away_blocked_att1.y_std,
                            Away_failed_att1.y_std=away_skills_std$Away_failed_att1.y_std,
                            Away_net_violation_blocks.y_std=away_skills_std$Away_net_violation_blocks.y_std,
                            Away_failed_blocks.y_std=away_skills_std$Away_failed_blocks.y_std)

#--Combine both raw and std skills of athletes.

away_data_by_sets_athletes_distinct_based_std<-cbind(away_data_by_sets_athletes_distinct,away_skills_std)
away_data_by_sets_athletes_distinct_based_std<-as.data.frame(away_data_by_sets_athletes_distinct_based_std)
#---Now lets combine with the original ones
home_data_by_sets_athletes_distinct_both_original_stds<-cbind(home_data_by_sets_athletes_distinct,
                                                              home_data_by_sets_athletes_distinct_based_std)
away_data_by_sets_athletes_distinct_both_original_stds<-cbind(away_data_by_sets_athletes_distinct,
                                                              away_data_by_sets_athletes_distinct_based_std)
colnames(home_data_by_sets_athletes_distinct_based_std)
colnames(away_data_by_sets_athletes_distinct_based_std)

head(home_data_by_sets_athletes_distinct_based_std)
#---Multiply the coefficients with the corresponding columns
colnames(home_data_by_sets_athletes_distinct)
colnames(away_data_by_sets_athletes_distinct)
multiplication_home_skills_coefs_per_player_std<-multiplication_away_skills_coefs_per_player_std<-c()
multiplication_home_skills_coefs_per_player<-multiplication_away_skills_coefs_per_player<-c()

head(home_data_by_sets_athletes_distinct_based_std)
head(home_data_by_sets_athletes_distinct)

head(away_data_by_sets_athletes_distinct_based_std)
head(away_data_by_sets_athletes_distinct)

#-------Home skills multiplied with coefficients

#---Standardised coef
multiplication_home_skills_coefs_per_player_std<-home_data_by_sets_athletes_distinct_based_std$Home_failed_serves_coef*
  home_data_by_sets_athletes_distinct_based_std$Home_failed_serves.x_std+
  home_data_by_sets_athletes_distinct_based_std$Home_poor_passes_coef*
  home_data_by_sets_athletes_distinct_based_std$Home_poor_passes.x_std+
  home_data_by_sets_athletes_distinct_based_std$Home_failed_passes_coef*
  home_data_by_sets_athletes_distinct_based_std$Home_failed_passes.x_std+
  home_data_by_sets_athletes_distinct_based_std$Home_blocked_att1_coef*
  home_data_by_sets_athletes_distinct_based_std$Home_blocked_att1.x_std+
  home_data_by_sets_athletes_distinct_based_std$Home_failed_att1_coef*
  home_data_by_sets_athletes_distinct_based_std$Home_failed_att1.x_std


#-------Away skills multiplied with coefficients

# 
# away_skills_std<-data.frame(Away_failed_serves.y_std=away_skills_std$Away_failed_serves.y_std,
#                             Away_failed_passes.y_std=away_skills_std$Away_failed_passes.y_std,
#                             Away_perfect_att2.y_std=  away_skills_std$Away_perfect_att2.y_std,
#                             Away_failed_att2.y_std=away_skills_std$Away_failed_att2.y_std,
#                             Away_blocked_att1.y_std= away_skills_std$Away_blocked_att1.y_std,
#                             Away_failed_att1.y_std=away_skills_std$Away_failed_att1.y_std,
#                             Away_net_violation_blocks.y_std=away_skills_std$Away_net_violation_blocks.y_std,
#                             Away_failed_blocks.y_std=away_skills_std$Away_failed_blocks.y_std)
# 
#---Standardised coef

multiplication_away_skills_coefs_per_player_std<-
  away_data_by_sets_athletes_distinct$Away_failed_serves_coef*
    away_data_by_sets_athletes_distinct_based_std$Away_failed_serves.y_std+
    away_data_by_sets_athletes_distinct$Away_failed_passes_coef*
    away_data_by_sets_athletes_distinct_based_std$Away_failed_passes.y_std+
    away_data_by_sets_athletes_distinct$Away_blocked_att1_coef*
    away_data_by_sets_athletes_distinct_based_std$Away_blocked_att1.y_std+
    away_data_by_sets_athletes_distinct$Away_failed_att1_coef*
    away_data_by_sets_athletes_distinct_based_std$Away_failed_att1.y_std+
    away_data_by_sets_athletes_distinct$Away_perfect_att2_coef*
    away_data_by_sets_athletes_distinct_based_std$Away_perfect_att2.y_std+
    away_data_by_sets_athletes_distinct$Away_failed_att2_coef*
    away_data_by_sets_athletes_distinct_based_std$Away_failed_att2.y_std+
    away_data_by_sets_athletes_distinct$Away_blocked_net_violation_coef*
    away_data_by_sets_athletes_distinct_based_std$Away_net_violation_blocks.y_std+
    away_data_by_sets_athletes_distinct$Away_failed_blocks_coef*
    away_data_by_sets_athletes_distinct_based_std$Away_failed_blocks.y_std
#--- Non Standardised coef

#---- Combine both home and away Players' skills (raw and std per player for their home and away matches) as well as models' coefficients
#---- along with their calculated impact [log(l1)-log(l2)]-[log(l1*)-log(l2*)] where l1*, l2* are their predictors without athletes ' std skills
#---- (by substracting their athletes' std skills)
home_data_by_sets_athletes_distinct_skills_coef_multiplication<-cbind(home_data_by_sets_athletes_distinct_both_original_stds,
                                                                      multiplication_home_skills_coefs_per_player_std)
away_data_by_sets_athletes_distinct_skills_coef_multiplication<-cbind(away_data_by_sets_athletes_distinct_both_original_stds,
                                                                      multiplication_away_skills_coefs_per_player_std)

head(home_data_by_sets_athletes_distinct_skills_coef_multiplication[,colnames(home_data_by_sets_athletes_distinct_skills_coef_multiplication)%in%
                                                                      c(paste0("Home_",
                                                                                                c("failed_serves.x",
                                                                                                    "poor_passes.x","failed_passes.x",
                                                                                                      "blocked_att1.x","failed_att1.x")),
                                                                        paste0("Home_",
                                                                               c("failed_serves.x",
                                                                                 "poor_passes.x","failed_passes.x",
                                                                                 "blocked_att1.x","failed_att1.x"),"_std"),
                                                                        "multiplication_home_skills_coefs_per_player_std")])

head(away_data_by_sets_athletes_distinct_skills_coef_multiplication[,colnames(away_data_by_sets_athletes_distinct_skills_coef_multiplication)%in%
                                                                      c(paste0("Away_",c("failed_serves.y","failed_passes.y",
                                                                                                    "blocked_att1.y","failed_att1.y",
                                                                                                     "perfect_att2.y", "failed_att2.y",
                                                                                                      "net_violation_blocks.y","failed_blocks.y")),
                                                                        paste0("Away_",c("failed_serves.y","failed_passes.y",
                                                                                         "blocked_att1.y","failed_att1.y",
                                                                                         "perfect_att2.y", "failed_att2.y",
                                                                                         "net_violation_blocks.y","failed_blocks.y"),"_std"),
                                                                        "multiplication_away_skills_coefs_per_player_std")])#---Calculate the difference between (l1-l2)-(l1*-l2*) where l1*,l2* are when athletes are absent from the total skills. 


#This is for both log and exponential scale (former is more interpreatable)


#---Log Scale
home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_log_scale_std<-multiplication_home_skills_coefs_per_player_std
summary(home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_log_scale_std)
# home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_log_scale<-multiplication_home_skills_coefs_per_player
home_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_log_scale_std<-multiplication_home_skills_coefs_per_player_std
summary(home_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_log_scale_std)


away_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_log_scale_std<-multiplication_away_skills_coefs_per_player_std
summary(away_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_log_scale_std)

# away_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_log_scale<-multiplication_away_skills_coefs_per_player
away_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_log_scale_std<--multiplication_away_skills_coefs_per_player_std
summary(away_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_log_scale_std)

#---Exp Scale
## home
home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std<- (
                                              1-exp(-multiplication_home_skills_coefs_per_player_std) )
home_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_exp_scale_std<-home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std

home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std_with_team_impact<-home_data_by_sets_athletes_distinct_skills_coef_multiplication$avg_lambda1_star* (
  1-exp(-multiplication_home_skills_coefs_per_player_std) )
home_data_by_sets_athletes_distinct_skills_coef_multiplication$median_impact_athletes_absence_exp_scale_std_with_team_impact<-home_data_by_sets_athletes_distinct_skills_coef_multiplication$median_lambda1_star* (
  1-exp(-multiplication_home_skills_coefs_per_player_std) )

summary(home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std)
summary(home_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_exp_scale_std)
summary(home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std_with_team_impact)
summary(home_data_by_sets_athletes_distinct_skills_coef_multiplication$median_impact_athletes_absence_exp_scale_std_with_team_impact)

home_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_exp_scale_std_with_team_impact<-home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std_with_team_impact
home_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_median_impact_athletes_absence_exp_scale_std_with_team_impact<-home_data_by_sets_athletes_distinct_skills_coef_multiplication$median_impact_athletes_absence_exp_scale_std_with_team_impact

summary(home_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_exp_scale_std_with_team_impact)
summary(home_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_median_impact_athletes_absence_exp_scale_std_with_team_impact)

# away
away_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std<-(
  1-exp(-multiplication_away_skills_coefs_per_player_std) )
away_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_exp_scale_std<-(
  exp(-multiplication_away_skills_coefs_per_player_std)-1 )


away_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std_with_team_impact<-away_data_by_sets_athletes_distinct_skills_coef_multiplication$avg_lambda2_star*(
  1-exp(-multiplication_away_skills_coefs_per_player_std) )
away_data_by_sets_athletes_distinct_skills_coef_multiplication$median_impact_athletes_absence_exp_scale_std_with_team_impact<-away_data_by_sets_athletes_distinct_skills_coef_multiplication$median_lambda2_star*(
  1-exp(-multiplication_away_skills_coefs_per_player_std) )

summary(away_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std)#prefer median
summary(away_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_exp_scale_std)#prefer median
summary(away_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_exp_scale_std_with_team_impact)#prefer median
summary(away_data_by_sets_athletes_distinct_skills_coef_multiplication$median_impact_athletes_absence_exp_scale_std_with_team_impact)#prefer median


away_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_exp_scale_std_with_team_impact<-away_data_by_sets_athletes_distinct_skills_coef_multiplication$avg_lambda2_star*(
  exp(-multiplication_away_skills_coefs_per_player_std)-1 )
away_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_median_impact_athletes_absence_exp_scale_std_with_team_impact<-away_data_by_sets_athletes_distinct_skills_coef_multiplication$median_lambda2_star*(
  exp(-multiplication_away_skills_coefs_per_player_std)-1 )

summary(away_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_impact_athletes_absence_exp_scale_std_with_team_impact)#prefer median
summary(away_data_by_sets_athletes_distinct_skills_coef_multiplication$inverse_median_impact_athletes_absence_exp_scale_std_with_team_impact)#prefer median

#---Calculate the mean, sd in order to approximate their average performance per Player and ranking them.
#
# Firstly, combine both home and aways dataframes including the impact of athletes in both log and exp scale

#--log scale rbind of two columns related with the impact of absence of athletes as well 
head(home_data_by_sets_athletes_distinct_skills_coef_multiplication)
head(away_data_by_sets_athletes_distinct_skills_coef_multiplication)

#--need to have same columns names to apply rbind and for this reason, we assign to new objects in order to avoid making changes in old objects
home_data_by_sets_athletes_distinct_skills_coef_multiplication_new<-home_data_by_sets_athletes_distinct_skills_coef_multiplication
away_data_by_sets_athletes_distinct_skills_coef_multiplication_new<-away_data_by_sets_athletes_distinct_skills_coef_multiplication

colnames(home_data_by_sets_athletes_distinct_skills_coef_multiplication_new)[colnames(home_data_by_sets_athletes_distinct_skills_coef_multiplication_new)%in%c(
  "home_Team","Home_Player.x","avg_lambda1_star")]<-c("Team","Player","avg_lambda_star")
colnames(away_data_by_sets_athletes_distinct_skills_coef_multiplication_new)[colnames(away_data_by_sets_athletes_distinct_skills_coef_multiplication_new)%in%c(
  "away_Team", "Away_Player.y","avg_lambda2_star")]<-c("Team","Player","avg_lambda_star")

#---Keep specific columns
home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new<-rbind(home_data_by_sets_athletes_distinct_skills_coef_multiplication_new[c(
  "Team","Player",
  "avg_lambda_star",
  "impact_athletes_absence_log_scale_std",
  "inverse_impact_athletes_absence_log_scale_std",
  "impact_athletes_absence_exp_scale_std",
  "inverse_impact_athletes_absence_exp_scale_std",
  "impact_athletes_absence_exp_scale_std_with_team_impact",
  "median_impact_athletes_absence_exp_scale_std_with_team_impact"
  )],
  away_data_by_sets_athletes_distinct_skills_coef_multiplication_new[c(
    "Team","Player",
    "avg_lambda_star",
    "impact_athletes_absence_log_scale_std",
    "inverse_impact_athletes_absence_log_scale_std",
    "impact_athletes_absence_exp_scale_std",
    "inverse_impact_athletes_absence_exp_scale_std",
    "impact_athletes_absence_exp_scale_std_with_team_impact",
    "median_impact_athletes_absence_exp_scale_std_with_team_impact")])
head(home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new)
#---home and away matches of a random athlete
home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new[home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new$Player%in%
                                                                          home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new$Player[1],]

#---Calculate their impact based on their averages

athletes_impact<-home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new %>%
  group_by(Player,Team) %>%   
  summarize(median_of_impact_absence_log_scale = round(median(impact_athletes_absence_log_scale_std, na.rm = T),2),
            median_of_impact_absence_exp_scale = round(median(impact_athletes_absence_exp_scale_std, na.rm = T),5),
            median_of_inverse_impact_absence_log_scale = round(median(inverse_impact_athletes_absence_log_scale_std, na.rm = T),2),
            median_of_inverse_impact_absence_exp_scale = round(median(inverse_impact_athletes_absence_exp_scale_std, na.rm = T),5),
            # with team impact
            median_of_impact_athletes_absence_exp_scale_std_with_team_impact = round(median(impact_athletes_absence_exp_scale_std_with_team_impact, na.rm = T),2),
            median_of_median_impact_athletes_absence_exp_scale_std_with_team_impact = round(median(median_impact_athletes_absence_exp_scale_std_with_team_impact, na.rm = T),2)
  ) %>%
  arrange( desc(median_of_impact_absence_log_scale) )      %>% as.data.frame()      # Specify function
#---------------------------------------------------------------------------------------

###----Athletes above average per Position Analysis by means of using all actions
###---- to calculate their total skill actions
#---Keep in a vector the selected coefficients

#-Rename the dataset to represent whole season statistics
volley_athletes_all_season_statistics<-volley_athletes

revised_zdts_skills<-c("total_serves","failed_serves","very_good_serves","perfect_serves",
                       "total_passes","very_good_passes", "perfect_passes","poor_passes","failed_passes",
                       "total_att1", "blocked_att1","failed_att1","perfect_att1",
                       "total_att2", "blocked_att2","failed_att2","perfect_att2",
                       "total_blocks", "net_violation_blocks","failed_blocks","perfect_blocks",
                       "total_settings", "failed_settings"
)

#---Join along with the impact of athletes-their skill actions total in the season+Position
colnames(volley_athletes_all_season_statistics)[c(4:length(names(volley_athletes_all_season_statistics)))]<-paste0(
  colnames(volley_athletes_all_season_statistics)[c(4:length(names(volley_athletes_all_season_statistics)))],
  "_all_season")


#----COLNAMES TO BE USED FOR DETECTING THE THRESHOLD OF 
revised_zdts_skills_all_season<-paste0(c("total_serves","failed_serves","very_good_serves","perfect_serves",
                                         "total_passes","very_good_passes", "perfect_passes","poor_passes","failed_passes",
                                         "total_att1", "blocked_att1","failed_att1","perfect_att1",
                                         "total_att2", "blocked_att2","failed_att2","perfect_att2",
                                         "total_blocks", "net_violation_blocks","failed_blocks","perfect_blocks",
                                         "total_settings", "failed_settings"
),
"_all_season")

total_revised_zdts_skills_all_season<-c("total_serves_all_season","total_passes_all_season",
                                        "total_att1_all_season","total_att2_all_season",
                                        "total_blocks_all_season","total_settings_all_season")

separate_revised_zdts_skills_all_season<-paste0(c("failed_serves","very_good_serves","perfect_serves",
                                                  "very_good_passes", "perfect_passes","poor_passes","failed_passes",
                                                  "blocked_att1","failed_att1","perfect_att1",
                                                  "blocked_att2","failed_att2","perfect_att2",
                                                  "net_violation_blocks","failed_blocks","perfect_blocks",
                                                  "failed_settings"
),
"_all_season")
#--Join total skill actions
athletes_impact_full_dataset<-left_join(athletes_impact,volley_athletes_all_season_statistics[,
                                                                                              colnames(volley_athletes_all_season_statistics)%in%c("Player","Position",
                                                                                                                                                   revised_zdts_skills_all_season)],
                                        by="Player")
head(athletes_impact_full_dataset)
#---create the sum of all skill actions in a unique variable in order to obtain a criterion variable for filtering out
#--- athletes with low frequency skill actions

athletes_impact_full_dataset$sum_skill_actions_all_season<-apply(athletes_impact_full_dataset[
  total_revised_zdts_skills_all_season],1,sum)
head(athletes_impact_full_dataset)

#---Filter out athletes with less total skills actions than the 50% (median) across all athletes and per Position

#----- and per Position

# Average threshold of impact per Position
avg_top_75_per_Position<-athletes_impact_full_dataset %>% 
  group_by(Position) %>% dplyr::summarize(avg_total_skill_actions = mean(sum_skill_actions_all_season, na.rm=TRUE),
                                          top_75_total_skill_actions=quantile(sum_skill_actions_all_season, na.rm=TRUE,probs = 0.25))  %>% 
  as.data.frame()

# athletes_impact_full_dataset_top_mean_percentage %>% 
#   arrange(desc(avg_impact_absence_log),Position) %>% 
#   group_by(Position) %>% slice(1:5)  %>% as.data.frame()

#--Position S above average skill actions
athletes_impact_full_dataset_top_mean_percentage_S<-athletes_impact_full_dataset[
  (athletes_impact_full_dataset$sum_skill_actions_all_season>=
     avg_top_75_per_Position$top_75_total_skill_actions[avg_top_75_per_Position$Position=="S"] )&
    athletes_impact_full_dataset$Position%in%"S" ,]
#-Arranged players of Position S
athletes_impact_full_dataset_top_mean_percentage_S %>% 
  arrange(desc(median_of_impact_absence_log_scale)) 


athletes_impact_full_dataset_top_mean_percentage_S %>% 
  arrange(desc(median_of_impact_absence_log_scale),Position) %>% 
  group_by(Position) %>% slice(1:5)  %>% as.data.frame()

#--Position H above average skill actions
athletes_impact_full_dataset_top_mean_percentage_H<-athletes_impact_full_dataset[
  (athletes_impact_full_dataset$sum_skill_actions_all_season>=
     avg_top_75_per_Position$top_75_total_skill_actions[avg_top_75_per_Position$Position=="H"] )&
    athletes_impact_full_dataset$Position%in%"H" ,]
#-Arranged players of Position H
athletes_impact_full_dataset_top_mean_percentage_H %>% 
  arrange(desc(median_of_impact_absence_log_scale)) 


athletes_impact_full_dataset_top_mean_percentage_H %>% 
  arrange(desc(median_of_impact_absence_log_scale),Position) %>% 
  group_by(Position) %>% slice(1:5)  %>% as.data.frame()

#--Position L above average skill actions
athletes_impact_full_dataset_top_mean_percentage_L<-athletes_impact_full_dataset[
  (athletes_impact_full_dataset$sum_skill_actions_all_season>=
     avg_top_75_per_Position$top_75_total_skill_actions[avg_top_75_per_Position$Position=="L"] )&
    athletes_impact_full_dataset$Position%in%"L" ,]
#-Arranged players of Position L
athletes_impact_full_dataset_top_mean_percentage_L %>% 
  arrange(desc(median_of_impact_absence_log_scale)) 


athletes_impact_full_dataset_top_mean_percentage_L %>% 
  arrange(desc(median_of_impact_absence_log_scale),Position) %>% 
  group_by(Position) %>% slice(1:5)  %>% as.data.frame()

#--Position M above average skill actions
athletes_impact_full_dataset_top_mean_percentage_M<-athletes_impact_full_dataset[
  (athletes_impact_full_dataset$sum_skill_actions_all_season>=
     avg_top_75_per_Position$top_75_total_skill_actions[avg_top_75_per_Position$Position=="M"] )&
    athletes_impact_full_dataset$Position%in%"M" ,]
#-Arranged players of Position M
athletes_impact_full_dataset_top_mean_percentage_M %>% 
  arrange(desc(median_of_impact_absence_log_scale)) 


athletes_impact_full_dataset_top_mean_percentage_M %>% 
  arrange(desc(median_of_impact_absence_log_scale),Position) %>% 
  group_by(Position) %>% slice(1:5)  %>% as.data.frame()

#--Position 0 above average skill actions
athletes_impact_full_dataset_top_mean_percentage_0<-athletes_impact_full_dataset[
  (athletes_impact_full_dataset$sum_skill_actions_all_season>=
     avg_top_75_per_Position$top_75_total_skill_actions[avg_top_75_per_Position$Position=="0"] )&
    athletes_impact_full_dataset$Position%in%"0" ,]
#-Arranged players of Position 0
athletes_impact_full_dataset_top_mean_percentage_0 %>% 
  arrange(desc(median_of_impact_absence_log_scale)) 


athletes_impact_full_dataset_top_mean_percentage_0 %>% 
  arrange(desc(median_of_impact_absence_log_scale),Position) %>% 
  group_by(Position) %>% slice(1:5)  %>% as.data.frame()


#--Position O above average skill actions
athletes_impact_full_dataset_top_mean_percentage_O<-athletes_impact_full_dataset[
  (athletes_impact_full_dataset$sum_skill_actions_all_season>=
     avg_top_75_per_Position$top_75_total_skill_actions[avg_top_75_per_Position$Position=="O"] )&
    athletes_impact_full_dataset$Position%in%"O" ,]
#-Arranged players of Position O
athletes_impact_full_dataset_top_mean_percentage_O %>% 
  arrange(desc(median_of_impact_absence_log_scale)) 


athletes_impact_full_dataset_top_mean_percentage_O %>% 
  arrange(desc(median_of_impact_absence_log_scale),Position) %>% 
  group_by(Position) %>% slice(1:5)  %>% as.data.frame()
#---Union of the athletes with  total skill actions above the average of his Position's total skill actions
athletes_above_average_per_Position<-rbind(athletes_impact_full_dataset_top_mean_percentage_S %>% 
                                             arrange(desc(median_of_impact_absence_log_scale)) ,
                                           athletes_impact_full_dataset_top_mean_percentage_H %>% 
                                             arrange(desc(median_of_impact_absence_log_scale)) ,
                                           athletes_impact_full_dataset_top_mean_percentage_L %>% 
                                             arrange(desc(median_of_impact_absence_log_scale)) ,
                                           athletes_impact_full_dataset_top_mean_percentage_M %>% 
                                             arrange(desc(median_of_impact_absence_log_scale)),
                                           athletes_impact_full_dataset_top_mean_percentage_0 %>% 
                                             arrange(desc(median_of_impact_absence_log_scale)) ,
                                           athletes_impact_full_dataset_top_mean_percentage_O %>% 
                                             arrange(desc(median_of_impact_absence_log_scale)) )

#--Remove athlete of 0 position (error)
athletes_above_average_per_Position<-athletes_above_average_per_Position[!athletes_above_average_per_Position$Position%in%"0",]
athletes_above_average_per_Position$Position<-factor(athletes_above_average_per_Position$Position)
write.csv(athletes_above_average_per_Position,file="final_best_impact_athletes_2016_2017.csv")


#---------------------------------------------------------------------------------------
# Visualisation of the columns

colnames(athletes_above_average_per_Position)

# Athletes'  Evaluation: log scale
plot1<-ggplot(athletes_above_average_per_Position, aes(x=Position, y=median_of_impact_absence_log_scale, 
                                                                     fill=Position)) + 
  geom_boxplot()  +
  scale_y_continuous(name = "Athletes' Impact \n(Log scale)",
                             limits=c( min(athletes_above_average_per_Position$median_of_impact_absence_log_scale), 
                             max(athletes_above_average_per_Position$median_of_impact_absence_log_scale))
  )+
  scale_x_discrete(name = "Field Position") +
  ggtitle("Boxplot of Athletes' Impact Evaluation by Position (Log scale)")+
  theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        text = element_text(size = 25),
        axis.title = element_text(face="bold",size = 25),
        axis.text.x=element_text(face="bold",size = 23),
        axis.text.y=element_text(face="bold",size = 23)		)+
  theme(legend.position="none")


pdf(file="Boxplot_Volleyball_athletes_per_position.pdf", width =14, height =7.5)

grid.arrange(plot1)
dev.off()

# Athletes'  Evaluation: exp scale
plot2<-ggplot(athletes_above_average_per_Position, aes(x=Position, y=median_of_impact_absence_exp_scale, 
                                                       fill=Position)) + 
  geom_boxplot()  +
  scale_y_continuous(name = "Athletes' Impact \n(Exp. scale)",
                     limits=c( min(athletes_above_average_per_Position$median_of_impact_absence_exp_scale), 
                               max(athletes_above_average_per_Position$median_of_impact_absence_exp_scale))
  )+
  scale_x_discrete(name = "Field Position") +
  ggtitle("Boxplot of Athletes' Impact Evaluation by Position (Exp scale)")+
  theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        text = element_text(size = 25),
        axis.title = element_text(face="bold",size = 25),
        axis.text.x=element_text(face="bold",size = 23),
        axis.text.y=element_text(face="bold",size = 23)		)+
  theme(legend.position="none")


pdf(file="Boxplot_Volleyball_athletes_per_position_exp_scale.pdf", width =14, height =7.5)

grid.arrange(plot2)
dev.off()

# Athletes'  Evaluation+Team's Impact (avg l1,l2: exp scale
plot3<-ggplot(athletes_above_average_per_Position, aes(x=Position, 
                                                       y=median_of_impact_athletes_absence_exp_scale_std_with_team_impact, 
                                                       fill=Position)) + 
  geom_boxplot()  +
  scale_y_continuous(name = "Athletes' Impact \n(Exp. scale)",
                     limits=c( min(athletes_above_average_per_Position$median_of_impact_athletes_absence_exp_scale_std_with_team_impact), 
                               max(athletes_above_average_per_Position$median_of_impact_athletes_absence_exp_scale_std_with_team_impact))
  )+
  scale_x_discrete(name = "Field Position") +
  ggtitle("Boxplot of Athletes' Impact Evaluation along with \nTeam-level performance impact (Post. mean) \nby Position (Exp scale)")+
  theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        text = element_text(size = 25),
        axis.title = element_text(face="bold",size = 25),
        axis.text.x=element_text(face="bold",size = 23),
        axis.text.y=element_text(face="bold",size = 23)		)+
  theme(legend.position="none")


pdf(file="Boxplot_Volleyball_athletes_per_position_exp_scale_with_average_team_impact.pdf", width =14, height =7.5)

grid.arrange(plot3)
dev.off()
# Athletes'  Evaluation+Team's Impact (median l1,l2: exp scale
plot4<-ggplot(athletes_above_average_per_Position, aes(x=Position, 
                                                       y=median_of_median_impact_athletes_absence_exp_scale_std_with_team_impact, 
                                                       fill=Position)) + 
  geom_boxplot()  +
  scale_y_continuous(name = "Athletes' Impact \n(Exp. scale)",
                     limits=c( min(athletes_above_average_per_Position$median_of_median_impact_athletes_absence_exp_scale_std_with_team_impact), 
                               max(athletes_above_average_per_Position$median_of_median_impact_athletes_absence_exp_scale_std_with_team_impact))
  )+
  scale_x_discrete(name = "Field Position") +
  ggtitle("Boxplot of Athletes' Impact Evaluation along with \nTeam-level performance impact (Post. median) \nby Position (Exp scale)")+
  theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        text = element_text(size = 25),
        axis.title = element_text(face="bold",size = 25),
        axis.text.x=element_text(face="bold",size = 23),
        axis.text.y=element_text(face="bold",size = 23)		)+
  theme(legend.position="none")


pdf(file="Boxplot_Volleyball_athletes_per_position_exp_scale_with_median_team_impact.pdf", width =14, height =7.5)

grid.arrange(plot3)
dev.off()