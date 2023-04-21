######---------------------------------------------------------------------------- 
######---------- SECTION 7: ZDTS-based Athletes'  Evaluation in each field Position
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

# Data Preparation on team-level skills
#------------
source("C:\\Users\\vasileios palaskas\\Documents\\GitHub\\Bayesian_Skill_Importance_for_Volleyball_Set_Determination\\Section_2_1_Data_Processing\\Data_Preparation.R")#-Extra_Data_Processes/Data_Preparation.R
source("C:\\Users\\vasileios palaskas\\Documents\\GitHub\\Bayesian_Skill_Importance_for_Volleyball_Set_Determination\\Section_2_1_Data_Processing\\Athletes_Data_Preparation_3_clusters.R")#-Extra_Data_Processes/Athletes_Data_Preparation.R

# Choose the working directory of this file (...\\Submitted_Appendix\\ZDTS\\)

# Skills actions frequencies in team-level per both their home and away matches.

X_home<-data_by_sets%>%dplyr::select(Home_perfect_serves,Home_very_good_serves,
                                     Home_failed_serves,Home_perfect_passes,Home_very_good_passes,
                                     Home_poor_passes,Home_failed_passes,Home_perfect_att1,
                                     Home_blocked_att1,Home_failed_att1,Home_perfect_att2,
                                     Home_blocked_att2,Home_failed_att2,Home_perfect_blocks,
                                     Home_net_violation_blocks,Home_failed_blocks,Home_failed_settings)

X_away<-data_by_sets%>%dplyr::select(  Away_perfect_serves,Away_very_good_serves,
                                       Away_failed_serves,Away_perfect_passes,Away_very_good_passes,
                                       Away_poor_passes,Away_failed_passes,Away_perfect_att1,
                                       Away_blocked_att1,Away_failed_att1,Away_perfect_att2,
                                       Away_blocked_att2,Away_failed_att2,Away_perfect_blocks,
                                       Away_net_violation_blocks,Away_failed_blocks,Away_failed_settings)

# Obtain the differences of Skills actions frequencies in team-level per both their home and away matches.
X_home_diff<-data.frame(X_home-X_away)
colnames(X_home_diff)<-c(
  "perfect_serves","very_good_serves",
  "failed_serves","perfect_passes","very_good_passes",
  "poor_passes","failed_passes","perfect_att1",
  "blocked_att1","failed_att1","perfect_att2",
  "blocked_att2","failed_att2","perfect_blocks",
  "net_violation_blocks","failed_blocks","failed_settings")# Rename properly the skill variables

# Skill events selected via the BVS process based on PSI Median Threshold within team-level for their home and away matches.
# Standardization of the Model Matrices (including skill event actions for the home, away matches as well as the differences of those ones)

X_home_std<-data.frame(scale(X_home,center=T,scale=T) )
X_away_std<-data.frame(scale(X_away,center=T,scale=T) )
X_home_diff_std<-data.frame(scale(X_home-X_away,center=T,scale=T) )
X_away_diff_std<-data.frame(scale(X_away-X_home,center=T,scale=T) )
# Renaming to define which are standardized
colnames(X_home_std)<-paste0(colnames(X_home_std),"_","std")
colnames(X_away_std)<-paste0(colnames(X_away_std),"_","std")
colnames(X_away_diff_std)<-paste0(colnames(X_away_diff_std),"_","std")

# Choose the final Selected covariates obtained from the output of the the Gibbs BVS Method

final_X_home_std<-X_home_std%>%dplyr::select(Home_failed_serves_std,Home_poor_passes_std,
                                             Home_failed_passes_std,Home_blocked_att1_std,
                                             Home_failed_att1_std)

final_X_away_std<-X_away_std%>%dplyr::select(Away_failed_serves_std,
                                             Away_failed_passes_std,
                                             Away_blocked_att1_std,Away_failed_att1_std,
                                             Away_perfect_att2_std,Away_failed_att2_std,
                                             Away_net_violation_blocks_std,Away_failed_blocks_std)

# List required for the fitting of the Stan-based revised ZDTS with only skill actions
data_zdts_only_skills<-list(c_thres=2,c_std=5,
                            n_games=dim(data_by_sets)[1],
                            n_teams=length(levels(data_by_sets$home_Team)),
                            X_home=final_X_home_std,X_away=final_X_away_std,
                            K_home=ncol(final_X_home_std),
                            K_away=ncol(final_X_away_std),
                            home_sets=data_by_sets$home_sets,
                            away_sets=data_by_sets$away_sets)
#------------

# Saving/Loading the Bayesian ZDTS  model object
#-------
# Set appropriate working directory
setwd("C:/Users/vasileios palaskas/Documents/GitHub/Bayesian_Skill_Importance_for_Volleyball_Set_Determination/Sections_5_3-6_3/Section_5_3/ZDTS/With_only_skill_actions")
# load(file="revised_ZDTS_only_Skills_after_BVS")
# # 
# ZDTS_only_Skills_after_BVS<-stan("ZDTS_Skills_after_BVS.stan",
#                                  data=data_zdts_only_skills,chains=2,cores=2,
#                                  init_r=0.5,
#                                  iter=12000,
#                                  warmup=2000)### R
# # 
# save(ZDTS_only_Skills_after_BVS,file="revised_ZDTS_only_Skills_after_BVS")

# Set appropriate working directory
setwd("C:/Users/vasileios palaskas/Documents/GitHub/Bayesian_Skill_Importance_for_Volleyball_Set_Determination/Sections_5_3-6_3/Section_5_3/ZDTS/With_only_skill_actions")
load(file="revised_ZDTS_only_Skills_after_BVS")
#-------

# Extraction of several posterior quantities of ZDTS Model Parameters(deviances, log-likelihoods)
#-------------


#### Posterior summary statistics of the skill actions' parameters concerning the frequencies of skill actions of 
#### teams in the home, away matches, respectively.

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
betas_summary <- summary(ZDTS_only_Skills_after_BVS, pars = c("mu","home","beta_home","beta_away"), 
                         probs = c(0.025, 0.5,0.95))$summary
print(round(betas_summary[,c(1,3,4,5,6)],2))
betas_summary_main<-round(betas_summary[,c(1,3,4,5,6)],2)
xtable(betas_summary_main)

##### Extraction of the MCMC output of model's parameters 
params_final_ZDTS_paper.v1<-rstan::extract(ZDTS_only_Skills_after_BVS)
dim(params_final_ZDTS_paper.v1$lambda1)
mu_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$mu
home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$home
beta_home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$beta_home
beta_away_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$beta_away
lambda1_star_home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$lambda1_star
lambda2_star_away_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$lambda2_star
# Posterior mean of the parameters
apply(beta_home_params_final_ZDTS_paper.v1,2,mean)# Home blocked att 1 the worst skill
apply(beta_away_params_final_ZDTS_paper.v1,2,mean)#(Away) perfect att2   is the best skill


#-------------

#############--- Calculation of athlete-level skill actions in both free and standardized scale
#-----------------------


# Additional to the team-statistics, also include standardized selected covariates-statistics of teams (team-level frequencies of the skill actions)
# (in order to join later also player's statistics and 
# make calculations between team and athletes' based statistics)
data_by_sets<-cbind(data_by_sets,final_X_home_std,final_X_away_std)
head(data_by_sets)
# Now, for both home and away teams, each athlete is found in each row for each match from 22 matches of his
# own team
volley_athletes_per_game_statistics_home_team<-volley_athletes_per_game_statistics
colnames(volley_athletes_per_game_statistics_home_team)[1]<-"home_Team"
volley_athletes_per_game_statistics_away_team<-volley_athletes_per_game_statistics
colnames(volley_athletes_per_game_statistics_away_team)[1]<-"away_Team"
head(volley_athletes_per_game_statistics_home_team)
head(volley_athletes_per_game_statistics_away_team)

# Join the a) total skill actions per athlete of the whole season, average total skill actions across all athletes of the whole season
#  to the athletes' statistics per match for both his home and away matches,
volley_athletes_per_game_statistics_home_team<-left_join(volley_athletes_per_game_statistics_home_team,
                                                         volley_athletes_all_season_statistics[,colnames(volley_athletes_all_season_statistics)%in%c(
                                                           "Player","total_skill_actions","average_total_skill_actions")],
                                                         by="Player")
volley_athletes_per_game_statistics_away_team<-left_join(volley_athletes_per_game_statistics_away_team,
                                                         volley_athletes_all_season_statistics[,colnames(volley_athletes_all_season_statistics)%in%c(
                                                           "Player","total_skill_actions","average_total_skill_actions")],
                                                         by="Player")
head(volley_athletes_per_game_statistics_home_team)
head(volley_athletes_per_game_statistics_away_team)
dim(volley_athletes_per_game_statistics_home_team)#112 athletes
dim(volley_athletes_per_game_statistics_away_team)# 112 athletes

# Join athletes' data in both home and away matches with their own team's data using as key argument the home, away teams
# In essence, for each competing team we join its athletes' executions in both home and away ground matches 
# (suffix .x and .y refer to the home, away matches , respectively).
# In other words, in each row of athlete, we also joined the team-level skill actions, sets scored, etc...
data_by_sets_athletes<-left_join(data_by_sets,volley_athletes_per_game_statistics_home_team,by="home_Team")
data_by_sets_athletes<-left_join(data_by_sets_athletes,volley_athletes_per_game_statistics_away_team,by="away_Team")
head(data_by_sets_athletes)
dim(data_by_sets_athletes)
# Rename properly the skill actions of athletes to identify which 
# athlete-level skills are from their home and which ones from their away games.
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
                                                                     "failed_settings.x",
                                                                     "total_skill_actions.x","average_total_skill_actions.x")]<-paste0("Home",
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
                                                                                     "failed_settings.x",
                                                                                     "total_skill_actions.x","average_total_skill_actions.x")
                                                                               )

# Rename properly the skill of actions of athletes
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
                                                                     "perfect_blocks.y","total_settings.y","failed_settings.y",
                                                                     "total_skill_actions.y","average_total_skill_actions.y")]<-paste0(
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
                                                                             "perfect_blocks.y","total_settings.y","failed_settings.y",
                                                                             "total_skill_actions.y","average_total_skill_actions.y")
                                                                     )

# Create a dataframe with differences among team and its own team's athletes skill executions
data_by_sets_athletes_actions_differences<-data_by_sets_athletes



# Separate the (BVS Selected) athletes' skill actions for their home, away matches along with the athletes' total skill actions of
# each category



home_athletes_skills_coefs<-c("home_Team",paste0("Home_",c("Player.x","Position.x",
                                                           "failed_serves.x",
                                                           "poor_passes.x","failed_passes.x",
                                                           "blocked_att1.x","failed_att1.x",
                                                           "total_serves.x","total_passes.x",
                                                           "total_att1.x","total_skill_actions.x",
                                                           "average_total_skill_actions.x"
                                                           # ,
                                                           # "failed_serves_coef",
                                                           # "poor_passes_coef","failed_passes_coef",
                                                           # "blocked_att1_coef","failed_att1_coef"
)),# "avg_lambda1_star","median_lambda1_star",
"away_Team")

away_athletes_skills_coefs<-c("away_Team",paste0("Away_",c("Player.y","Position.y",
                                                           "failed_serves.y","failed_passes.y",
                                                           "blocked_att1.y","failed_att1.y",
                                                           "perfect_att2.y", "failed_att2.y",
                                                           "net_violation_blocks.y","failed_blocks.y",
                                                           "total_serves.y","total_passes.y",
                                                           "total_att1.y","total_att2.y",
                                                           "total_blocks.y", "total_skill_actions.y",
                                                           "average_total_skill_actions.y"
                                                           # ,
                                                           # "failed_serves_coef","failed_passes_coef",
                                                           # "blocked_att1_coef","failed_att1_coef",
                                                           # "perfect_att2_coef", "failed_att2_coef",
                                                           # "blocked_net_violation_coef","failed_blocks_coef"
)),
#  ,"avg_lambda2_star","median_lambda2_star",
"home_Team" )

# Dataframes representing in the home, away matches, respectively, their team, Position, BVS-Selected skill actions
home_data_by_sets_athletes<-data_by_sets_athletes%>%select(all_of(home_athletes_skills_coefs))%>%as.data.frame()
away_data_by_sets_athletes<-data_by_sets_athletes%>%select(all_of(away_athletes_skills_coefs))%>%as.data.frame()

head(home_data_by_sets_athletes)
head(away_data_by_sets_athletes)
# Keep distinct rows: Each row represents athletes' statistics per match for both his home and away matches.
home_data_by_sets_athletes_distinct<-home_data_by_sets_athletes%>%distinct()
away_data_by_sets_athletes_distinct<-away_data_by_sets_athletes%>%distinct()

head(home_data_by_sets_athletes_distinct)
head(away_data_by_sets_athletes_distinct)

dim(home_data_by_sets_athletes_distinct)# why 1224?
dim(away_data_by_sets_athletes_distinct)#why 1220?


# For more fair comparison, we should normalize the skill actions of each athlete with 
# athletes' own total frequencies of the main category
# of skill actions. For example, for an athlete A, related to his failed serves execution,
# we standardize those ones with the ratio of average total skill actions with the total skill actions of the same field Position
# This process will be implemented for each skill action selected by the BVS Algorithm.

# Home

home_data_by_sets_athletes_distinct$normalized_Home_failed_serves.x<-home_data_by_sets_athletes_distinct$Home_failed_serves.x*
  (home_data_by_sets_athletes_distinct$Home_average_total_skill_actions.x/home_data_by_sets_athletes_distinct$Home_total_skill_actions.x)

home_data_by_sets_athletes_distinct$normalized_Home_poor_passes.x<-home_data_by_sets_athletes_distinct$Home_poor_passes.x*
  (home_data_by_sets_athletes_distinct$Home_average_total_skill_actions.x/home_data_by_sets_athletes_distinct$Home_total_skill_actions.x)

home_data_by_sets_athletes_distinct$normalized_Home_failed_passes.x<-home_data_by_sets_athletes_distinct$Home_failed_passes.x*
  (home_data_by_sets_athletes_distinct$Home_average_total_skill_actions.x/home_data_by_sets_athletes_distinct$Home_total_skill_actions.x)

home_data_by_sets_athletes_distinct$normalized_Home_blocked_att1.x<-home_data_by_sets_athletes_distinct$Home_blocked_att1.x*
  (home_data_by_sets_athletes_distinct$Home_average_total_skill_actions.x/home_data_by_sets_athletes_distinct$Home_total_skill_actions.x)

home_data_by_sets_athletes_distinct$normalized_Home_failed_att1.x<-home_data_by_sets_athletes_distinct$Home_failed_att1.x*
  (home_data_by_sets_athletes_distinct$Home_average_total_skill_actions.x/home_data_by_sets_athletes_distinct$Home_total_skill_actions.x)
# Away
away_data_by_sets_athletes_distinct$normalized_Away_failed_serves.y<-away_data_by_sets_athletes_distinct$Away_failed_serves.y*
  (away_data_by_sets_athletes_distinct$Away_average_total_skill_actions.y/away_data_by_sets_athletes_distinct$Away_total_skill_actions.y)

away_data_by_sets_athletes_distinct$normalized_Away_failed_passes.y<-away_data_by_sets_athletes_distinct$Away_failed_passes.y*
  (away_data_by_sets_athletes_distinct$Away_average_total_skill_actions.y/away_data_by_sets_athletes_distinct$Away_total_skill_actions.y)

away_data_by_sets_athletes_distinct$normalized_Away_blocked_att1.y<-away_data_by_sets_athletes_distinct$Away_blocked_att1.y*
  (away_data_by_sets_athletes_distinct$Away_average_total_skill_actions.y/away_data_by_sets_athletes_distinct$Away_total_skill_actions.y)

away_data_by_sets_athletes_distinct$normalized_Away_failed_att1.y<-away_data_by_sets_athletes_distinct$Away_failed_att1.y*
  (away_data_by_sets_athletes_distinct$Away_average_total_skill_actions.y/away_data_by_sets_athletes_distinct$Away_total_skill_actions.y)

away_data_by_sets_athletes_distinct$normalized_Away_perfect_att2.y<-away_data_by_sets_athletes_distinct$Away_perfect_att2.y*
  (away_data_by_sets_athletes_distinct$Away_average_total_skill_actions.y/away_data_by_sets_athletes_distinct$Away_total_skill_actions.y)

away_data_by_sets_athletes_distinct$normalized_Away_failed_att2.y<-away_data_by_sets_athletes_distinct$Away_failed_att2.y*
  (away_data_by_sets_athletes_distinct$Away_average_total_skill_actions.y/away_data_by_sets_athletes_distinct$Away_total_skill_actions.y)

away_data_by_sets_athletes_distinct$normalized_Away_net_violation_blocks.y<-away_data_by_sets_athletes_distinct$Away_net_violation_blocks.y*
  (away_data_by_sets_athletes_distinct$Away_average_total_skill_actions.y/away_data_by_sets_athletes_distinct$Away_total_skill_actions.y)

away_data_by_sets_athletes_distinct$normalized_Away_failed_blocks.y<-away_data_by_sets_athletes_distinct$Away_failed_blocks.y*
  (away_data_by_sets_athletes_distinct$Away_average_total_skill_actions.y/away_data_by_sets_athletes_distinct$Away_total_skill_actions.y)
# Replace NaN (0/0) with 0
home_data_by_sets_athletes_distinct[is.na(home_data_by_sets_athletes_distinct)]<-0
away_data_by_sets_athletes_distinct[is.na(away_data_by_sets_athletes_distinct)]<-0

#example case: Mouclias all home games and away games(Paok has played twice in his home ground with kifisia)
head(home_data_by_sets_athletes_distinct)
head(away_data_by_sets_athletes_distinct)

home_data_by_sets_athletes_distinct[home_data_by_sets_athletes_distinct$Home_Player.x%in%
                                      home_data_by_sets_athletes_distinct$Home_Player.x[1],]
away_data_by_sets_athletes_distinct[away_data_by_sets_athletes_distinct$Away_Player.y%in%
                                      home_data_by_sets_athletes_distinct$Home_Player.x[1],]
# Averages of the normalized skill actions of athletes in home, away matches across all athletes (independent of their Position)
head(home_data_by_sets_athletes_distinct)

home_data_by_sets_athletes_distinct$avg_normalized_Home_failed_serves.x<-mean(home_data_by_sets_athletes_distinct$normalized_Home_failed_serves.x)
home_data_by_sets_athletes_distinct$avg_normalized_Home_poor_passes.x<-mean(home_data_by_sets_athletes_distinct$normalized_Home_poor_passes.x)
home_data_by_sets_athletes_distinct$avg_normalized_Home_failed_passes.x<-mean(home_data_by_sets_athletes_distinct$normalized_Home_failed_passes.x)
home_data_by_sets_athletes_distinct$avg_normalized_Home_blocked_att1.x<-mean(home_data_by_sets_athletes_distinct$normalized_Home_blocked_att1.x)
home_data_by_sets_athletes_distinct$avg_normalized_Home_failed_att1.x<-mean(home_data_by_sets_athletes_distinct$normalized_Home_failed_att1.x)

away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_serves.y<-mean(away_data_by_sets_athletes_distinct$normalized_Away_failed_serves.y)
away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_passes.y<-mean(away_data_by_sets_athletes_distinct$normalized_Away_failed_passes.y)
away_data_by_sets_athletes_distinct$avg_normalized_Away_blocked_att1.y<-mean(away_data_by_sets_athletes_distinct$normalized_Away_blocked_att1.y)
away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_att1.y<-mean(away_data_by_sets_athletes_distinct$normalized_Away_failed_att1.y)
away_data_by_sets_athletes_distinct$avg_normalized_Away_perfect_att2.y<-mean(away_data_by_sets_athletes_distinct$normalized_Away_perfect_att2.y)
away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_att2.y<-mean(away_data_by_sets_athletes_distinct$normalized_Away_failed_att2.y)
away_data_by_sets_athletes_distinct$avg_normalized_Away_net_violation_blocks.y<-mean(away_data_by_sets_athletes_distinct$normalized_Away_net_violation_blocks.y)
away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_blocks.y<-mean(away_data_by_sets_athletes_distinct$normalized_Away_failed_blocks.y)


# Averages of the normalized skill actions of athletes in home, away matches per Position
head(home_data_by_sets_athletes_distinct)

# Position_avg_home_normalized_skill_actions_athletes<-home_data_by_sets_athletes_distinct %>% 
#   group_by(Home_Position.x) %>% dplyr::summarize(Position_avg_normalized_Home_failed_serves.x = mean(normalized_Home_failed_serves.x, na.rm=TRUE),
#                                                  Position_avg_normalized_Home_poor_passes.x = mean(normalized_Home_poor_passes.x, na.rm=TRUE),
#                                                  Position_avg_normalized_Home_failed_passes.x = mean(normalized_Home_failed_passes.x, na.rm=TRUE),
#                                                  Position_avg_normalized_Home_blocked_att1.x = mean(normalized_Home_blocked_att1.x, na.rm=TRUE),
#                                                  Position_avg_normalized_Home_failed_att1.x = mean(normalized_Home_failed_att1.x, na.rm=TRUE)
#   )  %>% 
#   as.data.frame()
# 
# Position_avg_away_normalized_skill_actions_athletes<-away_data_by_sets_athletes_distinct %>% 
#   group_by(Away_Position.y) %>% dplyr::summarize(Position_avg_normalized_Away_failed_serves.y = mean(normalized_Away_failed_serves.y, na.rm=TRUE),
#                                                  Position_avg_normalized_Away_failed_passes.y = mean(normalized_Away_failed_passes.y, na.rm=TRUE),
#                                                  Position_avg_normalized_Away_blocked_att1.y = mean(normalized_Away_blocked_att1.y, na.rm=TRUE),
#                                                  Position_avg_normalized_Away_failed_att1.y = mean(normalized_Away_failed_att1.y, na.rm=TRUE),
#                                                  Position_avg_normalized_Away_perfect_att2.y = mean(normalized_Away_perfect_att2.y, na.rm=TRUE),
#                                                  Position_avg_normalized_Away_failed_att2.y = mean(normalized_Away_failed_att2.y, na.rm=TRUE),
#                                                  Position_avg_normalized_Away_net_violation_blocks.y = mean(normalized_Away_net_violation_blocks.y, na.rm=TRUE),
#                                                  Position_avg_normalized_Away_failed_blocks.y= mean(normalized_Away_failed_blocks.y, na.rm=TRUE)
#                                                  
#                                                  
#   )  %>% 
#   as.data.frame()#  Standardization of home athletes' skills based on their teams' avg, sd of specific team-level skills 
# 
# # Now let's join the average normalized skill actions of athletes, per Position, in the home, away matches, respectively, to the
# # dataframe including both the simple and the normalized skill actions of athletes in the home, away matches, respectively.
# home_athletes_skill_actions_all_scales<-left_join(home_data_by_sets_athletes_distinct,Position_avg_home_normalized_skill_actions_athletes,by="Home_Position.x")
# away_athletes_skill_actions_all_scales<-left_join(away_data_by_sets_athletes_distinct,Position_avg_away_normalized_skill_actions_athletes,by="Away_Position.y")

# Calculate the standardized scaling of athletes' skill actions per Position for the home, away matches, respectively.
home_athletes_skill_actions_all_scales$Home_failed_serves.x_std<-(home_athletes_skill_actions_all_scales$normalized_Home_failed_serves.x-
                                                                    home_athletes_skill_actions_all_scales$avg_normalized_Home_failed_serves.x)/  
  sd(X_home$Home_failed_serves)

home_athletes_skill_actions_all_scales$Home_poor_passes.x_std<-(home_athletes_skill_actions_all_scales$normalized_Home_poor_passes.x-
                                                                  home_athletes_skill_actions_all_scales$avg_normalized_Home_poor_passes.x)/  
  sd(X_home$Home_poor_passes)

home_athletes_skill_actions_all_scales$Home_failed_passes.x_std<-(home_athletes_skill_actions_all_scales$normalized_Home_failed_passes.x-
                                                                    home_athletes_skill_actions_all_scales$avg_normalized_Home_failed_passes.x)/  
  sd(X_home$Home_failed_passes)

home_athletes_skill_actions_all_scales$Home_blocked_att1.x_std<-(home_athletes_skill_actions_all_scales$normalized_Home_blocked_att1.x-
                                                                   home_athletes_skill_actions_all_scales$avg_normalized_Home_blocked_att1.x)/  
  sd(X_home$Home_blocked_att1)

home_athletes_skill_actions_all_scales$Home_failed_att1.x_std<-(home_athletes_skill_actions_all_scales$normalized_Home_failed_att1.x-
                                                                  home_athletes_skill_actions_all_scales$avg_normalized_Home_failed_att1.x)/  
  sd(X_home$Home_failed_att1)
# Away matches
away_athletes_skill_actions_all_scales$Away_failed_serves.y_std<-(away_athletes_skill_actions_all_scales$normalized_Away_failed_serves.y-
                                                                    away_athletes_skill_actions_all_scales$avg_normalized_Away_failed_serves.y)/  
  sd(X_away$Away_failed_serves)


away_athletes_skill_actions_all_scales$Away_failed_passes.y_std<-(away_athletes_skill_actions_all_scales$normalized_Away_failed_passes.y-
                                                                    away_athletes_skill_actions_all_scales$avg_normalized_Away_failed_passes.y)/  
  sd(X_away$Away_failed_passes)

away_athletes_skill_actions_all_scales$Away_blocked_att1.y_std<-(away_athletes_skill_actions_all_scales$normalized_Away_blocked_att1.y-
                                                                   away_athletes_skill_actions_all_scales$avg_normalized_Away_blocked_att1.y)/  
  sd(X_away$Away_blocked_att1)

away_athletes_skill_actions_all_scales$Away_failed_att1.y_std<-(away_athletes_skill_actions_all_scales$normalized_Away_failed_att1.y-
                                                                  away_athletes_skill_actions_all_scales$avg_normalized_Away_failed_att1.y)/  
  sd(X_away$Away_failed_att1)

away_athletes_skill_actions_all_scales$Away_perfect_att2.y_std<-(away_athletes_skill_actions_all_scales$normalized_Away_perfect_att2.y-
                                                                   away_athletes_skill_actions_all_scales$avg_normalized_Away_perfect_att2.y)/  
  sd(X_away$Away_perfect_att2)

away_athletes_skill_actions_all_scales$Away_failed_att2.y_std<-(away_athletes_skill_actions_all_scales$normalized_Away_failed_att2.y-
                                                                  away_athletes_skill_actions_all_scales$avg_normalized_Away_failed_att2.y)/  
  sd(X_away$Away_failed_att2)

away_athletes_skill_actions_all_scales$Away_net_violation_blocks.y_std<-(away_athletes_skill_actions_all_scales$normalized_Away_net_violation_blocks.y-
                                                                           away_athletes_skill_actions_all_scales$avg_normalized_Away_net_violation_blocks.y)/  
  sd(X_away$Away_net_violation_blocks)

away_athletes_skill_actions_all_scales$Away_failed_blocks.y_std<-(away_athletes_skill_actions_all_scales$normalized_Away_failed_blocks.y-
                                                                    away_athletes_skill_actions_all_scales$avg_normalized_Away_failed_blocks.y)/  
  sd(X_away$Away_failed_blocks)
# Convert it to a dataframe
# home_skills_std<-data.frame(Home_failed_serves.x_std=home_skills_std$Home_failed_serves.x_std,
#                             Home_poor_passes.x_std=home_skills_std$Home_poor_passes.x_std,
#                             Home_failed_passes.x_std= home_skills_std$Home_failed_passes.x_std,
#                             Home_blocked_att1.x_std=home_skills_std$Home_blocked_att1.x_std,
#                             Home_failed_att1.x_std=home_skills_std$Home_failed_att1.x_std)
# home_athletes_skill_actions_all_scales
# Combine both raw and std skills of athletes.
home_athletes_skill_actions_all_scales
head(home_athletes_skill_actions_all_scales)

away_athletes_skill_actions_all_scales
head(away_athletes_skill_actions_all_scales)


#-----------------------

# Development of formula, within Bayesian framework, for the construction of a 
# single performance evaluation metric for all volleyball athletes

#---------------------

# Step 1: Multiply the home, away actions' coefficients with the corresponding standardized skill actions
#         to create Bayesian performance indexes related to the home, away matches of the athletes 
#-----
# Home matches: Coefficient
multiplication_home_skills_coefs_per_player_std<- beta_home_params_final_ZDTS_paper.v1%*%
  t( home_athletes_skill_actions_all_scales[,
                                            colnames(home_athletes_skill_actions_all_scales)%in%c(
                                              "Home_failed_serves.x_std","Home_poor_passes.x_std",
                                              "Home_failed_passes.x_std","Home_blocked_att1.x_std",
                                              "Home_failed_att1.x_std")])
head(multiplication_home_skills_coefs_per_player_std)
dim(multiplication_home_skills_coefs_per_player_std)
# Away matches: Coefficient

multiplication_away_skills_coefs_per_player_std<- beta_away_params_final_ZDTS_paper.v1%*%
  t( away_athletes_skill_actions_all_scales[,
                                            colnames(away_athletes_skill_actions_all_scales)%in%c(
                                              "Away_failed_serves.y_std","Away_failed_passes.y_std",
                                              "Away_perfect_att2.y_std","Away_failed_att2.y_std",
                                              "Away_blocked_att1.y_std","Away_failed_att1.y_std",
                                              "Away_net_violation_blocks.y_std","Away_failed_blocks.y_std")])

head(multiplication_away_skills_coefs_per_player_std)    
dim(multiplication_away_skills_coefs_per_player_std)
#-----


#  Combine both home and away athletes' skills (raw and std per player for their home and away matches) 
#  as well as models' coefficients along with their posterior mean of  single performance evaluation metric 
#  calculated by the [log(l1)-log(l2)]-[log(l1*)-log(l2)] , [log(l1)-log(l2)]-[log(l1)-log(l2*)]
#  for home, away matches, respectively, where l1*, l2* are their linear predictors 
#  when a specific athlete is replaced by an average one in a corresponding ground.

home_data_by_sets_athletes_distinct_skills_coef_multiplication<-data.frame(home_athletes_skill_actions_all_scales,
                                    post_mean_home_evaluation= apply( multiplication_home_skills_coefs_per_player_std,2,mean))

head(home_data_by_sets_athletes_distinct_skills_coef_multiplication)

away_data_by_sets_athletes_distinct_skills_coef_multiplication<-data.frame(away_athletes_skill_actions_all_scales,
                          post_mean_away_evaluation= apply( multiplication_away_skills_coefs_per_player_std,2,mean))

# Example case: In home, away matches of athletes, we present the skill actions along with the 
# posterior mean of the single evaluation metric index.
head(home_data_by_sets_athletes_distinct_skills_coef_multiplication[,colnames(home_data_by_sets_athletes_distinct_skills_coef_multiplication)%in%
                                                                      c(paste0("Home_",c("failed_serves.x", "poor_passes.x",
                                                                                         "failed_passes.x",  "blocked_att1.x","failed_att1.x")),
                                                                        paste0("Home_",
                                                                               c("failed_serves.x","poor_passes.x",
                                                                                 "failed_passes.x","blocked_att1.x",
                                                                                 "failed_att1.x"),"_std"),
                                                                        "post_mean_home_evaluation")])

head(away_data_by_sets_athletes_distinct_skills_coef_multiplication[,colnames(away_data_by_sets_athletes_distinct_skills_coef_multiplication)%in%
                                                                      c(paste0("Away_",c("failed_serves.y","failed_passes.y",
                                                                                         "blocked_att1.y","failed_att1.y",
                                                                                         "perfect_att2.y", "failed_att2.y",
                                                                                         "net_violation_blocks.y","failed_blocks.y")),
                                                                        paste0("Away_",c("failed_serves.y","failed_passes.y",
                                                                                         "blocked_att1.y","failed_att1.y",
                                                                                         "perfect_att2.y", "failed_att2.y",
                                                                                         "net_violation_blocks.y","failed_blocks.y"),
                                                                               "_std"),"post_mean_away_evaluation")])# Calculate the difference between (l1-l2)-(l1*-l2*) where l1*,l2* are when athletes are absent from the total skills. 


summary(home_data_by_sets_athletes_distinct_skills_coef_multiplication)# post_mean_home_evaluation: min:-7.76596     , Median : 0.73056     , mean=0,  Max.   :0.94312   
summary(away_data_by_sets_athletes_distinct_skills_coef_multiplication)# post_mean_away_evaluation: min=-5.9407   , Median : 0.5207       , mean=0,  Max.   :  0.7909  

# 
home_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_log_scale_std<-home_data_by_sets_athletes_distinct_skills_coef_multiplication$post_mean_home_evaluation
away_data_by_sets_athletes_distinct_skills_coef_multiplication$impact_athletes_absence_log_scale_std<-away_data_by_sets_athletes_distinct_skills_coef_multiplication$post_mean_away_evaluation

#---------------------



# Need to have same columns names to apply rbind and for this reason,
#  we assign to new objects in order to avoid making changes in old objects
home_data_by_sets_athletes_distinct_skills_coef_multiplication_new<-home_data_by_sets_athletes_distinct_skills_coef_multiplication
away_data_by_sets_athletes_distinct_skills_coef_multiplication_new<-away_data_by_sets_athletes_distinct_skills_coef_multiplication

colnames(home_data_by_sets_athletes_distinct_skills_coef_multiplication_new)[colnames(home_data_by_sets_athletes_distinct_skills_coef_multiplication_new)%in%c(
  "home_Team","Home_Player.x")]<-c("Team","Player")
colnames(away_data_by_sets_athletes_distinct_skills_coef_multiplication_new)[colnames(away_data_by_sets_athletes_distinct_skills_coef_multiplication_new)%in%c(
  "away_Team", "Away_Player.y")]<-c("Team","Player")

# Keep specific columns
home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new<-rbind(home_data_by_sets_athletes_distinct_skills_coef_multiplication_new[c(
  "Team","Player",
  "impact_athletes_absence_log_scale_std")],
  away_data_by_sets_athletes_distinct_skills_coef_multiplication_new[c(
    "Team","Player",
    "impact_athletes_absence_log_scale_std")])
head(home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new)
# home and away matches of a random athlete
home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new[home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new$Player%in%
                                                                          home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new$Player[1],]

#---Calculate their impact based on their averages

athletes_impact<-home_away_data_by_sets_athletes_distinct_skills_coef_multiplication_new %>%
  group_by(Player,Team) %>%   
  summarize(median_of_impact_absence_log_scale = round( median(impact_athletes_absence_log_scale_std, 
                                                               na.rm = T),2),
            mean_of_impact_absence_log_scale = round( mean(impact_athletes_absence_log_scale_std, 
                                                           na.rm = T),2)
            
  ) %>%
  arrange( desc(median_of_impact_absence_log_scale) )      %>% as.data.frame()      # Specify function


summary(athletes_impact)

### Athletes above average per Position Analysis by means of using all actions
###  to calculate their total skill actions
#---Keep in a vector the selected coefficients

#-Rename the dataset to represent whole season statistics
volley_athletes_all_season_statistics<-volley_athletes

revised_zdts_skills<-c("total_serves","failed_serves","very_good_serves","perfect_serves",
                       "total_passes","very_good_passes", "perfect_passes","poor_passes","failed_passes",
                       "total_att1", "blocked_att1","failed_att1","perfect_att1",
                       "total_att2", "blocked_att2","failed_att2","perfect_att2",
                       "total_blocks", "net_violation_blocks","failed_blocks","perfect_blocks",
                       "total_settings", "failed_settings")

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
                                         "total_settings", "failed_settings"),"_all_season")

total_revised_zdts_skills_all_season<-c("total_serves_all_season","total_passes_all_season",
                                        "total_att1_all_season","total_att2_all_season",
                                        "total_blocks_all_season","total_settings_all_season")

separate_revised_zdts_skills_all_season<-paste0(c("failed_serves","very_good_serves","perfect_serves",
                                                  "very_good_passes", "perfect_passes","poor_passes","failed_passes",
                                                  "blocked_att1","failed_att1","perfect_att1",
                                                  "blocked_att2","failed_att2","perfect_att2",
                                                  "net_violation_blocks","failed_blocks","perfect_blocks",
                                                  "failed_settings"),"_all_season")
#--Join total skill actions
athletes_impact_full_dataset<-left_join(athletes_impact,volley_athletes_all_season_statistics[,
                                        colnames(volley_athletes_all_season_statistics)%in%c("Player","Position",
                                                                                                                                                   revised_zdts_skills_all_season)],
                                        by="Player")
head(athletes_impact_full_dataset)
summary(athletes_impact_full_dataset)
#---create the sum of all skill actions in a unique variable in order to obtain a criterion variable for filtering out
#--- athletes with low frequency skill actions

athletes_impact_full_dataset$sum_skill_actions_all_season<-apply(athletes_impact_full_dataset[
  total_revised_zdts_skills_all_season],1,sum)
head(athletes_impact_full_dataset)
summary(athletes_impact_full_dataset)
#---Filter out athletes with less total skills actions than the 50% (median) across all athletes and per Position

#----- and per Position

# Average threshold of impact per Position
athletes_impact_full_dataset$Position[athletes_impact_full_dataset$Position=="0"]<-"O"
athletes_impact_full_dataset$Position<-as.factor(athletes_impact_full_dataset$Position)

# Arrange based on the Position, and posterior evaluation Index
athletes_impact_full_dataset<-athletes_impact_full_dataset%>% 
  arrange(Position,desc(median_of_impact_absence_log_scale) ) 

write.csv(athletes_impact_full_dataset,file="top_80_Bayesian_stdized_evaluation_index_2016_2017.csv",row.names = FALSE)

