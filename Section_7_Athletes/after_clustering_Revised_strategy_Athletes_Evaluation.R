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

# Data Preparation on team-level skills actions frequencies for both home and their away matches
#------------
source("C:\\Users\\vasileios palaskas\\Documents\\GitHub\\Bayesian_Skill_Importance_for_Volleyball_Set_Determination\\Section_2_1_Data_Processing\\Data_Preparation.R")#-Extra_Data_Processes/Data_Preparation.R
source("C:\\Users\\vasileios palaskas\\Documents\\GitHub\\Bayesian_Skill_Importance_for_Volleyball_Set_Determination\\Section_2_1_Data_Processing\\Athletes_Data_Preparation_3_clusters.R")#-Extra_Data_Processes/Athletes_Data_Preparation.R

# Frequencies of Skills actions (in team-level) for both their home and away matches.

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
dim(X_home)
mean(apply(X_home,1,sum))
# Obtain the differences of Skills actions frequencies between their home and away matches in team-level 
X_home_diff<-data.frame(X_home-X_away)
colnames(X_home_diff)<-c("perfect_serves","very_good_serves",
  "failed_serves","perfect_passes","very_good_passes",
  "poor_passes","failed_passes","perfect_att1",
  "blocked_att1","failed_att1","perfect_att2",
  "blocked_att2","failed_att2","perfect_blocks",
  "net_violation_blocks","failed_blocks","failed_settings")# Rename properly the skill variables
#------------

# Saving/Loading the Bayesian ZDTS  model object
#-------
# Set appropriate working directory
setwd("C:/Users/vasileios palaskas/Documents/GitHub/Bayesian_Skill_Importance_for_Volleyball_Set_Determination/Sections_5_3-6_3/Section_5_3/ZDTS/With_only_skill_actions")
load(file="revised_ZDTS_only_Skills_after_BVS")
#-------

# Extraction of several posterior quantities of ZDTS Model Parameters
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

# print(ZDTS_only_Skills_after_BVS,
#       pars=c("mu","home",
#              "beta_home","beta_away","dev"),probs = c(0.025,0.5,0.975), digits=2)

# Access summary statistics of the model parameters 
betas_summary <- summary(ZDTS_only_Skills_after_BVS, pars = c("mu","home","beta_home","beta_away"), 
                         probs = c(0.025, 0.5,0.95))$summary
print(round(betas_summary[,c(1,3,4,5,6)],2))

##### Extraction of the MCMC chains of ZDTS model's parameters 
params_final_ZDTS_paper.v1<-rstan::extract(ZDTS_only_Skills_after_BVS)
dim(params_final_ZDTS_paper.v1$lambda1)
# MCMC chains for selected parameters
mu_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$mu
home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$home
beta_home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$beta_home
beta_away_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$beta_away
lambda1_star_home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$lambda1_star
lambda2_star_away_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$lambda2_star
#-------------

############# Calculation of athlete-level skill actions in both original and standardise scale for
############# both their home and away matches.
#-----------------------

# Athletes' skill actions frequencies in their Home and Away matches (Here, they are identical since we made
# the naive assumption that their match-based skill actions are equal to their total skill actions divided
# by the total number of matches independent of their field ground)
volley_athletes_per_game_statistics_home_team<-volley_athletes_per_game_statistics
colnames(volley_athletes_per_game_statistics_home_team)[1]<-"home_Team"
volley_athletes_per_game_statistics_away_team<-volley_athletes_per_game_statistics
colnames(volley_athletes_per_game_statistics_away_team)[1]<-"away_Team"

head(volley_athletes_per_game_statistics_home_team)
head(volley_athletes_per_game_statistics_away_team)

# Join the a) total skill actions per athlete of the whole season, average total skill actions across all athletes of the same
# position of the to b) the athletes' statistics per match for both his home and away matches in terms of next matrices multiplications required
# for athletes performance evaluation. 
# (Join a) and b) dataframes)
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

# For the needs of athletes' performance evaluations, we need also the the team-level data. For this reason,
#  Join athletes' data both home and away datasets) into their own team's data using as key argument the home, away teams
# In essence, for each competing team we join its athletes' executions in both home and away ground matches 
# (suffix .x and .y refer to the skill events frequencies executed in home, away matches, respectively).
# In other words, now the new datasets will include in each row an athlete along with his statistics in each match combined 
# with the team-level skill actions, sets scored, etc...
data_by_sets_athletes<-left_join(data_by_sets,volley_athletes_per_game_statistics_home_team,by="home_Team")
data_by_sets_athletes<-left_join(data_by_sets_athletes,volley_athletes_per_game_statistics_away_team,by="away_Team")
head(data_by_sets_athletes)
dim(data_by_sets_athletes)


# Home matches athletes' skill actions+their own teams' skill actions
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

# Away matches athletes' skill actions+their own teams' skill actions
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

# Separate the names of selected athletes' skill actions for their home, away matches along with the athletes' total skill actions & average 
# skill actions of athletes from the same field Position.
home_athletes_skills_coefs<-c("home_Team",paste0("Home_",c("Player.x","Position.x",
                                                           "failed_serves.x",
                                                           "poor_passes.x","failed_passes.x",
                                                           "blocked_att1.x","failed_att1.x",
                                                           "total_serves.x","total_passes.x",
                                                           "total_att1.x","total_skill_actions.x",
                                                           "average_total_skill_actions.x")),"away_Team")

away_athletes_skills_coefs<-c("away_Team",paste0("Away_",c("Player.y","Position.y",
                                                           "failed_serves.y","failed_passes.y",
                                                           "blocked_att1.y","failed_att1.y",
                                                           "perfect_att2.y", "failed_att2.y",
                                                           "net_violation_blocks.y","failed_blocks.y",
                                                           "total_serves.y","total_passes.y",
                                                           "total_att1.y","total_att2.y",
                                                           "total_blocks.y", "total_skill_actions.y",
                                                           "average_total_skill_actions.y")),"home_Team" )

# Dataframes representing in the home, away matches athlete-level skill actions, respectively,
# combined with their team, Position, BVS-Selected skill actions
home_data_by_sets_athletes<-data_by_sets_athletes%>%select(all_of(home_athletes_skills_coefs))%>%as.data.frame()
away_data_by_sets_athletes<-data_by_sets_athletes%>%select(all_of(away_athletes_skills_coefs))%>%as.data.frame()

head(home_data_by_sets_athletes)
head(away_data_by_sets_athletes)
# Keep distinct rows: Each row represents athletes' statistics per match for both his home and away matches. In essence, remove duplicate rows
home_data_by_sets_athletes_distinct<-home_data_by_sets_athletes%>%distinct()
away_data_by_sets_athletes_distinct<-away_data_by_sets_athletes%>%distinct()

head(home_data_by_sets_athletes_distinct)
head(away_data_by_sets_athletes_distinct)

dim(home_data_by_sets_athletes_distinct)# why 1224?
dim(away_data_by_sets_athletes_distinct)#why 1220?

# For more fair comparison, we should normalize the skill actions of each athlete with their own total frequencies of the main category
# of skill actions. For example, for an athlete A, related to his failed serves execution,
# we standardize those ones with the ratio of average total skill actions with the total skill actions of the same field Position
# This process will be implemented for each skill action selected by the BVS Algorithm in their home and away matches (see below).

# Home matches: Obtain normalised coefficients

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
# Away matches: Obtain normalised coefficients

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
# head(home_data_by_sets_athletes_distinct)
# head(away_data_by_sets_athletes_distinct)
# 
# home_data_by_sets_athletes_distinct[home_data_by_sets_athletes_distinct$Home_Player.x%in%
#                                       home_data_by_sets_athletes_distinct$Home_Player.x[1],]
# away_data_by_sets_athletes_distinct[away_data_by_sets_athletes_distinct$Away_Player.y%in%
#                                       home_data_by_sets_athletes_distinct$Home_Player.x[1],]
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

# Calculate the standardized scaling of athletes' skill actions per Position for the home, away matches, respectively.
home_data_by_sets_athletes_distinct$Home_failed_serves.x_std<-(home_data_by_sets_athletes_distinct$normalized_Home_failed_serves.x-
                                                                    home_data_by_sets_athletes_distinct$avg_normalized_Home_failed_serves.x)/  
  sd(X_home$Home_failed_serves)

home_data_by_sets_athletes_distinct$Home_poor_passes.x_std<-(home_data_by_sets_athletes_distinct$normalized_Home_poor_passes.x-
                                                                  home_data_by_sets_athletes_distinct$avg_normalized_Home_poor_passes.x)/  
  sd(X_home$Home_poor_passes)

home_data_by_sets_athletes_distinct$Home_failed_passes.x_std<-(home_data_by_sets_athletes_distinct$normalized_Home_failed_passes.x-
                                                                    home_data_by_sets_athletes_distinct$avg_normalized_Home_failed_passes.x)/  
  sd(X_home$Home_failed_passes)

home_data_by_sets_athletes_distinct$Home_blocked_att1.x_std<-(home_data_by_sets_athletes_distinct$normalized_Home_blocked_att1.x-
                                                                   home_data_by_sets_athletes_distinct$avg_normalized_Home_blocked_att1.x)/  
  sd(X_home$Home_blocked_att1)

home_data_by_sets_athletes_distinct$Home_failed_att1.x_std<-(home_data_by_sets_athletes_distinct$normalized_Home_failed_att1.x-
                                                                  home_data_by_sets_athletes_distinct$avg_normalized_Home_failed_att1.x)/  
  sd(X_home$Home_failed_att1)
# Away matches
away_data_by_sets_athletes_distinct$Away_failed_serves.y_std<-(away_data_by_sets_athletes_distinct$normalized_Away_failed_serves.y-
                                                                    away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_serves.y)/  
  sd(X_away$Away_failed_serves)


away_data_by_sets_athletes_distinct$Away_failed_passes.y_std<-(away_data_by_sets_athletes_distinct$normalized_Away_failed_passes.y-
                                                                    away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_passes.y)/  
  sd(X_away$Away_failed_passes)

away_data_by_sets_athletes_distinct$Away_blocked_att1.y_std<-(away_data_by_sets_athletes_distinct$normalized_Away_blocked_att1.y-
                                                                   away_data_by_sets_athletes_distinct$avg_normalized_Away_blocked_att1.y)/  
  sd(X_away$Away_blocked_att1)

away_data_by_sets_athletes_distinct$Away_failed_att1.y_std<-(away_data_by_sets_athletes_distinct$normalized_Away_failed_att1.y-
                                                                  away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_att1.y)/  
  sd(X_away$Away_failed_att1)

away_data_by_sets_athletes_distinct$Away_perfect_att2.y_std<-(away_data_by_sets_athletes_distinct$normalized_Away_perfect_att2.y-
                                                                   away_data_by_sets_athletes_distinct$avg_normalized_Away_perfect_att2.y)/  
  sd(X_away$Away_perfect_att2)

away_data_by_sets_athletes_distinct$Away_failed_att2.y_std<-(away_data_by_sets_athletes_distinct$normalized_Away_failed_att2.y-
                                                                  away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_att2.y)/  
  sd(X_away$Away_failed_att2)

away_data_by_sets_athletes_distinct$Away_net_violation_blocks.y_std<-(away_data_by_sets_athletes_distinct$normalized_Away_net_violation_blocks.y-
                                                                           away_data_by_sets_athletes_distinct$avg_normalized_Away_net_violation_blocks.y)/  
  sd(X_away$Away_net_violation_blocks)

away_data_by_sets_athletes_distinct$Away_failed_blocks.y_std<-(away_data_by_sets_athletes_distinct$normalized_Away_failed_blocks.y-
                                                                    away_data_by_sets_athletes_distinct$avg_normalized_Away_failed_blocks.y)/  
  sd(X_away$Away_failed_blocks)

#-----------------------

# Development of formula, within Bayesian framework, for the construction of a 
# single performance evaluation metric for all volleyball athletes

#---------------------

# Step 1: Multiply the home, away actions' coefficients with the corresponding standardized skill actions
#         to create Bayesian performance indexes related to the home, away matches of the athletes 
#-----
# Home matches: Coefficient
multiplication_home_skills_coefs_per_player_std<- beta_home_params_final_ZDTS_paper.v1%*%
  t( home_data_by_sets_athletes_distinct[,
                                            colnames(home_data_by_sets_athletes_distinct)%in%c(
                                              "Home_failed_serves.x_std","Home_poor_passes.x_std",
                                              "Home_failed_passes.x_std","Home_blocked_att1.x_std",
                                              "Home_failed_att1.x_std")])
head(multiplication_home_skills_coefs_per_player_std)
dim(multiplication_home_skills_coefs_per_player_std)
# Away matches: Coefficient

multiplication_away_skills_coefs_per_player_std<- ( beta_away_params_final_ZDTS_paper.v1%*%
  t( away_data_by_sets_athletes_distinct[,
                                            colnames(away_data_by_sets_athletes_distinct)%in%c(
                                              "Away_failed_serves.y_std","Away_failed_passes.y_std",
                                              "Away_perfect_att2.y_std","Away_failed_att2.y_std",
                                              "Away_blocked_att1.y_std","Away_failed_att1.y_std",
                                              "Away_net_violation_blocks.y_std","Away_failed_blocks.y_std")]) )

head(multiplication_away_skills_coefs_per_player_std)    
dim(multiplication_away_skills_coefs_per_player_std)
#-----


#  Combine both home and away athletes' skills (raw and std per player for their home and away matches) 
#  as well as models' coefficients along with their posterior mean of  single performance evaluation metric 
#  calculated by the [log(l1)-log(l2)]-[log(l1*)-log(l2)] , [log(l1)-log(l2)]-[log(l1)-log(l2*)]
#  for home, away matches, respectively, where l1*, l2* are their linear predictors 
#  when a specific athlete is replaced by an average one in a corresponding ground.

home_data_by_sets_athletes_distinct_skills_coef_multiplication<-data.frame(home_data_by_sets_athletes_distinct,
                                   post_mean_home_evaluation= apply( multiplication_home_skills_coefs_per_player_std,2,mean))

head(home_data_by_sets_athletes_distinct_skills_coef_multiplication)

away_data_by_sets_athletes_distinct_skills_coef_multiplication<-data.frame(away_data_by_sets_athletes_distinct,
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

write.csv(athletes_impact_full_dataset,file="Final_After_Clustering_Bayesian_stdized_evaluation_index_2016_2017.csv",row.names = FALSE)

