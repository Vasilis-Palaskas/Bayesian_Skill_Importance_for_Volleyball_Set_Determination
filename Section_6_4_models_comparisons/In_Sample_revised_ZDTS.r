# Load the proper libraries.
library(rstan)
library(coda)
library(bayesplot)
library(skellam)
options(mc.cores = parallel::detectCores())# Activate multiple cores for stan models

################Data Preparation
#---------
source("C:\\Users\\vasileios palaskas\\Desktop\\Github folder\\Bayesian_Variable_Selection_Volleyball\\Section_2_1_Data_Processing\\Data_Preparation.R")#-Data_Preparation.R

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
## Vector of teams names along with
## their ranking positions, points, abilities
teams <- c("Ethnikos Alexandroupolis", "Foinikas Syrou", "Iraklis Chalkidas",       
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
#---datalist required for the revised ZDTS with only+Skills
data_zdts_only_skills<-list(c_thres=2,c_std=5,
                            n_games=dim(data_by_sets)[1],
                            n_teams=length(levels(data_by_sets$home_Team)),
                            away_team=as.numeric(data_by_sets$away_Team),
                            home_team=as.numeric(data_by_sets$home_Team),
                            X_home=final_X_home_std,X_away=final_X_away_std,
                            K_home=ncol(final_X_home_std),
                            K_away=ncol(final_X_away_std),
                            home_sets=data_by_sets$home_sets,
                            away_sets=data_by_sets$away_sets)

#---Set appropriate working directory
setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_Skills_Revised/Sections 4.4-4.5")## Run ZDTS_only_Skills_after_BVS.stan

load(file="revised_ZDTS_only_Skills_after_BVS")

# Parameter output
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

#---Arguments provided to the function
model<- ZDTS_only_Skills_after_BVS
train_dataset<-data_zdts_only_skills
test_dataset<-data_zdts_only_skills

in_sample_prediction_ZDTS<-function(train_dataset,test_dataset,model){
  
  ##### Extraction of model parameters
  params_final_ZDTS_paper.v1<-rstan::extract(model)
  dim(params_final_ZDTS_paper.v1$lambda1)
  
  mu_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$mu
  home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$home
  beta_home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$beta_home
  beta_away_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$beta_away
  # 
  # att_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$attack
  # def_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$defense
  
  #######--------We use the test data
  lambda1_out_final_ZDTS_paper.v1<-lambda2_out_final_ZDTS_paper.v1<-matrix(nrow=dim(params_final_ZDTS_paper.v1$lambda1)[1],
                                                                           ncol=test_dataset$n_games)
  # for (i in 1:test_dataset$n_games){
  #   for (j in 1:dim(lambda1_out_final_ZDTS_paper.v1)[1]){
  #     print(i)
  #     lambda1_out_final_ZDTS_paper.v1[j,i]<-exp(mu_params_final_ZDTS_paper.v1[j]+home_params_final_ZDTS_paper.v1[j]+
  #                                                 rowSums( beta_home_params_final_ZDTS_paper.v1[j,]*
  #                                                            test_dataset$X_home[i,])  )
  #     lambda2_out_final_ZDTS_paper.v1[j,i]<-exp(mu_params_final_ZDTS_paper.v1[j]+home_params_final_ZDTS_paper.v1[j]+
  #                                                 rowSums( beta_away_params_final_ZDTS_paper.v1[j,]*
  #                                                            test_dataset$X_away[i,]))
  #   }
  # }
  lambda1_out_final_ZDTS_paper.v1<-exp(replicate(dim(lambda1_out_final_ZDTS_paper.v1)[2],mu_params_final_ZDTS_paper.v1+
                                                   home_params_final_ZDTS_paper.v1)+
                                         beta_home_params_final_ZDTS_paper.v1%*%
                                         t(test_dataset$X_home) )
  lambda2_out_final_ZDTS_paper.v1<-exp(replicate(dim(lambda2_out_final_ZDTS_paper.v1)[2],
                                                 mu_params_final_ZDTS_paper.v1)+
                                         beta_away_params_final_ZDTS_paper.v1%*%
                                         t(test_dataset$X_away) )
  #### ----Out-of-sample Predicted differences from ZDS---#########
  #########---------Generation of values from ZDTS---------###########
  
  ####----Now we start to generate the predicted values------#####
  
  multi_pred_differences_out_final_ZDTS_paper.v1<-matrix(nrow=nrow(lambda2_out_final_ZDTS_paper.v1),
                                                         ncol=ncol(lambda2_out_final_ZDTS_paper.v1))
  
  for (i in 1:ncol(lambda2_out_final_ZDTS_paper.v1)) {
    for (j in 1:nrow(lambda2_out_final_ZDTS_paper.v1)) {
      print(i)
      #in order to avoid potential overflow
      if (lambda1_out_final_ZDTS_paper.v1[j,i]>700){
        lambda1_out_final_ZDTS_paper.v1[j,i]<-700
      }
      if (lambda2_out_final_ZDTS_paper.v1[j,i]>700){
        lambda2_out_final_ZDTS_paper.v1[j,i]<-700
      } 
      numer<-c(dskellam(-3,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(-2,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(-1,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(1,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(2,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(3,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]))
      
      denom<-dskellam(-3,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(-2,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(-1,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(1,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(2,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(3,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])
      if (denom==0){
        denom<-0.001
      }
      
      prob<-numer/denom
      x<-rmultinom(1,1,prob)
      multi_pred_differences_out_final_ZDTS_paper.v1[j,i]<-which(x[,1]==1)
    }
  }
  
  ###Transformation to the scale of negative and positive differences
  pred_differences_out_final_ZDTS_paper.v1<-matrix(nrow=nrow(multi_pred_differences_out_final_ZDTS_paper.v1
  ),ncol=ncol(multi_pred_differences_out_final_ZDTS_paper.v1))
  dim(multi_pred_differences_out_final_ZDTS_paper.v1)
  
  for (j in 1:nrow(multi_pred_differences_out_final_ZDTS_paper.v1
  )){
    for (i in 1:ncol(multi_pred_differences_out_final_ZDTS_paper.v1)) {
      
      if (multi_pred_differences_out_final_ZDTS_paper.v1
          [j,i]==4) {
        pred_differences_out_final_ZDTS_paper.v1[j,i]<-1
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==5){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<-2
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==6){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<-3
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==1){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<--3
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==2){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<--2
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==3){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<--1
      }
    }
  }
  
  
  
  #####------Regeneration of the league (by rmultinom differences)------########
  
  matrix_rows<-dim(pred_differences_out_final_ZDTS_paper.v1)[1]*test_dataset$n_games
  matrix_pred_game_points<-matrix(nrow=matrix_rows,ncol=test_dataset$n_teams)
  dim(matrix_pred_game_points)
  points_team<-NULL
  
  
  for (j in 0:(dim(pred_differences_out_final_ZDTS_paper.v1)[1]-1)){
    
    for (i in 1:test_dataset$n_games) {
      
      if (pred_differences_out_final_ZDTS_paper.v1[j+1,i]>1) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-3
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
      } else if (pred_differences_out_final_ZDTS_paper.v1[j+1,i]==1) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-2
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0	
      } else if (pred_differences_out_final_ZDTS_paper.v1[j+1,i]<(-1))	{
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-3
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0	
      } else if (pred_differences_out_final_ZDTS_paper.v1[j+1,i]==(-1)) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-2
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
      }
    }
    
  }
  
  
  ######## This is the matrix with number of games in rows and number of teams in col####
  ######-----Total points-----########
  post_distr_total_points<-matrix(nrow=dim(pred_differences_out_final_ZDTS_paper.v1)[1],ncol=test_dataset$n_teams)
  
  for (j in 0:(dim(pred_differences_out_final_ZDTS_paper.v1)[1]-1)){
    for (i in 1:test_dataset$n_teams) {
      post_distr_total_points[j+1,i]<-sum(matrix_pred_game_points[(j*test_dataset$n_games+1):(j*test_dataset$n_games+
                                                                                                test_dataset$n_games),i])
      
    }
  }
  
  
  
  ######------------------------------------------------------------------------------------------------------------------------------------------
  ##########--------Mean total points--------#########
  ###------- We extract the mean from the posterior of total points-------#######
  mean_pred_total_points<-NULL
  for (i in 1:test_dataset$n_teams){
    mean_pred_total_points[i]<-mean(post_distr_total_points[,i])
  }
  teams
  ranking_datafr<-data.frame(teams,mean_pred_total_points)
  ord_ranking_datafr<-ranking_datafr[order(ranking_datafr$mean_pred_total_points,decreasing=T),]
  
  
  
  ######
  ######----Predicted differences : multi_pred_differences_out_final_ZDTS_paper.v1
  ######----Predicted points:pred_team_points
  
  return(list(pred_differences_out=pred_differences_out_final_ZDTS_paper.v1,
              multi_pred_differences_out=multi_pred_differences_out_final_ZDTS_paper.v1,
              pred_team_points=post_distr_total_points,
              pred_ranking_test=ord_ranking_datafr))
}

###########-------------------------In Sample prediction results--------------------###############
in_sample_results_ZDTS<-in_sample_prediction_ZDTS(data_zdts_only_skills,data_zdts_only_skills,
                                                  model)

pred_differences_out=in_sample_results_ZDTS$pred_differences_out
multi_pred_differences_out=in_sample_results_ZDTS$multi_pred_differences_out
pred_team_points=in_sample_results_ZDTS$pred_team_points
pred_ranking_test=in_sample_results_ZDTS$pred_ranking_test

pred_differences_out<-pred_differences_out
pred_team_points<-pred_team_points
pred_ranking <-pred_ranking_test
multi_pred_differences_out<-multi_pred_differences_out

# 	pred_differences_out<-in_sample_results_ZDTS$pred_differences_out
# 	pred_team_points<-in_sample_results_ZDTS$pred_team_points
#   pred_ranking <-in_sample_results_ZDTS$pred_ranking_test
# 	multi_pred_differences_out<-in_sample_results_ZDTS$multi_pred_differences_out

save(pred_differences_out,file="in_sample_revised_set_differences_zdts")
save(multi_pred_differences_out,file="In sample Predicted differences (ZDTS)(Multinomial values)")
save(pred_ranking,file="in_sample_revised_ranking_zdts")
save(pred_team_points,file="in_sample_revised_points_zdts")

###########----------Posterior summaries of MAD----------------###########################

################--------------Deviance measures-------------###############
### Since we obtained some predicted quantities (set differences, points, rankings), we are ready to 
### obtain the posterior distribution of Mean Absolute Difference (MAD) diagnostics

##---Simple Bayesian RMSE, MSE MAE calculation
mae <- function(y,yhat){mean(abs(yhat-y))}

##########-------Expected Points----------########


observed_points_dev<-c(36,37,16,28,38,14,62,7,39,16,50,53)


# # or
mean(apply(pred_team_points,1,mae,y=observed_points_dev))#2.91
sd(apply(pred_team_points,1,mae,y=observed_points_dev))#0.64

##########-------Expected set differences----------########
#load(file.choose())### Main data/ new_data_zdts_only_skills
##########


observed_set_diff_dev<-data_zdts_only_skills$home_sets-data_zdts_only_skills$away_sets



# # or
mean(apply(pred_differences_out,1,mae,y=observed_set_diff_dev))#0.68
sd(apply(pred_differences_out,1,mae,y=observed_set_diff_dev))###0.07

##########-------Frequencies----------########


###Predicted set differences###
multi_pred_differences_out
pred_differences_out


length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))


pred_freq_zdts<-NULL
for (i in 1:nrow((pred_differences_out))) {
  pred_freq_zdts<-c(pred_freq_zdts,as.vector(c(table(pred_differences_out[i,])["-3"],
                                               table(pred_differences_out[i,])["-2"],table(pred_differences_out[i,])["-1"],
                                               table(pred_differences_out[i,])["1"],table(pred_differences_out[i,])["2"],
                                               table(pred_differences_out[i,])["3"])))
}

pred_freq_zdts_iter<-matrix(pred_freq_zdts,nrow=nrow(pred_differences_out),6,byrow=T)

colnames(pred_freq_zdts_iter)<-c("-3","-2","-1","1","2","3")

pred_freq_zdts_iter_matrix<-matrix(pred_freq_zdts_iter,nrow=nrow(pred_freq_zdts_iter),6)


# # # or
mean(apply(pred_freq_zdts_iter_matrix,1,mae,y=observed_freq))#7.55
sd(apply(pred_freq_zdts_iter_matrix,1,mae,y=observed_freq))###1.45



##########-------Relative Frequencies----------########
#load(file.choose())### Main data/ new_data_zdts_only_skills
##########

###Predicted set differences###
multi_pred_differences_out
pred_differences_out

length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))

pred_freq_zdts<-NULL
for (i in 1:nrow((pred_differences_out))) {
  pred_freq_zdts<-c(pred_freq_zdts,as.vector(c(table(pred_differences_out[i,])["-3"],
                                               table(pred_differences_out[i,])["-2"],
                                               table(pred_differences_out[i,])["-1"],
                                               table(pred_differences_out[i,])["1"],
                                               table(pred_differences_out[i,])["2"],
                                               table(pred_differences_out[i,])["3"])))
}

pred_rel_freq_zdts_iter<-matrix(pred_freq_zdts/132,nrow=nrow(pred_differences_out),6,byrow=T)

colnames(pred_rel_freq_zdts_iter)<-c("-3","-2","-1","1","2","3")

pred_rel_freq_zdts_iter_matrix<-matrix(pred_rel_freq_zdts_iter,nrow=nrow(pred_rel_freq_zdts_iter),6)


mean(apply(pred_rel_freq_zdts_iter_matrix,1,mae,y=observed_freq/data_zdts_only_skills$n_games))*100# 0.0572
sd(apply(pred_rel_freq_zdts_iter_matrix,1,mae,y=observed_freq/data_zdts_only_skills$n_games))*100### 0.0109

#------------------------------------------------------------

######----------Expected - Observed set difference (per team)------------###########
pred_home_sets<-pred_away_sets<-pred_home_team<-pred_away_team<-matrix(nrow=nrow(pred_differences_out),
                                                                       ncol=data_zdts_only_skills$n_games)


for (i in 1:nrow(pred_differences_out)){
  for (j in 1:data_zdts_only_skills$n_games){
    
    if (pred_differences_out[i,j]==3){
      pred_home_sets[i,j]<-3
      pred_away_sets[i,j]<-0
    } else if (pred_differences_out[i,j]==2){
      pred_home_sets[i,j]<-3
      pred_away_sets[i,j]<-1
    }else if (pred_differences_out[i,j]==1){
      pred_home_sets[i,j]<-3
      pred_away_sets[i,j]<-2
    }else if (pred_differences_out[i,j]==(-1)){
      pred_home_sets[i,j]<-2
      pred_away_sets[i,j]<-3
    }else if (pred_differences_out[i,j]==(-2)){
      pred_home_sets[i,j]<-1
      pred_away_sets[i,j]<-3
    }else if (pred_differences_out[i,j]==(-3)){
      pred_home_sets[i,j]<-0
      pred_away_sets[i,j]<-3
    }
  }
}


######------Observed set difference--------##########
observed_set_diff<-win_sets<-lose_sets<-NULL

for (i in 1:data_zdts_only_skills$n_teams){
  win_sets[i]<-sum(data_zdts_only_skills$home_sets[data_zdts_only_skills$home_team==i])+
    sum(data_zdts_only_skills$away_sets[data_zdts_only_skills$away_team==i])
  lose_sets[i]<-sum(data_zdts_only_skills$away_sets[data_zdts_only_skills$home_team==i])+
    sum(data_zdts_only_skills$home_sets[data_zdts_only_skills$away_team==i])
  observed_set_diff[i]<-win_sets[i]-lose_sets[i]
}

######------Generated set difference--------##########

win_pred_sets<-lose_pred_sets<-pred_set_diff<-matrix(nrow=nrow(pred_differences_out),ncol=data_zdts_only_skills$n_teams)

for (i in 1:nrow(pred_differences_out)){
  for (j in 1:data_zdts_only_skills$n_teams){
    win_pred_sets[i,j]<-sum(pred_home_sets[i,which(data_zdts_only_skills$home_team==j)])+
      sum(pred_away_sets[i,which(data_zdts_only_skills$away_team==j)])
    lose_pred_sets[i,j]<-sum(pred_away_sets[i,which(data_zdts_only_skills$home_team==j)])+
      sum(pred_home_sets[i,which(data_zdts_only_skills$away_team==j)])
    
    pred_set_diff[i,j]<-win_pred_sets[i,j]-lose_pred_sets[i,j]
  }
}


##########-------Expected Total set differences----------########

mean(apply(pred_set_diff,1,mae,y=observed_set_diff))#  5.05
sd(apply(pred_set_diff,1,mae,y=observed_set_diff))###  1.08
