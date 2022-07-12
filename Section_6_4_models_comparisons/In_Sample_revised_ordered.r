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

skill_events<-X_home_diff

X_ordered_Skills<-skill_events[,colnames(skill_events)%in%
                                 c("perfect_serves","failed_serves",
                                   "perfect_att1","failed_att1",
                                   "perfect_att2","failed_att2",
                                   "perfect_blocks",
                                   "failed_settings") ]


## Vector of teams names along with
## their ranking positions, points, abilities
teams <- levels(data_by_sets$home_Team)
observed_positions<-c("(7)","(6)","(9)","(8)","(5)","(11)","(1)","(12)","(4)","(10)","(3)","(2)")
observed_points<-c("(36)","(37)","(16)","(28)","(38)","(14)","(62)","(7)","(39)","(16)","(50)","(53)")


teams_attack<-paste0(teams," ","Attack")
teams_defense<-paste0(teams," ","Defense")
teams_over<-paste0(teams," ","Overall")

teams_pos<-paste0(teams," ",observed_positions)
teams_points<-paste0(teams," ",observed_points)
#---------

#---Data required for Ordered multinomial

model_data<-list(Y=data_by_sets$sets_difference_factor,X=X_ordered_Skills,n_teams=
                   length(levels(data_by_sets$home_Team)),
                 N=dim(data_by_sets)[1],K=ncol(X_ordered_Skills),ncat=6,
                 home_team=as.numeric(data_by_sets$home_Team),
                 away_team=as.numeric(data_by_sets$away_Team))

load("revised_ordered_skills_after_BVS")


###----Arguments provided for the function
model<-ordered_skills_after_BVS_model
train_dataset<-model_data
test_dataset<-model_data

##By having the same data set for both training and test data set, we implement the in sample diagnostics
##By having different data sets for the training and test data sets, we implement the out-of-sample prediction


in_sample_prediction_Ordered<-function(train_dataset,test_dataset,model){
  
## Extraction of model parameters
params_final_ordered_logistic<-rstan::extract(model)
  
  
####Proper transformations so that observed dif-sets and pred_dif sets be compatitible
pred_differences_ord<-params_final_ordered_logistic$y_pred

transform_pred_diff_ord<-matrix(nrow=dim(pred_differences_ord)[1],ncol=dim(pred_differences_ord)[2])## the y on original scale [-3,3]
for (i in 1:dim(pred_differences_ord)[1]) {
	for (j in 1:dim(pred_differences_ord)[2]){

  if (pred_differences_ord[i,j]==3) {
   transform_pred_diff_ord[i,j]=-1
  } else if (pred_differences_ord[i,j]==2){
    transform_pred_diff_ord[i,j]=-2
  } else if (pred_differences_ord[i,j]==1){
    transform_pred_diff_ord[i,j]=-3
  } else if  (pred_differences_ord[i,j]==4) {
   transform_pred_diff_ord[i,j]=1
  } else if (pred_differences_ord[i,j]==5) {
    transform_pred_diff_ord[i,j]=2
  } else if (pred_differences_ord[i,j]==6) {
    transform_pred_diff_ord[i,j]=3
  }	
 }
}
 
  #####------Regeneration of the league (by rmultinom differences)------########
  
  matrix_rows<-dim(transform_pred_diff_ord)[1]*test_dataset$N
  matrix_pred_game_points<-matrix(nrow=matrix_rows,ncol=12)
  dim(matrix_pred_game_points)
  points_team<-NULL
  
  
  for (j in 0:(dim(transform_pred_diff_ord)[1]-1)){
    
    for (i in 1:test_dataset$N) {
      
      if (transform_pred_diff_ord[j+1,i]>1) {
        matrix_pred_game_points[j*test_dataset$N+i,test_dataset$home_team[i]]<-3
        matrix_pred_game_points[j*test_dataset$N+i,test_dataset$away_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$N+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
      } else if (transform_pred_diff_ord[j+1,i]==1) {
        matrix_pred_game_points[j*test_dataset$N+i,test_dataset$home_team[i]]<-2
        matrix_pred_game_points[j*test_dataset$N+i,test_dataset$away_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$N+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0	
      } else if (transform_pred_diff_ord[j+1,i]<(-1))	{
        matrix_pred_game_points[j*test_dataset$N+i,test_dataset$home_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$N+i,test_dataset$away_team[i]]<-3
        matrix_pred_game_points[j*test_dataset$N+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0	
      } else if (transform_pred_diff_ord[j+1,i]==(-1)) {
        matrix_pred_game_points[j*test_dataset$N+i,test_dataset$home_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$N+i,test_dataset$away_team[i]]<-2
        matrix_pred_game_points[j*test_dataset$N+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
      }
    }
    
  }
  
  
  ######## This is the matrix with number of games in rows and number of teams in col####
  ######-----Total points-----########
  post_distr_total_points<-matrix(nrow=dim(transform_pred_diff_ord)[1],ncol=test_dataset$n_teams)
  
  for (j in 0:(dim(transform_pred_diff_ord)[1]-1)){
    for (i in 1:test_dataset$n_teams) {
      post_distr_total_points[j+1,i]<-sum(matrix_pred_game_points[(j*test_dataset$N+1):(j*test_dataset$N+test_dataset$N),i])
      
    }
  }
  
  
  
  # ------------------------------------------------------------------------------------------------------------------------------------------
  ##########--------Mean total points--------#########
  ###------- We extract the mean from the posterior of total points-------#######
  mean_pred_total_points<-NULL
  for (i in 1:test_dataset$n_teams){
    mean_pred_total_points[i]<-mean(post_distr_total_points[,i])
  }
   teams
  ranking_datafr<-data.frame(teams,mean_pred_total_points)
  ord_ranking_datafr<-ranking_datafr[order(ranking_datafr$mean_pred_total_points,decreasing=T),]
  
  
  			return(list(pred_differences_out=transform_pred_diff_ord,
			multi_pred_differences_out=pred_differences_ord,
			pred_team_points=post_distr_total_points,
			pred_ranking_test=ord_ranking_datafr)
			        )
}




###########-------------------------In Sample prediction results--------------------###############
in_sample_results_ordered<-in_sample_prediction_Ordered(model_data,
                                                        model_data,
                                                        ordered_skills_after_BVS_model)

				
	pred_differences_out_ordered<-in_sample_results_ordered$pred_differences_out
	pred_team_points_ordered<-in_sample_results_ordered$pred_team_points
  pred_ranking_ordered<-in_sample_results_ordered$pred_ranking_test
	multi_pred_differences_out_ordered<-in_sample_results_ordered$multi_pred_differences_out
							
	save(pred_differences_out_ordered,file="in_sample_set_difference_revised_ordered")
	save(pred_ranking_ordered,file="in_sample_ranking_revised_ordered")
	save(pred_team_points_ordered,file="in_sample_points_revised_ordered")

	
	###########----------Posterior summaries of MAD----------------###########################

## model_data needed for the calculation of post. distr. of MAD quantities.


	model_data<-list(Y=data_by_sets$sets_difference_factor,X=X_ordered_Skills,n_teams=
	                   length(levels(data_by_sets$home_Team)),
	                 N=dim(data_by_sets)[1],K=ncol(X_ordered_Skills),ncat=6,
	                 home_team=as.numeric(data_by_sets$home_Team),
	                 away_team=as.numeric(data_by_sets$away_Team))	

	
	################--------------Deviance measures-------------###############
	### Since we obtained some predicted quantities (set differences, points, rankings), we are ready to 
	### obtain the posterior distribution of Mean Absolute Difference (MAD) diagnostics
	
	##---Simple Bayesian RMSE, MSE MAE calculation
	mae <- function(y,yhat){mean(abs(yhat-y))}

##########-------Expected Points----------########


observed_points_dev<-c(36,37,16,28,38,14,62,7,39,16,50,53)


# # or
mean(apply(pred_team_points_ordered,1,mae,y=observed_points_dev))#2.38
sd(apply(pred_team_points_ordered,1,mae,y=observed_points_dev))#0.62


##########-------Expected set differences----------########

observed_set_diff_dev<-data_by_sets$sets_difference

mean(apply(pred_differences_out_ordered,1,mae,y=observed_set_diff_dev))#0.64
sd(apply(pred_differences_out_ordered,1,mae,y=observed_set_diff_dev))###0.07

##########-------Frequencies----------########


###Predicted set differences###
multi_pred_differences_out_ordered
pred_differences_out_ordered


length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))


pred_freq_ordered<-NULL
for (i in 1:nrow((pred_differences_out_ordered))) {
	pred_freq_ordered<-c(pred_freq_ordered,as.vector(c(table(pred_differences_out_ordered[i,])["-3"],
	table(pred_differences_out_ordered[i,])["-2"],table(pred_differences_out_ordered[i,])["-1"],
	table(pred_differences_out_ordered[i,])["1"],table(pred_differences_out_ordered[i,])["2"],
	table(pred_differences_out_ordered[i,])["3"])))
}

pred_freq_ordered_iter<-matrix(pred_freq_ordered,nrow=nrow(pred_differences_out_ordered),6,byrow=T)
pred_freq_ordered_iter[which(is.na(pred_freq_ordered_iter))]<-0
colnames(pred_freq_ordered_iter)<-c("-3","-2","-1","1","2","3")

pred_freq_ordered_iter_matrix<-matrix(pred_freq_ordered_iter,nrow=nrow(pred_freq_ordered_iter),6)

mean(apply(pred_freq_ordered_iter_matrix,1,mae,y=observed_freq))#7.55
sd(apply(pred_freq_ordered_iter_matrix,1,mae,y=observed_freq))###1.45



##########-------Relative Frequencies----------########
##########

###Predicted set differences###
multi_pred_differences_out_ordered
pred_differences_out_ordered

length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))


pred_freq_ordered<-NULL
for (i in 1:nrow(pred_differences_out_ordered)) {
	pred_freq_ordered<-c(pred_freq_ordered,as.vector(c(table(pred_differences_out_ordered[i,])["-3"],
	table(pred_differences_out_ordered[i,])["-2"],table(pred_differences_out_ordered[i,])["-1"],
	table(pred_differences_out_ordered[i,])["1"],table(pred_differences_out_ordered[i,])["2"],
	table(pred_differences_out_ordered[i,])["3"])))
}

pred_rel_freq_ordered_iter<-matrix(pred_freq_ordered/132,nrow=nrow(pred_differences_out_ordered),6,byrow=T)
pred_rel_freq_ordered_iter[which(is.na(pred_rel_freq_ordered_iter))]<-0

colnames(pred_rel_freq_ordered_iter)<-c("-3","-2","-1","1","2","3")

pred_rel_freq_ordered_iter_matrix<-matrix(pred_rel_freq_ordered_iter,nrow=nrow(pred_rel_freq_ordered_iter),6)

mean(apply(pred_rel_freq_ordered_iter_matrix,1,mae,y=observed_freq/model_data$N))*100# 2.63
sd(apply(pred_rel_freq_ordered_iter_matrix,1,mae,y=observed_freq/model_data$N))*100### 0.96

#------------------------------------------------------------

######----------Expected - Observed set difference (per team)------------###########
# The below dataframe is constructed here only in terms for convenience for this metric
data_zdts_ta_skills<-list(c_thres=2,c_std=5,
                          n_games=dim(data_by_sets)[1],
                          n_teams=length(levels(data_by_sets$home_Team)),
                          X_home=X_home_std,X_away=X_away_std,K=ncol(X_home_std),
                          home_sets=data_by_sets$home_sets,
                          away_sets=data_by_sets$away_sets,
                          home_team=as.numeric(data_by_sets$home_Team),
                          away_team=as.numeric(data_by_sets$away_Team))


pred_home_sets<-pred_away_sets<-pred_home_team<-pred_away_team<-matrix(nrow=nrow(pred_differences_out_ordered),
                                                                       ncol=model_data$N)


for (i in 1:nrow(pred_differences_out_ordered)){
	for (j in 1:model_data$N){

		if (pred_differences_out_ordered[i,j]==3){
			pred_home_sets[i,j]<-3
			pred_away_sets[i,j]<-0
		} else if (pred_differences_out_ordered[i,j]==2){
			pred_home_sets[i,j]<-3
			pred_away_sets[i,j]<-1
		}else if (pred_differences_out_ordered[i,j]==1){
			pred_home_sets[i,j]<-3
			pred_away_sets[i,j]<-2
		}else if (pred_differences_out_ordered[i,j]==(-1)){
			pred_home_sets[i,j]<-2
			pred_away_sets[i,j]<-3
		}else if (pred_differences_out_ordered[i,j]==(-2)){
			pred_home_sets[i,j]<-1
			pred_away_sets[i,j]<-3
		}else if (pred_differences_out_ordered[i,j]==(-3)){
			pred_home_sets[i,j]<-0
			pred_away_sets[i,j]<-3
		}
	}
}


######------Observed set difference--------##########
observed_set_diff<-win_sets<-lose_sets<-NULL


for (i in 1:model_data$n_teams){
	win_sets[i]<-sum(data_zdts_ta_skills$home_sets[data_zdts_ta_skills$home_team==i])+
					sum(data_zdts_ta_skills$away_sets[data_zdts_ta_skills$away_team==i])
	lose_sets[i]<-sum(data_zdts_ta_skills$away_sets[data_zdts_ta_skills$home_team==i])+
					sum(data_zdts_ta_skills$home_sets[data_zdts_ta_skills$away_team==i])
	observed_set_diff[i]<-win_sets[i]-lose_sets[i]
}

######------Generated set difference--------##########

win_pred_sets<-lose_pred_sets<-pred_set_diff<-matrix(nrow=nrow(pred_differences_out_ordered),ncol=model_data$n_teams)

for (i in 1:nrow(pred_differences_out_ordered)){
	for (j in 1:model_data$n_teams){
		win_pred_sets[i,j]<-sum(pred_home_sets[i,which(model_data$home_team==j)])+
					sum(pred_away_sets[i,which(model_data$away_team==j)])
		lose_pred_sets[i,j]<-sum(pred_away_sets[i,which(model_data$home_team==j)])+
					sum(pred_home_sets[i,which(model_data$away_team==j)])

		pred_set_diff[i,j]<-win_pred_sets[i,j]-lose_pred_sets[i,j]
	}
}




##########-------Expected Total set differences----------########

mean(apply(pred_set_diff,1,mae,y=observed_set_diff))#   4.147
sd(apply(pred_set_diff,1,mae,y=observed_set_diff))###   0.933



