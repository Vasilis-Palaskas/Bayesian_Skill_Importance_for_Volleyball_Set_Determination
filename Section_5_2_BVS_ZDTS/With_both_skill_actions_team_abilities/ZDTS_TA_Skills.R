# Load the proper libraries.
library(rstan)
library(coda)
library(bayesplot)
library(ggmcmc)
library(car)
# Choose the working directory of this file (...\\Submitted_Appendix\\Ordered\\)

#---Data Preparation
source("C:\\Users\\vasileios palaskas\\Documents\\GitHub\\Bayesian_Skill_Importance_for_Volleyball_Set_Determination\\Section_2_1_Data_Processing\\Data_Preparation.R")#-Data_Preparation.R

# Choose the working directory of this file (...\\Submitted_Appendix\\ZDTS\\)
# setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_TA_Skills")

#------Skills for both Home and Away Teams-----Exclude perfect and very good passes from both Home and Away Skills
X_home<-data_by_sets[c(
  "Home_perfect_serves","Home_very_good_serves",
  "Home_failed_serves",
  "Home_poor_passes","Home_failed_passes","Home_perfect_att1",
  "Home_blocked_att1","Home_failed_att1","Home_perfect_att2",
  "Home_blocked_att2","Home_failed_att2","Home_perfect_blocks",
  "Home_net_violation_blocks","Home_failed_blocks","Home_failed_settings")
]

X_away<-data_by_sets[c(
  "Away_perfect_serves","Away_very_good_serves",
  "Away_failed_serves",
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
  "failed_serves",
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

#-------Step 0: Run the full model to obtain the pilot posterior stv. and means of model parameters

setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_TA_Skills_Revised")
data_zdts_ta_skills<-list(c_thres=2,c_std=5,
                          n_games=dim(data_by_sets)[1],
                          n_teams=length(levels(data_by_sets$home_Team)),
                          X_home=X_home_std,X_away=X_away_std,K=ncol(X_home_std),
                          home_sets=data_by_sets$home_sets,
                          away_sets=data_by_sets$away_sets,
                       home_team=as.numeric(data_by_sets$home_Team),
                       away_team=as.numeric(data_by_sets$away_Team))


## Run full_zdts_only_skills.stan
full_zdts_ta_skills_revised<-stan("full_zdts_skills.stan",
                  data=data_zdts_ta_skills,chains=2,init_r=0.5,
                   iter=10000,warmup=2000,cores=2)### R
 save(full_zdts_ta_skills_revised,file="full_zdts_ta_skills_revised")

# load("full_zdts_ta_skills_revised")
# Extract the posterior summary statistics of both candidate variables' parameters and rest of other parameters.

betas_summary<-summary(full_zdts_ta_skills_revised, pars = c("beta_home","beta_away"))$summary
attack_raw_summary<-summary(full_zdts_ta_skills_revised, pars = c("attack_raw"))$summary
defense_raw_summary<-summary(full_zdts_ta_skills_revised, pars = c("defense_raw"))$summary
mu_summary<-summary(full_zdts_ta_skills_revised, pars = c("mu"))$summary
home_summary<-summary(full_zdts_ta_skills_revised, pars = c("home"))$summary



# Use their posterior means and standard deviations (from pilot run)
# for both initial values specification and prior specification.

post_mean_beta_home<-betas_summary[1:(dim(X_home)[2]),1]####posterior mean for beta home
post_mean_beta_away<-betas_summary[(dim(X_home)[2]+1):(2*dim(X_home)[2]),1]####posterior mean for beta

post_sd_beta_home<-betas_summary[1:(dim(X_home)[2]),3]### posterior sd for beta
post_sd_beta_away<-betas_summary[(dim(X_home)[2]+1):(2*dim(X_home)[2]),3]### posterior sd for beta

post_mean_beta<-c(post_mean_beta_home,post_mean_beta_away)
post_sd_beta<-c(post_sd_beta_home,post_sd_beta_away)
####------------------
###  BSS For ZDTS model with both team abilities and skill actions can now begin
####------------------
# Step 1: Initialization of the model parameters.
gammas_home<-rep(1,(dim(X_home)[2]))
gammas_away<-rep(1,(dim(X_home)[2]))
mu<-mu_summary[,1]
home<-home_summary[,1]
betas_home<-post_mean_beta_home
betas_away<-post_mean_beta_away

attack_raw<-attack_raw_summary[,1]
defense_raw<-defense_raw_summary[,1]



# Prepare the vectors with the posterior samples of all gammas and betas, respectively.
gammas_home_matrix<-gammas_away_matrix<-betas_home_matrix<-betas_away_matrix<-NULL
T<-30000 # Total MCMC iterations
# Step 2 
for (i in 1:T){
  print(i)
  

  # Step 3: Data input needed for running the model through RStan.
  data_varsel_zdts<-list(n_teams=data_zdts_ta_skills$n_teams,n_games=data_zdts_ta_skills$n_games,
                         c_thres=2,c_std=5,
                         home_sets=data_zdts_ta_skills$home_sets,
                         away_sets=data_zdts_ta_skills$away_sets,
                         X_home=X_home_std,X_away=X_away_std,
                         K=ncol(X_home_std),
                         home_team=as.numeric(data_by_sets$home_Team),
                         away_team=as.numeric(data_by_sets$away_Team),
                         gammas_home=gammas_home,gammas_away=gammas_away,
                         post_mean_beta_home=post_mean_beta_home,post_mean_beta_away=post_mean_beta_away,
                         post_sd_beta_home=post_sd_beta_home,post_sd_beta_away=post_sd_beta_away)
  
  # Step 4:Run the model through RStan for one sampling iteration (20 warm up and 21 total iterations, 21-20=1 sampling iteration) in order to update the betas from the full conditional posterior distributions. 
  # Use the previous iteration's parameter values as initial parameter values so MCMC Algorithm can begin.
  n_chains <- 1
  initf2 <- function(chain_id = 1) {
	    list(beta_home=betas_home,beta_away=betas_away,
                    mu=mu, home=home,
                    attack_raw=attack_raw,defense_raw=defense_raw)
      }
  init_ll <- lapply(1:n_chains, function(id) initf2(chain_id = id))
  zdts_volley_skills_all<-stan("ZDTS_BVS_TA_Skills.stan",
                               data=data_varsel_zdts,chains=n_chains,
                                iter=21,warmup=20,init=init_ll,
                                control=list(adapt_window=15,adapt_init_buffer=3,adapt_term_buffer=2))### R

  # Initialize the log-likelihood for both cases 0 and 1 for gammas indicators/coefficients.
  log_point_zero<-matrix(NA,nrow=data_varsel_zdts$n_games,ncol=(2*data_varsel_zdts$K)) # matrix with log likelihoods when gamma[j]=0
  log_point_one<-matrix(NA,nrow=data_varsel_zdts$n_games,ncol=(2*data_varsel_zdts$K))  #  matrix with log likelihoods when gamma[j]=1
  
  # Extract both model's parameters and log-likelihoods for both cases of gammas indicators.
  par<-rstan::extract(zdts_volley_skills_all)
  mu<-par$mu[1]
  home<-par$home[1]
  betas_home<-par$beta_home[1,]
  betas_away<-par$beta_away[1,]
  attack_raw<-par$attack_raw[1,]
  defense_raw<-par$defense_raw[1,]
  log_point_zero<-par$log_lik_zero[1,,]
  log_point_one<-par$log_lik_one[1,,]
  
  
  # Prepare the vector of gammas for all candidate variables.
  gammas_home<-gammas_away<-NULL
  
  
  # Step 5
  for (j in 1:(2*data_varsel_zdts$K)){# K candidate variables
    log_O_j<- O_j<-NULL
    # Step 6: Calculation of the logarithm (more convenient) of O_j quantity needed in order to update the gamma indicators.
    if (j<(data_varsel_zdts$K+1)){
      log_O_j<-sum(log_point_one[,j])-sum(log_point_zero[,j])+dnorm(betas_home[j],0,sqrt(data_varsel_zdts$n_games)*post_sd_beta_home[j],log=T)-dnorm(betas_home[j],post_mean_beta_home[j],post_sd_beta_home[j],log=T)
      
    # We specify an upper bound in order to avoid Nan potential problems due to overflow.
      log_O_j[log_O_j>700]<-700
      O_j<-exp(log_O_j)
      
      gammas_home<-c(gammas_home,rbinom(1,1,O_j/(1+O_j)))
      
      
    }else {
      # Step 6: Calculation of the logarithm (more convenient) of O_j quantity needed in order to update the gamma indicators.
      log_O_j<-sum(log_point_one[,j])-sum(log_point_zero[,j])+dnorm(betas_away[j-data_varsel_zdts$K],0,sqrt(data_varsel_zdts$n_games)*post_sd_beta_away[j-data_varsel_zdts$K],log=T)-dnorm(betas_away[j-data_varsel_zdts$K],post_mean_beta_away[j-data_varsel_zdts$K],post_sd_beta_away[j-data_varsel_zdts$K],log=T)
      
    # We specify an upper bound in order to avoid Nan potential problems due to overflow.
      log_O_j[log_O_j>700]<-700
      O_j<-exp(log_O_j)
      
      gammas_away<-c(gammas_away,rbinom(1,1,O_j/(1+O_j)))
      
      
    }
  }
  
  # Step 7: In each one of T iterations, store the values of both posterior gammas and betas values in the Txp matrices
  gammas_home_matrix<-c(gammas_home_matrix,gammas_home)
  betas_home_matrix<-c(betas_home_matrix,betas_home)
  gammas_away_matrix<-c(gammas_away_matrix,gammas_away)
  betas_away_matrix<-c(betas_away_matrix,betas_away)
  
}  

# Save these values in order to manipulate them in terms of convergence diagnostics,, posterior summary statistics, etc...
# Save these values in order to manipulate them in terms of convergence diagnostics, posterior summary statistics, etc...
save(gammas_home_matrix,file="gammas_home_matrix_ta_revised")
save(gammas_away_matrix,file="gammas_away_matrix_ta_revised")

save(betas_home_matrix,file="betas_home_matrix_ta_revised")
save(betas_away_matrix,file="betas_away_matrix_ta_revised")

load(file.choose())

# Store both gammas and betas posterior values after discarding the warmup from T iterations (here, we have chosen 10% of total T iterations).
warmup<-10000
# Each column includes the gammas values of each candidate variable.
final_posterior_values_gammas_home<-matrix(gammas_home_matrix[(data_zdts_ta_skills$K*warmup+1):length(gammas_home_matrix)],
                                           nrow=T-warmup,ncol=data_zdts_ta_skills$K,byrow=TRUE)

colnames(final_posterior_values_gammas_home)<-names(X_home)

# Each column includes the gammas values of each candidate variable.
final_posterior_values_gammas_away<-matrix(gammas_away_matrix[(data_zdts_ta_skills$K*warmup+1):length(gammas_away_matrix)],
                                           nrow=T-warmup,ncol=data_zdts_ta_skills$K,byrow=TRUE)
colnames(final_posterior_values_gammas_away)<-names(X_away)
# Each column includes the betas values of each candidate variable.

final_posterior_values_betas_home<-matrix(betas_home_matrix[(data_zdts_ta_skills$K*warmup+1):length(betas_home_matrix)],
                                          nrow=T-warmup,ncol=data_zdts_ta_skills$K,byrow=TRUE)

colnames(final_posterior_values_gammas_home)<-names(X_home)

final_posterior_values_betas_away<-matrix(betas_away_matrix[(data_zdts_ta_skills$K*warmup+1):length(betas_away_matrix)],
                                          nrow=T-warmup,ncol=data_zdts_ta_skills$K,byrow=TRUE)
colnames(final_posterior_values_gammas_away)<-names(X_away)


# Prepare a dataframe with column names the names of candidate variables.
df_final_posterior_values_gammas_home<-as.data.frame(final_posterior_values_gammas_home)
colnames(df_final_posterior_values_gammas_home)<-names(X_home)

df_final_posterior_values_gammas_away<-as.data.frame(final_posterior_values_gammas_away)
colnames(df_final_posterior_values_gammas_away)<-names(X_away)

# Step 8: Obtain the posterior inclusion probabilities for each one candidate variable
posterior_inclusion_probabilities_home<-round(apply(df_final_posterior_values_gammas_home,2,mean),2)
print(posterior_inclusion_probabilities_home)

# Step 8: Obtain the posterior inclusion probabilities for each one candidate variable
posterior_inclusion_probabilities_away<-round(apply(df_final_posterior_values_gammas_away,2,mean),2)
print(posterior_inclusion_probabilities_away)
