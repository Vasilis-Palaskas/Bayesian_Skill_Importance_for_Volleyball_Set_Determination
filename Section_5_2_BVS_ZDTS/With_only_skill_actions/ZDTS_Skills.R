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

#### Standardization of the numeric features (frequencies of skill actions) to avoid numeric overflow and speed mcmc convergence

X_home_std<-data.frame(scale(X_home,center=T,scale=T) )
X_away_std<-data.frame(scale(X_away,center=T,scale=T) )


###########################################################################################
###############--------Hybrid R–Stan Implementation of Gibbs Variable Selection (Algorithm 1 in the paper)
###########################################################################################

####Step 0: MCMC Pilot run in order to obtain the empirical mean and standard deviation of candidate parameters
#--------


# Create the datalist for running pilot run of full ordered multinomial model with only skill actions (remove both poor and failed pass from our analysis)
data_zdts_only_skills<-list(c_thres=2,c_std=5,
                            n_games=dim(data_by_sets)[1],
                       n_teams= nteams,
                       X_home=X_home_std,X_away=X_away_std,K=ncol(X_home_std),
                       home_sets=data_by_sets$home_sets,
                       away_sets=data_by_sets$away_sets)


setwd("C:/Users/vasileios palaskas/DocumentsGitHub/Bayesian_Skill_Importance_for_Volleyball_Set_Determination/Section_5_2_BVS_ZDTS/With_only_skill_actions")
full_zdts_only_skills_revised<-stan("full_zdts_only_skills.stan",
                             data=data_zdts_only_skills,chains=2,init_r=0.5,
                             iter=10000,warmup=2000,cores=2)### ## Run full_zdts_only_skills.stan

save(full_zdts_only_skills_revised,file="full_zdts_only_skills_revised")

load("full_zdts_only_skills_revised")

# Extract the posterior summary statistics of both candidate variables' parameters and rest of other parameters.
betas_summary<-summary(full_zdts_only_skills_revised, pars = c("beta_home","beta_away"))$summary
mu_summary<-summary(full_zdts_only_skills_revised, pars = c("mu"))$summary
home_summary<-summary(full_zdts_only_skills_revised, pars = c("home"))$summary


# Use their posterior means and standard deviations (from pilot run)

post_mean_beta_home<-betas_summary[1:(dim(X_home)[2]),1]####posterior mean for beta home
post_mean_beta_away<-betas_summary[(dim(X_home)[2]+1):(2*dim(X_home)[2]),1]####posterior mean for beta
post_sd_beta_home<-betas_summary[1:(dim(X_home)[2]),3]### posterior sd for beta
post_sd_beta_away<-betas_summary[(dim(X_home)[2]+1):(2*dim(X_home)[2]),3]### posterior sd for beta
post_mean_beta<-c(post_mean_beta_home,post_mean_beta_away)
post_sd_beta<-c(post_sd_beta_home,post_sd_beta_away)

#--------

# Step 1: Initialization of the model parameters.
# Use their posterior means and standard deviations for both initial values specification and prior specification.

gammas_home<-rep(1,(dim(X_home)[2]))
gammas_away<-rep(1,(dim(X_home)[2]))
mu<-mu_summary[,1]
home<-home_summary[,1]
betas_home<-post_mean_beta_home
betas_away<-post_mean_beta_away


# Prepare the matrices with the posterior samples of dimension Txp (p=K--> during algorithm iterations) 
#  for all gammas and betas coefficients related to the skill actions , respectively.
gammas_home_matrix<-gammas_away_matrix<-betas_home_matrix<-betas_away_matrix<-NULL
T<-30000 # Total MCMC iterations run for the (BVS)

for (i in 1:T){# Step 2  
  print(i)
  
  # Step 3: Data input needed for running the model through RStan.
  data_varsel_zdts<-list(n_teams=data_zdts_only_skills$n_teams,n_games=data_zdts_only_skills$n_games,
                         c_thres=2,c_std=5,
                         home_sets=data_zdts_only_skills$home_sets,
                         away_sets=data_zdts_only_skills$away_sets,
                         X_home=as.matrix(X_home_std),X_away=as.matrix(X_away_std),
                         K=ncol(X_home_std),
                         gammas_home=gammas_home,gammas_away=gammas_away,
                        post_mean_beta_home=post_mean_beta_home,post_mean_beta_away=post_mean_beta_away,
                        post_sd_beta_home=post_sd_beta_home,post_sd_beta_away=post_sd_beta_away)
  
  # Step 4: Run the model through R-Stan for one sampling iteration (20 warm up and 21 total iterations,21-20=1 sampling iteration) 
  # in order to update the betas from the full conditional posterior distributions. 
  # In each MCMC iteration, Use the previous iteration's parameter values as initial parameter values 
  n_chains <- 1
  initf2 <- function(chain_id = 1) {
	    list(beta_home=betas_home,beta_away=betas_away, mu=mu, home=home)
      }
  init_ll <- lapply(1:n_chains, function(id) initf2(chain_id = id))
  
  zdts_volley_skills_all<-stan("ZDTS_BVS_Skills.stan",
                               data=data_varsel_zdts,chains=n_chains,
                               iter=21,warmup=20,
                               init= init_ll,
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
      log_O_j<-sum(log_point_one[,j])-sum(log_point_zero[,j])+
        dnorm(betas_away[j-data_varsel_zdts$K],0,sqrt(data_varsel_zdts$n_games)*post_sd_beta_away[j-data_varsel_zdts$K],log=T)-
        dnorm(betas_away[j-data_varsel_zdts$K],post_mean_beta_away[j-data_varsel_zdts$K],post_sd_beta_away[j-data_varsel_zdts$K],log=T)
      
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

# Save the posterior densities of both gammas and betas for further manipulation in terms of convergence diagnostics, posterior summary statistics, etc...
save(gammas_home_matrix,file="gammas_home_matrix_revised")
save(gammas_away_matrix,file="gammas_away_matrix_revised")

save(betas_home_matrix,file="betas_home_matrix_revised")
save(betas_away_matrix,file="betas_away_matrix_revised")

# Store both gammas and betas posterior values after discarding the warmup from T iterations (here, we have chosen 10% of total T iterations).
warmup<-10000
# Each column includes the gammas values of each candidate variable.
final_posterior_values_gammas_home<-matrix(gammas_home_matrix[(data_zdts_only_skills$K*warmup+1):length(gammas_home_matrix)],
                                           nrow=T-warmup,ncol=data_zdts_only_skills$K,byrow=TRUE)

colnames(final_posterior_values_gammas_home)<-names(X_home)

# Each column includes the gammas values of each candidate variable.
final_posterior_values_gammas_away<-matrix(gammas_away_matrix[(data_zdts_only_skills$K*warmup+1):length(gammas_away_matrix)],
                                           nrow=T-warmup,ncol=data_zdts_only_skills$K,byrow=TRUE)
colnames(final_posterior_values_gammas_away)<-names(X_away)
# Each column includes the betas values of each candidate variable.

final_posterior_values_betas_home<-matrix(betas_home_matrix[(data_zdts_only_skills$K*warmup+1):length(betas_home_matrix)],
                                          nrow=T-warmup,ncol=data_zdts_only_skills$K,byrow=TRUE)

colnames(final_posterior_values_gammas_home)<-names(X_home)

final_posterior_values_betas_away<-matrix(betas_away_matrix[(data_zdts_only_skills$K*warmup+1):length(betas_away_matrix)],
                                          nrow=T-warmup,ncol=data_zdts_only_skills$K,byrow=TRUE)
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

