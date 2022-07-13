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

# Correlations of our response variable and several skill actions
cor(data_by_sets$sets_difference,X_home_diff["failed_passes"])
cor(data_by_sets$sets_difference,X_home_diff["perfect_serves"])
cor(data_by_sets$sets_difference,X_home_diff["very_good_serves"])
cor(data_by_sets$sets_difference,X_home_diff["poor_passes"])
cor(data_by_sets$sets_difference,X_home_diff["perfect_blocks"])
cor(data_by_sets$sets_difference,X_home_diff["blocked_att1"])

corrplot(X_home_diff, method = 'number') # colorful number
cor_table<-round(cor(X_home_diff),5)
# which(abs(cor_table)>0.80,arr.ind = T)
cor_table

#----Collinearity issue checking
data_full<-data.frame(Y=data_by_sets$sets_difference_factor,X_home_diff)
mfull<-lm(Y~.,data=data_full)
vif_table_full<-as.matrix(round(vif(mfull),1)) 
vif(mfull)

# The most important correlations between explanatory features/variables which caused  unexpected results in Bayesian variable selection since essentially
# you may end up with multiple models with similar posterior probabilities and modes. 
cor(X_home_diff["poor_passes"],X_home_diff["very_good_serves"])
cor(X_home_diff["failed_passes"],X_home_diff["perfect_serves"])
# Both poor and failed passes will be removed in order to avoid as much as multi-collinearity and we prefer serves instead of passes
# since they have more intense impact in the final outcome of the matches in terms of set difference (our response variable)

###########################################################################################
###############--------Hybrid R–Stan Implementation of Gibbs Variable Selection (Algorithm 1 in the paper)
###########################################################################################

####Step 0: MCMC Pilot run in order to obtain the empirical mean and standard deviation of candidate parameters

#--------

# Create the datalist for running pilot run of full ordered multinomial model with only skill actions (remove both poor and failed pass from our analysis)
dataList<-list(Y=data_by_sets$sets_difference_factor,X=X_home_diff[,!colnames(X_home_diff)%in%c(
  "failed_passes","poor_passes")],n_teams=length(levels(data_by_sets$home_Team)),
               N=dim(data_by_sets)[1],K=ncol(X_home_diff[,!colnames(X_home_diff)%in%c(
                 "failed_passes","poor_passes")]),ncat=6)

## Run Full_ordered_skills.stan (full model)
Full_ordered_skills<-stan(file.choose(),iter=10000, warmup=2000,chains=2,thin=2,
                          data=dataList,control=list(max_treedepth=15),cores=2)

save(Full_ordered_skills,file="Full_ordered_skills_collinearity")
# Load the results from the full ordered logistic model (with all candidate variables).
load(file="Full_ordered_skills_collinearity")

# Extract the posterior summary statistics of both candidate variables' parameters and rest of other parameters.
betas_summary<-summary(Full_ordered_skills,pars = c("beta"))$summary
intercept_summary<-summary(Full_ordered_skills, pars = c("temp_Intercept"))$summary

# Extract posterior means and std. deviations from the candidate parameters from the variable selection
post_mean_betas<-betas_summary[1:dataList$K,1]
post_sd_betas<-betas_summary[1:dataList$K,3]
#--------

# Step 1: Initialization of the model parameters.
# Use their posterior means and standard deviations for both initial values specification and prior specification.

#------------
gammas<-rep(1,dataList$K)# All the candidate variables included in the model
temp_Intercept<-intercept_summary[,1]
betas<-post_mean_betas

# Prepare the vectors with the posterior samples of dimension Txp (p=K during algorithm iterations) for all gammas and betas coefficients , respectively.
gammas_matrix<-betas_matrix<-NULL

setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/Section_5_1_BVS_Ordered/With_only_skill_actions")#------------


T<-10000 # Total MCMC iterations

for (i in 1:T){# Step 2  
  print(i)
  # Step 3: Data input needed for running the model through RStan.
  data_varsel<-list(Y=dataList$Y,X=dataList$X,
                    N=dataList$N,K=dataList$K,n_teams=dataList$n_teams,
                    ncat=6,gammas=gammas,post_mean_betas=post_mean_betas,
                    post_sd_betas=post_sd_betas)
  
  # Step 4:Run the model through RStan for one sampling iteration (20 warm up and 21 total iterations, 21-20=1 sampling iteration) 
  # in order to update the betas from the full conditional posterior distributions. 
  # Use the previous iteration's parameter values as initial parameter values so MCMC Algorithm can begin properly.
  ord_volley_skills_all<-stan("Ordered_BVS_Skills.stan",
                              data=data_varsel,chains=1,
                              iter=21,warmup=20,init=list(list(betas=betas,temp_Intercept=temp_Intercept)),
                              control=list(adapt_window=15,adapt_init_buffer=3,adapt_term_buffer=2))
  
  # Initialize the log-likelihood for both cases 0 and 1 for gammas indicators/coefficients.
  log_point_zero<-matrix(NA,nrow=data_varsel$N,ncol=data_varsel$K) # matrix with log likelihoods when gamma[j]=0
  log_point_one<-matrix(NA,nrow=data_varsel$N,ncol=data_varsel$K)  #  matrix with log likelihoods when gamma[j]=1
  
  # Extract both model's parameters and log-likelihoods for both cases of gammas indicators.
  par<-rstan::extract(ord_volley_skills_all)
  temp_Intercept<-par$temp_Intercept[1,]
  betas<-par$betas[1,]
  log_point_zero<-par$log_lik_zero[1,,]
  log_point_one<-par$log_lik_one[1,,]
  
  # Prepare the vector of gammas for all candidate variables.
  gammas<-NULL
  
  # Step 5
  for (j in 1:data_varsel$K){# K candidate variables
    log_O_j<-O_j<-NULL
    
    # Step 6: Calculation of the logarithm (more convenient) of O_j quantity needed in order to update the gamma indicators.
    
    log_O_j<-sum(log_point_one[,j])-sum(log_point_zero[,j])+
      dnorm(betas[j],0,sqrt(data_varsel$N)*(post_sd_betas[j]),log=T)-
      dnorm(betas[j],post_mean_betas[j],post_sd_betas[j],log=T)
    # We specify an upper bound in order to avoid Nan potential problems due to overflow.
    log_O_j[log_O_j>700]<-700
    O_j<-exp(log_O_j)
    
    gammas<-c(gammas,rbinom(1,1,O_j/(1+O_j)))
    
  }
  
  # Step 7: In each one of T iterations, store the values of both posterior gammas and betas coefficients in the Txp matrices
  gammas_matrix<-c(gammas_matrix,gammas)
  betas_matrix<-c(betas_matrix,betas)
}

# Save the posterior densities of both gammas and betas for further manipulation in terms of convergence diagnostics, posterior summary statistics, etc...
save(gammas_matrix,file="BVS_Ordered_Skills_gammas_collinearity")
save(betas_matrix,file="BVS_Ordered_Skills_betas_collinearity")
load("BVS_Ordered_Skills_gammas_collinearity")
load("BVS_Ordered_Skills_betas_collinearity")
gammas_matrix<-gammas_matrix[ c(1:(dataList$K*T))]
betas_matrix<-betas_matrix[ c(1:(dataList$K*T))]

# Store both gammas and betas posterior values after discarding the warmup from T iterations (here, we have chosen to discard the 20% of total T iterations).
warmup<-3000

# Each column includes the gammas values of each candidate variable.
final_posterior_values_gammas<-matrix(gammas_matrix[(dataList$K*warmup+1):length(gammas_matrix)],
                                      nrow=T-warmup,ncol=dataList$K,byrow=TRUE)
# Each column includes the gammas values of each candidate variable.
final_posterior_values_betas<-matrix(betas_matrix[(dataList$K*warmup+1):length(betas_matrix)],
                                     nrow=T-warmup,ncol=dataList$K,byrow=TRUE)
# Prepare a dataframe by assigning in the variables names the corresponding column names.
df_final_posterior_values_gammas<-as.data.frame(final_posterior_values_gammas)

colnames(df_final_posterior_values_gammas)<-names(dataList$X)
# Step 8: Obtain the posterior inclusion probabilities for each one candidate variable
posterior_inclusion_probabilities<-round(apply(df_final_posterior_values_gammas,2,mean),2)
print(posterior_inclusion_probabilities)

