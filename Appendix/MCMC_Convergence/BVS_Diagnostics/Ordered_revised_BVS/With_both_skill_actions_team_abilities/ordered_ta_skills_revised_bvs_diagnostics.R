#  Load the posterior densities (objects) of both gammas and betas for further manipulation in terms of convergence diagnostics, posterior summary statistics, etc...

load("BVS_Ordered_TA_Skills_gammas")# saved in folder of my desktop
load("BVS_Ordered_TA_Skills_betas")# saved in folder of my desktop

# Store both gammas and betas posterior values after discarding the warmup from T iterations (here, we have chosen to discard the 20% of total T iterations).
warmup<-6000
# Each column includes the gammas values of each candidate variable.
final_posterior_values_gammas<-matrix(gammas_matrix[(dataList$K*warmup+1):length(gammas_matrix)],
                                      nrow=T-warmup,ncol=dataList$K,byrow=TRUE)
# Each column includes the gammas values of each candidate variable.
final_posterior_values_betas<-matrix(betas_matrix[(dataList$K*warmup+1):length(betas_matrix)],
                                     nrow=T-warmup,ncol=dataList$K,byrow=TRUE)
# Prepare a dataframe with column names the names of candidate variables.
df_final_posterior_values_gammas<-as.data.frame(final_posterior_values_gammas)
# names(dataList$X)<-c("perfect serve","very good serve","failed serve"," perfect pass",
#                      "very good pass","poor pass","failed pass","perfect att1","blocked att1",
#                      "failed att1","perfect att2","blocked att2","failed att2","perfect block",
#                      "block net violation","failed block","failed setting")
colnames(df_final_posterior_values_gammas)<-names(dataList$X)
# Step 8: Obtain the posterior inclusion probabilities for each one candidate variable
posterior_inclusion_probabilities<-round(apply(df_final_posterior_values_gammas,2,mean),3)
print(posterior_inclusion_probabilities)

### MCMC Convergence Checking
# 
# a) Firstly, for gammas and betas indicators
# 
## convert them to a mcmc pobject in terms of our convenience
mcmc_final_posterior_values_gammas<-as.mcmc(final_posterior_values_gammas)
colnames(mcmc_final_posterior_values_gammas)<-names(dataList$X)
mcmc_final_posterior_values_betas<-as.mcmc(final_posterior_values_betas)
colnames(mcmc_final_posterior_values_betas)<-names(dataList$X)


####------GG-Plots for Convergence Diagnostics

#----Step 1: Convert the mcmc object to a ggmcmc object
gg_posterior_values_betas<- ggs(mcmc_final_posterior_values_betas)

gg_posterior_values_gammas<- ggs(mcmc_final_posterior_values_gammas)

#----Step2: Save in a single pdf all the necessary plots for the assessment of the convergence
ggmcmc(gg_posterior_values_betas, 
       file = "converg_betas_revised_ordered_bvs_ta_skills.pdf", plot=c( "running",
                                                                 "geweke","Rhat","autocorrelation"))



ggmcmc(gg_posterior_values_gammas, 
       file = "converg_gammas_revised_ordered_bvs_ta_skills.pdf", plot=c( "running"
                                                                  ))

