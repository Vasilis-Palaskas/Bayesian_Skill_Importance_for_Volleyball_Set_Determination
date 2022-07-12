

#  Load the posterior densities (objects) of both gammas and betas for further manipulation in terms of convergence diagnostics, posterior summary statistics, etc...
load("BVS_Ordered_Skills_gammas_collinearity")# saved in folder of my desktop
load("BVS_Ordered_Skills_betas_collinearity")# saved in folder of my desktop

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



### MCMC Convergence Checking
# 
# a) Firstly, for gammas and betas indicators
# 
# convert them to a mcmc pobject in terms of our convenience
mcmc_final_posterior_values_gammas<-as.mcmc(final_posterior_values_gammas)
colnames(mcmc_final_posterior_values_gammas)<-names(dataList$X)
mcmc_final_posterior_values_betas<-as.mcmc(final_posterior_values_betas)
colnames(mcmc_final_posterior_values_betas)<-names(dataList$X)


################################################################################
#############---------- MCMC Convergence Diagnostics Visualisation
################################################################################
#----------

#----Step 1: Convert the mcmc object to a ggmcmc object
gg_posterior_values_betas<- ggs(mcmc_final_posterior_values_betas)

gg_posterior_values_gammas<- ggs(mcmc_final_posterior_values_gammas)

#----Step2: Save in a single pdf all the necessary plots for the assessment of the convergence
ggmcmc(gg_posterior_values_betas, 
       file = "converg_betas_revised_ordered_bvs_skills.pdf", plot=c( "running",
                                                              "geweke","Rhat","autocorrelation"))

ggmcmc(gg_posterior_values_gammas, 
       file = "converg_gammas_revised_ordered_bvs_skills.pdf", plot=c( "running"))

#----------


################################################################################
###  Table with several convergence diagnostics (neff, Rhat, Raftery-Lewis) 
################################################################################

########### Merged Chains Analysis
#------------


## Merged Chains summary statistics of model parameters
# ordered_params_summary<-summary(ordered_skills_after_BVS_model,pars=c(
#   "temp_Intercept","beta"))
# Convertion to mcmc objects

ordered_params<-mcmc_final_posterior_values_gammas

# Raftery Diagnostics
raftery.diag(ordered_params, q=0.025, r=0.005, s=0.95, converge.eps=0.001)
geweke.diag(ordered_params, frac1=0.1, frac2=0.5)
#heidel.diag Heidelberger and Welch’s convergence diagnostic
heidel.diag(ordered_params, eps=0.1, pvalue=0.05)[,c(1,3)]

#Table with these measures
converg.diag_matrix_ordered_params<-cbind(round(ordered_params_summary$summary[,c(9)]),
                                          round(ordered_params_summary$summary[,c(10)],2),
                                          round(raftery.diag(ordered_params, q=0.025, r=0.005, s=0.95, converge.eps=0.001)$resmatrix,3),
                                          heidel.diag(ordered_params, eps=0.1, pvalue=0.05)[,c(1,3)])
colnames(converg.diag_matrix_ordered_params)[c(1,2,7,8)]<-c("n_eff","Rhat","stationarity_test","p-value")

converg.diag_matrix_ordered_params_final<-converg.diag_matrix_ordered_params[,-c(3:5)]
# LateX table
xtable(converg.diag_matrix_ordered_params_final,digits=2,"MCMC Convergence diagnostics of N_eff, Rhat, Raftery and Lewis and
 and Heidelberger and Welch for the ordered-multinomial
       model with formulation a (Merged Chains)")
#------------