###  Load the r-stan object of corresponding fitted model to obtain the MCMC convergence diagnostics
load(file="revised_ZDTS_only_Skills_after_BVS")

################################################################################
#############---------- MCMC Convergence Diagnostics Visualisation
################################################################################

#----------
#Step 1: Convert the mcmc object to a ggmcmc object for visualisation purposes
ZDTS_only_Skills_after_BVS_parameters<-ggs(mcmc(cbind(mu,home,
                                                      beta_home_away)))

# mu parameter for >1 chains visualisation
gg_ZDTS_only_Skills_after_BVS_mu<- ggs(ZDTS_only_Skills_after_BVS,
                                                      family = "mu")
# Rename parameters for better visualisation in pdf files
par_names=colnames(mcmc_final_posterior_values_gammas_home_away)
par_labels=c("(Home) perfect serves","(Home) very good serves","(Home) failed serves",
             "(Home) poor passes","(Home) failed passes","(Home) perfect att1",
             "(Home) blocked att1","(Home) failed att1","(Home) perfect att2",
             "(Home) blocked att2","(Home) failed att2","(Home) perfect blocks",
             "(Home) block net violations","(Home) failed blocks","(Home) failed settings",
             "(Away) perfect serves","(Away) very good serves","(Away) failed serves",
             "(Away) poor passes","(Away) failed passes","(Away) perfect att1",
             "(Away) blocked att1","(Away) failed att1","(Away) perfect att2",
             "(Away) blocked att2","(Away) failed att2","(Away) perfect blocks",
             "(Away) block net violations","(Away) failed blocks","(Away) failed settings")


P <- data.frame(
        Parameter=par_names,
        Label=par_labels)
# Step2: Save in a single pdf all the necessary plots for the assessment of the convergence 
# (can also produce auto correlation plots, Rhat plots, etc...)

ggmcmc(ZDTS_only_Skills_after_BVS_parameters, 
       file = "converg_gg_revised_ZDTS_only_Skills_after_BVS_parameters.pdf", 
       plot=c( "running"))


# across multiple chains

ggmcmc(gg_ZDTS_only_Skills_after_BVS_mu,
       file = "converg_gg_revised_mu_ZDTS_only_Skills_after_BVS.pdf", plot=c( "running","traceplot",
                                                               "geweke","Rhat","autocorrelation"))


#----------

################################################################################
###  Table with several convergence diagnostics (neff, Rhat, Raftery-Lewis) 
################################################################################

########### Merged Chains Analysis
#------------

## Merged Chains summary statistics of model parameters
zdts_params_summary<-summary(ZDTS_only_Skills_after_BVS,pars=c("mu","home",
                                                               "beta_home","beta_away"))
# Convertion to mcmc objects

zdts_params<-mcmc(cbind(mu,home, beta_home_away))

# Raftery Diagnostics
raftery.diag(zdts_params, q=0.025, r=0.005, s=0.95, converge.eps=0.001)
geweke.diag(zdts_params, frac1=0.1, frac2=0.5)
#heidel.diag Heidelberger and Welch’s convergence diagnostic
heidel.diag(zdts_params, eps=0.1, pvalue=0.05)[,c(1,3)]

#Table with these measures
converg.diag_matrix_zdts_params<-cbind(round(zdts_params_summary$summary[,c(9)]),
                                               round(zdts_params_summary$summary[,c(10)],2),
                                               round(raftery.diag(zdts_params, q=0.025, r=0.005, s=0.95, converge.eps=0.001)$resmatrix,3),
                                       heidel.diag(zdts_params, eps=0.1, pvalue=0.05)[,c(1,3)])
colnames(converg.diag_matrix_zdts_params)[c(1,2,7,8)]<-c("n_eff","Rhat","stationarity_test","p-value")

converg.diag_matrix_zdts_params_final<-converg.diag_matrix_zdts_params[,-c(3:5)]
# LateX table
xtable(converg.diag_matrix_zdts_params_final,digits=2,"MCMC Convergence diagnostics of N_eff, Rhat, Raftery and Lewis and
 and Heidelberger and Welch for the ZDTS 
       model with formulation a (Merged Chains)")
#------------



########### Per chain Analysis
#------------
# Extraction of parameters' values including warmup iterations
params_ZDTS_only_Skills_after_BVS<-rstan::extract(ZDTS_only_Skills_after_BVS,permuted=F)
params_ZDTS_only_Skills_after_BVS<-as.data.frame(params_ZDTS_only_Skills_after_BVS)

mu_ZDTS_only_Skills_after_BVS<-rstan::extract(ZDTS_only_Skills_after_BVS,permuted=F,pars="mu")
mu_ZDTS_only_Skills_after_BVS<-as.data.frame(mu_ZDTS_only_Skills_after_BVS)
mu_ZDTS_only_Skills_after_BVS_chain1<-mu_ZDTS_only_Skills_after_BVS[,1]

home_ZDTS_only_Skills_after_BVS<-rstan::extract(ZDTS_only_Skills_after_BVS,permuted=F,pars="home")
home_ZDTS_only_Skills_after_BVS<-as.data.frame(home_ZDTS_only_Skills_after_BVS)
home_ZDTS_only_Skills_after_BVS_chain1<-home_ZDTS_only_Skills_after_BVS[,1]

beta_home_ZDTS_only_Skills_after_BVS<-rstan::extract(ZDTS_only_Skills_after_BVS,permuted=F,pars="beta_home")
beta_home_ZDTS_only_Skills_after_BVS<-as.data.frame(beta_home_ZDTS_only_Skills_after_BVS)

beta_away_ZDTS_only_Skills_after_BVS<-rstan::extract(ZDTS_only_Skills_after_BVS,permuted=F,pars="beta_away")
beta_away_ZDTS_only_Skills_after_BVS<-as.data.frame(beta_away_ZDTS_only_Skills_after_BVS)

# All together model parameters

all_params_ZDTS_only_Skills_after_BVS<-cbind(mu_ZDTS_only_Skills_after_BVS,home_ZDTS_only_Skills_after_BVS,
                                      beta_home_ZDTS_only_Skills_after_BVS,beta_away_ZDTS_only_Skills_after_BVS)


# all_params_ZDTS_only_Skills_after_BVS<-cbind(mu_ZDTS_only_Skills_after_BVS,home_ZDTS_only_Skills_after_BVS)
# Convertion to mcmc objects

zdts_params<-mcmc(all_params_ZDTS_only_Skills_after_BVS)

# Raftery Diagnostics
raftery.diag(zdts_params, q=0.025, r=0.005, s=0.95, converge.eps=0.001)
# gelman.diag(zdts_params, confidence = 0.95)
geweke.diag(zdts_params, frac1=0.1, frac2=0.5)
#heidel.diag Heidelberger and Welch’s convergence diagnostic
heidel.diag(zdts_params, eps=0.1, pvalue=0.05)[,c(1,3)]

#Table with these measures
converg.diag_matrix_zdts_params<-cbind(round(zdts_params_summary$summary[,c(9)]),
                                       round(zdts_params_summary$summary[,c(10)],2),
                                       round(raftery.diag(zdts_params, q=0.025, r=0.005, s=0.95, converge.eps=0.001)$resmatrix,3),
                                       heidel.diag(zdts_params, eps=0.1, pvalue=0.05)[,c(1,3)])
colnames(converg.diag_matrix_zdts_params)[c(1,2,7,8)]<-c("n_eff","Rhat","stationarity_test","p-value")
converg.diag_matrix_zdts_params_final<-converg.diag_matrix_zdts_params[,-c(3:5)]

# LateX table
xtable(converg.diag_matrix_zdts_params_final,digits=2,"MCMC Convergence diagnostics of N_eff, Rhat, Raftery and Lewis and
 and Heidelberger and Welch for the ZDTS 
       model with formulation a (Per Chain)")
#------------

