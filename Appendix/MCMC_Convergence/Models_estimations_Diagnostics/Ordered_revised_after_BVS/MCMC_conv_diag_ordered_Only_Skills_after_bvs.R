###  Load the r-stan object of corresponding fitted model to obtain the MCMC convergence diagnostics
load("revised_ordered_skills_after_BVS")

################################################################################
#############---------- MCMC Convergence Diagnostics Visualisation
################################################################################

#----------


#Step 1: Convert the mcmc object to a ggmcmc object for visualisation purposes
ordered_only_skills_after_bvs_parameters<-ggs(mcmc(cbind(temp_intercepts,beta)))

# beta parameters for >1 chains visualisation

# gg_ordered_only_skills_after_bvs_beta <- ggs(ordered_skills_after_BVS,family = "beta")


# Step2: Save in a single pdf all the necessary plots for the assessment of the convergence 
# (can also produce auto correlation plots, Rhat plots, etc...)

# ggmcmc(ordered_only_skills_after_bvs_parameters, 
#        file = "converg_gg_revised_ordered_only_skills_after_bvs_parameters.pdf", plot=c( "running"))
pdf(file="converg_gg_revised_ordered_only_skills_after_bvs_parameters.pdf", width =14.5, height =8.5)
ggs_running(ordered_only_skills_after_bvs_parameters)+
        facet_wrap(~ Parameter, scales = "free" )+  ggtitle("Parameters of the ordered multinomial (Formulation b)")
dev.off()  
# across multiple chains
# 
# ggmcmc(gg_ordered_only_skills_after_bvs_beta,
#        file = "converg_gg_revised_betas_ordered_only_skills.pdf", plot=c( "running","traceplot","geweke","Rhat","autocorrelation"))
#----------


################################################################################
###  Table with several convergence diagnostics (neff, Rhat, Raftery-Lewis) 
################################################################################

########### Merged Chains Analysis
#------------


## Merged Chains summary statistics of model parameters
ordered_params_summary<-summary(ordered_skills_after_BVS_model,pars=c(
                                                                   "temp_Intercept","beta"))
# Convertion to mcmc objects

ordered_params<-mcmc(cbind(temp_intercepts, beta))

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



########### Per chain Analysis
#------------
# Extraction of parameters' values including warmup iterations
params_ordered_skills_after_BVS_model<-rstan::extract(ordered_skills_after_BVS_model,permuted=F)
params_ordered_skills_after_BVS_model<-as.data.frame(params_ordered_skills_after_BVS_model)

temp_Intercept_ordered_skills_after_BVS_model<-rstan::extract(ordered_skills_after_BVS_model,permuted=F,pars="temp_Intercept")
temp_Intercept_ordered_skills_after_BVS_model<-as.data.frame(temp_Intercept_ordered_skills_after_BVS_model)

beta_ordered_skills_after_BVS_model<-rstan::extract(ordered_skills_after_BVS_model,permuted=F,pars="beta")
beta_ordered_skills_after_BVS_model<-as.data.frame(beta_ordered_skills_after_BVS_model)



# All together model parameters

all_params_ordered_skills_after_BVS_model<-cbind(temp_Intercept_ordered_skills_after_BVS_model,
                                                 beta_ordered_skills_after_BVS_model)


# all_params_ordered_skills_after_BVS_model<-cbind(mu_ordered_skills_after_BVS_model,home_ordered_skills_after_BVS_model)
# Convertion to mcmc objects

ordered_params<-mcmc(all_params_ordered_skills_after_BVS_model)

# Raftery Diagnostics
raftery.diag(ordered_params, q=0.025, r=0.005, s=0.95, converge.eps=0.001)
# gelman.diag(ordered_params, confidence = 0.95)
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
 and Heidelberger and Welch for the ZDTS 
       model with formulation a (Per Chain)")
#------------


