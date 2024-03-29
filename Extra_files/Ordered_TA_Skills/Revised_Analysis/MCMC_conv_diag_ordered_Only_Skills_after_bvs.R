####------GG-Plots for Convergence Diagnostics

#----Step 1: Convert the mcmc object to a ggmcmc object
ordered_only_skills_after_bvs_parameters<-ggs(mcmc(cbind(temp_intercepts,beta)))
gg_ordered_only_skills_after_bvs_beta <- ggs(ordered_skills_after_BVS,family = "beta")
gg_ordered_only_skills_after_bvs_temp_Intercept<- ggs(ordered_skills_after_BVS,
                                                      family = "temp_Intercept")
gg_ordered_only_skills_after_bvs_first_temp_Intercept<- ggs(ordered_skills_after_BVS,
                                                            family = "first_temp_Intercept")

#----Step2: Save in a single pdf all the necessary plots for the assessment of the convergence


ggmcmc(ordered_only_skills_after_bvs_parameters, 
       file = "converg_gg_revised_ordered_only_skills_after_bvs_parameters.pdf", plot=c( "running","traceplot",
                                                                    "geweke","Rhat","autocorrelation"))

#----Separately across multiple chains
ggmcmc(gg_ordered_only_skills_after_bvs_first_temp_Intercept,
       file = "converg_gg_revised_first_temp_Intercept_ordered_only_skills.pdf", plot=c( "running","traceplot",
                                                                              "geweke","Rhat","autocorrelation"))


ggmcmc(gg_ordered_only_skills_after_bvs_beta,
       file = "converg_gg_revised_betas_ordered_only_skills.pdf", plot=c( "running","traceplot",
                                                               "geweke","Rhat","autocorrelation"))

ggmcmc(gg_ordered_only_skills_after_bvs_temp_Intercept,
       file = "converg_gg_revised_intercepts_ordered_only_skills.pdf", plot=c( "running","traceplot",
                                                                    "geweke","Rhat","autocorrelation"))
