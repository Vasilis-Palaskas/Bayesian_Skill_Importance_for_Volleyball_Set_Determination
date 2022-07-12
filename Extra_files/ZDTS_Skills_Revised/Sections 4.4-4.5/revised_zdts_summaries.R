
#------------------Posterior summary statistics of-------------------------------------------------------------------------------------------
###---- the mu, home,exp(mu),exp(home), l1-l2, Ezdts (Table ++)--------###

##Extracting the parameter values 
#---Set appropriate working directory
# setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_Volleyball/ZDTS_Skills/Sections 4.3-4.5")
load("C:\\Users\\vasileios palaskas\\Dropbox\\Egidi_Ntzoufras_Palaskas_Drikos_BVS1 Paper\\Models_Outputs_4_5_sections\\ZDTS\\revised\\Skills\\revised_ZDTS_only_Skills_after_BVS")##"revised_ZDTS_only_Skills_after_BVS"

###--------------Posterior Summary Sonlytistics-Analysis------------------------########

#####----------------------Posterior summary----------------------------######


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
print(ZDTS_only_Skills_after_BVS,
      pars=c("mu","home",
             "beta_home","beta_away","dev"),probs = c(0.025,0.5,0.975), digits=2)


#-Access summary statistics
# df_of_draws <- as.data.frame(ZDTS_only_Skills_after_BVS)
# print(colnames(df_of_draws))
# fit_summary <- summary(ZDTS_only_Skills_after_BVS)
betas_summary <- summary(ZDTS_only_Skills_after_BVS, pars = c("mu","home","beta_home","beta_away"), probs = c(0.025, 0.5,0.95))$summary
print(round(betas_summary[,c(1,3,4,5,6)],2))
betas_summary_main<-round(betas_summary[,c(1,3,4,5,6)],2)
xtable(betas_summary_main)

## Extraction of model parameters
params_ZDTS_only_Skills_after_BVS<-rstan::extract(ZDTS_only_Skills_after_BVS)
# covariates
mu_ZDTS_only_Skills_after_BVS<-params_ZDTS_only_Skills_after_BVS$mu
home_ZDTS_only_Skills_after_BVS<-params_ZDTS_only_Skills_after_BVS$home
beta_home_params_ZDTS_only_Skills_after_BVS<-params_ZDTS_only_Skills_after_BVS$beta_home
beta_away_params_ZDTS_only_Skills_after_BVS<-params_ZDTS_only_Skills_after_BVS$beta_away
# l1, l2 parameters
lambda1_star<-params_ZDTS_only_Skills_after_BVS$lambda1_star
lambda2_star<-params_ZDTS_only_Skills_after_BVS$lambda2_star








####MU

mean_mu<-mean(params_ZDTS_only_Skills_after_BVS$mu)
median_mu<-median(params_ZDTS_only_Skills_after_BVS$mu)
sd_mu<-sd(params_ZDTS_only_Skills_after_BVS$mu)
quantile(params_ZDTS_only_Skills_after_BVS$mu,probs=c(0.025,0.975))

lower_interval_mu<-mean_mu-sd(params_ZDTS_only_Skills_after_BVS$mu)
upper_interval_mu<-mean_mu+sd(params_ZDTS_only_Skills_after_BVS$mu)
mu_posterior<-c(mean_mu,median_mu,sd_mu,quantile(params_ZDTS_only_Skills_after_BVS$mu,probs=c(0.025,0.975)))
print(mu_posterior)




## mu: exponential scale
exp_mu<-exp(params_ZDTS_only_Skills_after_BVS$mu)
mean_exp_mu<-mean(exp_mu)
median_exp_mu<-median(exp_mu)
sd_exp_mu<-sd(exp_mu)
quantile(exp_mu,probs=c(0.025,0.975))

lower_interval_exp_mu<-mean_exp_mu-sd(exp_mu)
upper_interval_exp_mu<-mean_exp_mu+sd(exp_mu)

exp_mu_posterior<-c(mean_exp_mu,median_exp_mu,sd_exp_mu,quantile(exp_mu,probs=c(0.025,0.975)))
print(exp_mu_posterior)

##home

mean_home<-mean(params_ZDTS_only_Skills_after_BVS$home)
median_home<-median(params_ZDTS_only_Skills_after_BVS$home)
sd_home<-sd(params_ZDTS_only_Skills_after_BVS$home)
quantile(params_ZDTS_only_Skills_after_BVS$home,probs=c(0.025,0.975))

lower_interval_home<-mean_home-sd(params_ZDTS_only_Skills_after_BVS$home)
upper_interval_home<-mean_home+sd(params_ZDTS_only_Skills_after_BVS$home)

home_posterior<-c(mean_home,median_home,sd_home,quantile(params_ZDTS_only_Skills_after_BVS$home,probs=c(0.025,0.975))
)
home_posterior
# Approx. SESD=0.61*home
params_ZDTS_only_Skills_after_BVS$approx_SESD<-exp(0.61*params_ZDTS_only_Skills_after_BVS$home)
mean_approx_SESD<-mean(params_ZDTS_only_Skills_after_BVS$approx_SESD)
median_approx_SESD<-median(params_ZDTS_only_Skills_after_BVS$approx_SESD)
sd_approx_SESD<-sd(params_ZDTS_only_Skills_after_BVS$approx_SESD)
quantile(params_ZDTS_only_Skills_after_BVS$approx_SESD,probs=c(0.025,0.975))

lower_interval_home<-mean_approx_SESD-sd(params_ZDTS_only_Skills_after_BVS$approx_SESD)
upper_interval_home<-mean_approx_SESD+sd(params_ZDTS_only_Skills_after_BVS$approx_SESD)

approx_SESD_posterior<-c(mean_approx_SESD,median_approx_SESD,sd_approx_SESD,quantile(params_ZDTS_only_Skills_after_BVS$approx_SESD,probs=c(0.025,0.975))
)
approx_SESD_posterior
##home: exponential scale
exp_home<-exp(params_ZDTS_only_Skills_after_BVS$home)
mean_exp_home<-mean(exp_home)
median_exp_home<-median(exp_home)
quantile(exp_home,probs=c(0.025,0.975))


lower_interval_exp_home<-mean_exp_home-sd(exp_home)
upper_interval_exp_home<-mean_exp_home+sd(exp_home)

exp_home_posterior<-c(mean_exp_home,median_exp_home,sd_home,quantile(exp_home,probs=c(0.025,0.975)))
exp_home_posterior


####--------- Computation of l1-l2, Ezdts between 
####---------   equal teams'strength
expec_value_zdts_equal<-expec_value_used_equal<-NULL
#### lambda1_star - lambda2_star between two equal strength teams

lambda1_star_equal<-exp(params_ZDTS_only_Skills_after_BVS$mu+params_ZDTS_only_Skills_after_BVS$home)
lambda2_star_equal<-exp(params_ZDTS_only_Skills_after_BVS$mu)

for (i in 1:dim(lambda1_star_equal)[1]){
  expec_value_zdts_nomin<-besselI(2*sqrt(lambda1_star_equal[i]*lambda2_star_equal[i]),1)+
    (2*besselI(2*sqrt(lambda1_star_equal[i]*lambda2_star_equal[i]),2)*(lambda1_star_equal[i]+lambda2_star_equal[i]))/
    (lambda1_star_equal[i]*lambda2_star_equal[i])^(1/2)+(3*besselI(2*sqrt(lambda1_star_equal[i]*lambda2_star_equal[i]),3)*(lambda1_star_equal[i]^2+lambda1_star_equal[i]*lambda2_star_equal[i]+lambda2_star_equal[i]^2))/
    (lambda1_star_equal[i]*lambda2_star_equal[i])
  
  expec_value_zdts_denomin<-besselI(2*sqrt(lambda1_star_equal[i]*lambda2_star_equal[i]),1)*(lambda1_star_equal[i]+lambda2_star_equal[i])+
    (besselI(2*sqrt(lambda1_star_equal[i]*lambda2_star_equal[i]),2)*(lambda1_star_equal[i]^2+lambda2_star_equal[i]^2))/
    (lambda1_star_equal[i]*lambda2_star_equal[i])^(1/2)+(besselI(2*sqrt(lambda1_star_equal[i]*lambda2_star_equal[i]),3)*(lambda1_star_equal[i]^3+lambda2_star_equal[i]^3))/
    (lambda1_star_equal[i]*lambda2_star_equal[i])
  
  
  expec_value_zdts_equal<-c(expec_value_zdts_equal,
                            (lambda1_star_equal[i]-lambda2_star_equal[i])*(expec_value_zdts_nomin/expec_value_zdts_denomin))
  
  expec_value_used_equal<-c(expec_value_used_equal,lambda1_star_equal[i]-lambda2_star_equal[i])
  
}





mean(expec_value_zdts_equal)
mean(expec_value_used_equal)

####------- lambda1_star - lambda2_star between two equal strength teams--------####


lambda1_star_lambda2_star_equal<-lambda1_star_equal-lambda2_star_equal
mean_lambda1_star_lambda2_star_equal<-mean(lambda1_star_lambda2_star_equal)
median_lambda1_star_lambda2_star_equal<-median(lambda1_star_lambda2_star_equal)
sd_lambda1_star_lambda2_star_equal<-sd(lambda1_star_lambda2_star_equal)
quantile(lambda1_star_lambda2_star_equal,probs=c(0.025,0.975))


lambda1_star_lambda2_star_equal_posterior<-c( mean_lambda1_star_lambda2_star_equal,
                                              median_lambda1_star_lambda2_star_equal,sd_lambda1_star_lambda2_star_equal,
                                              quantile(lambda1_star_lambda2_star_equal,probs=c(0.025,0.975))		)
lambda1_star_lambda2_star_equal_posterior
###### Ezdts between two equal strength teams


mean(expec_value_zdts_equal)
median(expec_value_zdts_equal)
sd(expec_value_zdts_equal)
quantile(expec_value_zdts_equal,probs=c(0.025,0.975))

mean(expec_value_zdts_equal)-sd(expec_value_zdts_equal)
mean(expec_value_zdts_equal)+sd(expec_value_zdts_equal)
#apply(as.matrix(expec_value_zdts_equal),1,quantile,probs=c(0.025,0.975))



expec_value_zdts_equal_posterior<-c(mean(expec_value_zdts_equal),median(expec_value_zdts_equal),
                                    sd(expec_value_zdts_equal),quantile(expec_value_zdts_equal,probs=c(0.025,0.975))
)


# ------- Table 4--------#


mu_posterior
home_posterior
exp_mu_posterior
exp_home_posterior
approx_SESD_posterior
lambda1_star_lambda2_star_equal_posterior
expec_value_zdts_equal_posterior

# betas unstandardized posterior distribution
params_ZDTS_only_Skills_after_BVS
beta_home_params_ZDTS_only_Skills_after_BVS<-params_ZDTS_only_Skills_after_BVS$beta_home
beta_away_params_ZDTS_only_Skills_after_BVS<-params_ZDTS_only_Skills_after_BVS$beta_away


colnames(beta_home_params_ZDTS_only_Skills_after_BVS)<-c("(Home) failed serves",
                                                         "(Home) poor passes",
                                                         "(Home) failed passes",
                                                         "(Home) blocked att1",
                                                         "(Home) failed att1")
colnames(beta_away_params_ZDTS_only_Skills_after_BVS)<-c("(Away) failed serves",
                                                         "(Away) failed passes",
                                                         "(Away) blocked att1",
                                                         "(Away) failed att1",
                                                         "(Away) perfect att2",
                                                         "(Away) failed att2",
                                                         "(Away) block net violations",
                                                         "(Away) failed blocks" )

#---calculate unstandardized coefficients
std_beta_home_params_ZDTS_only_Skills_after_BVS<-beta_home_params_ZDTS_only_Skills_after_BVS
std_beta_away_params_ZDTS_only_Skills_after_BVS<-beta_away_params_ZDTS_only_Skills_after_BVS

unstd_beta_home_params_ZDTS_only_Skills_after_BVS<-sweep(std_beta_home_params_ZDTS_only_Skills_after_BVS,
                                                         MARGIN=2,apply(X_home[,colnames(X_home)%in%c("Home_failed_serves",
                                                                                                      "Home_poor_passes",
                                                                                                      "Home_failed_passes",
                                                                                                      "Home_blocked_att1",
                                                                                                      "Home_failed_att1" )],2,sd), `/`)
unstd_beta_away_params_ZDTS_only_Skills_after_BVS<-sweep(std_beta_away_params_ZDTS_only_Skills_after_BVS,
                                                         MARGIN=2,apply(X_away[,colnames(X_away)%in%c("Away_failed_serves",
                                                                                                      "Away_failed_passes",
                                                                                                      "Away_blocked_att1",
                                                                                                      "Away_failed_att1",
                                                                                                      "Away_perfect_att2",
                                                                                                      "Away_failed_att2",
                                                                                                      "Away_net_violation_blocks",
                                                                                                      "Away_failed_blocks" )],2,sd), `/`)

# Unstandardised home parameters
unstd_home_params_revised_zdts<-t(rbind(apply(unstd_beta_home_params_ZDTS_only_Skills_after_BVS,2,mean),
        apply(unstd_beta_home_params_ZDTS_only_Skills_after_BVS,2,median),
        apply(unstd_beta_home_params_ZDTS_only_Skills_after_BVS,2,sd),
        apply(unstd_beta_home_params_ZDTS_only_Skills_after_BVS,2, function(x) quantile(x, probs = c(0.025, 0.975)))
))

xtable(unstd_home_params_revised_zdts)
#---Interpretation focused on the shifted expected set difference
round(exp(0.61*unstd_home_params_revised_zdts)-1,2)
# Unstandardised away parameters

unstd_away_params_revised_zdts<-xtable(t(rbind(apply(unstd_beta_away_params_ZDTS_only_Skills_after_BVS,2,mean),
               apply(unstd_beta_away_params_ZDTS_only_Skills_after_BVS,2,median),
               apply(unstd_beta_away_params_ZDTS_only_Skills_after_BVS,2,sd),
               apply(unstd_beta_away_params_ZDTS_only_Skills_after_BVS,2, function(x) quantile(x, probs = c(0.025, 0.975)))
          ))
      )
xtable(unstd_away_params_revised_zdts)
round(exp(0.61*-unstd_away_params_revised_zdts)-1,2)

# ------Appendix Table with posterior summary statistics of ZDTS model parameters-------##
# ZDTS_only_Skills_after_BVS_summary<-summary(ZDTS_only_Skills_after_BVS,
#                                             pars=c("mu","home","attack","defense"), probs=c(0.025,0.5,0.975))
# 
# round(ZDTS_only_Skills_after_BVS_summary$summary[c(1:26),c(1,3)],2)
# 
# xtable(round(ZDTS_only_Skills_after_BVS_summary$summary[c(1:26),c(1,3)],2))
# 
# 
# 
# #----- Appendix Table with posterior summary statistics of the ordered model parameters-------##
# final_ordered_logistic_summary<-summary(final_ordered_logistic,
#                                         pars=c("c","team_abil"), probs=c(0.025,0.5,0.975))
# 
# round(final_ordered_logistic_summary$summary[c(1:17),c(1,3)],2)
# 
# xtable(round(final_ordered_logistic_summary$summary[c(1:17),c(1,3)],2))
