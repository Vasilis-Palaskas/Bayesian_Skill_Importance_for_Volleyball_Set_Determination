# Load the posterior results of R-stan object related to the ZDTS (revised) with only skill actions proposed from BVS procedure
load(file="revised_ZDTS_only_Skills_after_BVS")

###--------------Posterior Summary Statistics-Analysis------------------------########

## Skill events actions analysis (post. summary statistics and visualisation of standardised coefficients)

#---------------------------
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

# Print the summary statistics of the below parameters
print(ZDTS_only_Skills_after_BVS,
      pars=c("mu","home",
             "beta_home","beta_away","dev"),probs = c(0.025,0.5,0.975), digits=2)

#-Access summary statistics

betas_summary <- summary(ZDTS_only_Skills_after_BVS, pars = c("mu","home","beta_home","beta_away"), probs = c(0.025, 0.5,0.95))$summary
print(round(betas_summary[,c(1,3,4,5,6)],2))
betas_summary_main<-round(betas_summary[,c(1,3,4,5,6)],2)
xtable(betas_summary_main)




## Stan interface for both summary and convergence diagnostics
launch_shinystan(ZDTS_only_Skills_after_BVS)

# Extract posterior simulations of parameters for further analysis
sims <- rstan::extract(ZDTS_only_Skills_after_BVS)

beta_home <- sims$beta_home
beta_away <- sims$beta_away
beta_home_away<-cbind(beta_home,beta_away)
mu<- sims$mu
home<- sims$home


## Order of ability parameters (based on the posterior medians)
beta_home_hat <- apply(beta_home,2, median)
beta_away_hat <- apply(beta_away,2, median)
beta_home_away_hat <- apply(beta_home_away,2, median)

mu_hat <- apply(mu,1,median)
home_hat <- apply(home,1,median)

beta_home_hat_ord <- order(beta_home_hat, decreasing = TRUE)
beta_away_hat_ord <- order(beta_away_hat, decreasing = TRUE)
beta_home_away_hat_ord <- order(beta_home_away_hat, decreasing = TRUE)

mu_hat_ord <- order(mu_hat, decreasing = TRUE)
home_hat_ord <- order(home_hat, decreasing = TRUE)



## Data frame of parameters in terms of convenience in both tables and graphs
beta_home<-data.frame(beta_home)
beta_away<-data.frame(beta_away)
beta_home_away<-data.frame(beta_home_away)
mu<-data.frame(mu)
home<-data.frame(home)


##---Proper parameters renaming for the purposes of visualisation
colnames(beta_home)<-c("(Home) failed serves",
                       "(Home) poor passes",
                       "(Home) failed passes",
                       "(Home) blocked att1",
                       "(Home) failed att1")
colnames(beta_away)<-c("(Away) failed serves",
                       "(Away) failed passes",
                       "(Away) blocked att1",
                       "(Away) failed att1",
                       "(Away) perfect att2",
                       "(Away) failed att2",
                       "(Away) block net violations",
                       "(Away) failed blocks" )

colnames(beta_home_away)<-c(colnames(beta_home),
                            colnames(beta_away))

###MCMC Posterior 95% uncertainty intervals


##---Combine 2 plots of both home and away skill event actions
color_scheme_set("brightblue")

pdf(file="revised_ZDTS_only_Skills_betas.pdf", width =12, height =7.5)

mcmc_intervals(beta_home_away[,c(beta_home_away_hat_ord)],
               prob = 0.95,prob_outer=0.95,
               point_est = c( "mean"))+ggtitle("Skill Events")+xlim(-2,1.4)+
  scale_x_continuous(breaks = c(seq(from = -2, to = 1.4, by = 0.4)))+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 20))
dev.off()
##--Separate 2 plots of home and away skill event actions, respectively.
color_scheme_set("brightblue")


plot1<-mcmc_intervals(beta_home[,c(beta_home_hat_ord)],
                      prob = 0.95,prob_outer=0.95,
                      point_est = c( "mean"))+ggtitle("Home Skill Events")+xlim(-2,1.4)+
  scale_x_continuous(breaks = c(seq(from = -2, to = 1.4, by = 0.4)))+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 20))


plot2<-mcmc_intervals(beta_away[,c(beta_away_hat_ord)],
                      prob = 0.95,prob_outer=0.95,
                      point_est = c( "mean"))+ggtitle("Away Skill Events")+xlim(-2,1.4)+
  scale_x_continuous(breaks = c(seq(from = -2, to = 1.4, by = 0.4)))+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 20))

pdf(file="revised_ZDTS_only_Skills_betas_separate.pdf", width =12, height =7.5)
grid.arrange(plot1,plot2,ncol=2)
dev.off()

#---------------------------

## Skill events actions analysis (post. summary statistics of unstandardised coefficients)

# Table 9. Posterior summaries of home and away parameters of revised ZDTS model in
#unstandardized scale (in ascending order of estimated effects).
##---------
# Transform standardised parameters of skill actions to unstandardised ones for better interpretation 
# based on main Paper intuition in Section 6.2 (Covariate effects as approximate proportional change of the shifted expected)

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

# Unstandardised home teams' skill event action parameters
unstd_home_params_revised_zdts<-t(rbind(apply(unstd_beta_home_params_ZDTS_only_Skills_after_BVS,2,mean),
                                        apply(unstd_beta_home_params_ZDTS_only_Skills_after_BVS,2,median),
                                        apply(unstd_beta_home_params_ZDTS_only_Skills_after_BVS,2,sd),
                                        apply(unstd_beta_home_params_ZDTS_only_Skills_after_BVS,2, 
                                              function(x) quantile(x, probs = c(0.025, 0.975)))
))

# Unstandardised away teams' skill event action parameters

unstd_away_params_revised_zdts<-t(rbind(apply(unstd_beta_away_params_ZDTS_only_Skills_after_BVS,2,mean),
                                        apply(unstd_beta_away_params_ZDTS_only_Skills_after_BVS,2,median),
                                        apply(unstd_beta_away_params_ZDTS_only_Skills_after_BVS,2,sd),
                                        apply(unstd_beta_away_params_ZDTS_only_Skills_after_BVS,2,
                                              function(x) quantile(x, probs = c(0.025, 0.975)))
))
# round(exp(0.61*-unstd_away_params_revised_zdts)-1,2)
xtable(unstd_home_params_revised_zdts)
xtable(unstd_away_params_revised_zdts)
#---------



## Remaining parameters analysis along with additional calculated quantities (Table 8)
#---------------------------

## Extraction of model parameters
params_ZDTS_only_Skills_after_BVS<-rstan::extract(ZDTS_only_Skills_after_BVS)


#- mu parameter post. analysis

mean_mu<-mean(params_ZDTS_only_Skills_after_BVS$mu)
median_mu<-median(params_ZDTS_only_Skills_after_BVS$mu)
sd_mu<-sd(params_ZDTS_only_Skills_after_BVS$mu)
quantile(params_ZDTS_only_Skills_after_BVS$mu,probs=c(0.025,0.975))

lower_interval_mu<-mean_mu-sd(params_ZDTS_only_Skills_after_BVS$mu)
upper_interval_mu<-mean_mu+sd(params_ZDTS_only_Skills_after_BVS$mu)
mu_posterior<-c(mean_mu,median_mu,sd_mu,quantile(params_ZDTS_only_Skills_after_BVS$mu,probs=c(0.025,0.975)))
print(mu_posterior)




#- mu parameter post. analysis (exponential scale)
exp_mu<-exp(params_ZDTS_only_Skills_after_BVS$mu)
mean_exp_mu<-mean(exp_mu)
median_exp_mu<-median(exp_mu)
sd_exp_mu<-sd(exp_mu)
quantile(exp_mu,probs=c(0.025,0.975))

lower_interval_exp_mu<-mean_exp_mu-sd(exp_mu)
upper_interval_exp_mu<-mean_exp_mu+sd(exp_mu)

exp_mu_posterior<-c(mean_exp_mu,median_exp_mu,sd_exp_mu,quantile(exp_mu,probs=c(0.025,0.975)))
print(exp_mu_posterior)

#- home parameter post. analysis

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
#- home parameter post. analysis (exponential scale)
exp_home<-exp(params_ZDTS_only_Skills_after_BVS$home)
mean_exp_home<-mean(exp_home)
median_exp_home<-median(exp_home)
quantile(exp_home,probs=c(0.025,0.975))

lower_interval_exp_home<-mean_exp_home-sd(exp_home)
upper_interval_exp_home<-mean_exp_home+sd(exp_home)

exp_home_posterior<-c(mean_exp_home,median_exp_home,sd_home,quantile(exp_home,probs=c(0.025,0.975)))
exp_home_posterior


####Computation of l1-l2, Ezdts between   equal teams' strength
# l1, l2 parameters
lambda1_star<-params_ZDTS_only_Skills_after_BVS$lambda1_star
lambda2_star<-params_ZDTS_only_Skills_after_BVS$lambda2_star

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

#### lambda1_star - lambda2_star between two equal strength teams--------####


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



expec_value_zdts_equal_posterior<-c(mean(expec_value_zdts_equal),median(expec_value_zdts_equal),
                                    sd(expec_value_zdts_equal),quantile(expec_value_zdts_equal,probs=c(0.025,0.975))
)
#Table 8


mu_posterior
home_posterior
exp_mu_posterior
exp_home_posterior
approx_SESD_posterior
lambda1_star_lambda2_star_equal_posterior
expec_value_zdts_equal_posterior

#---------------------------
