
# Save these values in order to manipulate them in terms of convergence diagnostics,, posterior summary statistics, etc...
# Save these values in order to manipulate them in terms of convergence diagnostics, posterior summary statistics, etc...
load(file="gammas_home_matrix_ta_revised")
load(file="gammas_away_matrix_ta_revised")

load(file="betas_home_matrix_ta_revised")
load(file="betas_away_matrix_ta_revised")


# Store both gammas and betas posterior values after discarding the warmup from T iterations (here, we have chosen 10% of total T iterations).
warmup<-10000
# Each column includes the gammas values of each candidate variable.
final_posterior_values_gammas_home<-matrix(gammas_home_matrix[(data_zdts_ta_skills$K*warmup+1):length(gammas_home_matrix)],
                                           nrow=T-warmup,ncol=data_zdts_ta_skills$K,byrow=TRUE)

colnames(final_posterior_values_gammas_home)<-names(X_home)

final_posterior_values_gammas_away<-matrix(gammas_away_matrix[(data_zdts_ta_skills$K*warmup+1):length(gammas_away_matrix)],
                                           nrow=T-warmup,ncol=data_zdts_ta_skills$K,byrow=TRUE)
colnames(final_posterior_values_gammas_away)<-names(X_away)

# Combine both home and away gammas parameters
final_posterior_values_gammas_home_away<-cbind(final_posterior_values_gammas_home,final_posterior_values_gammas_away)
# Each column includes the betas values of each candidate variable.

final_posterior_values_betas_home<-matrix(betas_home_matrix[(data_zdts_ta_skills$K*warmup+1):length(betas_home_matrix)],
                                          nrow=T-warmup,ncol=data_zdts_ta_skills$K,byrow=TRUE)

colnames(final_posterior_values_betas_home)<-names(X_home)

final_posterior_values_betas_away<-matrix(betas_away_matrix[(data_zdts_ta_skills$K*warmup+1):length(betas_away_matrix)],
                                          nrow=T-warmup,ncol=data_zdts_ta_skills$K,byrow=TRUE)
colnames(final_posterior_values_betas_away)<-names(X_away)


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


# MCMC Convergence diagnostics
# a) Firstly, for gammas and betas indicators
# 
# convert them to a mcmc object in terms of our convenience
mcmc_final_posterior_values_gammas_home<-as.mcmc(final_posterior_values_gammas_home)
colnames(mcmc_final_posterior_values_gammas_home)<-names(X_home)

mcmc_final_posterior_values_gammas_away<-as.mcmc(final_posterior_values_gammas_away)
colnames(mcmc_final_posterior_values_gammas_away)<-names(X_away)

mcmc_final_posterior_values_gammas_home_away<-as.mcmc(final_posterior_values_gammas_home_away)

mcmc_final_posterior_values_betas_home<-as.mcmc(final_posterior_values_betas_home)
colnames(mcmc_final_posterior_values_betas_home)<-names(X_home)

mcmc_final_posterior_values_betas_away<-as.mcmc(final_posterior_values_betas_away)
colnames(mcmc_final_posterior_values_betas_away)<-names(X_away)
####------GG-Plots for Convergence Diagnostics

#----Step 1: Convert the mcmc object to a ggmcmc object
gg_posterior_values_betas_home <- ggs(mcmc_final_posterior_values_betas_home)
gg_posterior_values_betas_away<- ggs(mcmc_final_posterior_values_betas_away)

gg_posterior_values_gammas_home <- ggs(mcmc_final_posterior_values_gammas_home)
gg_posterior_values_gammas_away <- ggs(mcmc_final_posterior_values_gammas_away)

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

gg_posterior_values_gammas_home_away<-ggs(mcmc_final_posterior_values_gammas_home_away, par_labels=P)
gg_posterior_values_gammas_home_away<-gg_posterior_values_gammas_home_away[!gg_posterior_values_gammas_home_away$Parameter%in%c(
        "(Home) poor passes","(Home) blocked att1","(Away) blocked att1"),]

# Separate the post. inclus. prob. of skill actions to two categories: a) the selected from the BVS algorithm skill actions (selected) and b) the not seletec ones (not_selected)
gg_posterior_values_gammas_home_away_selected<-gg_posterior_values_gammas_home_away[
        gg_posterior_values_gammas_home_away$Parameter%in%c("(Home) failed serves","(Home) failed passes",
                                                            "(Home) failed att1","(Away) failed serves","(Away) failed passes",
                                                            "(Away) failed att1","(Away) failed att2","(Away) block net violations",
                                                            "(Away) failed blocks"),]

gg_posterior_values_gammas_home_away_not_selected<-gg_posterior_values_gammas_home_away[
        gg_posterior_values_gammas_home_away$Parameter%in%c("(Home) perfect serves","(Home) very good serves",
                                                            "(Home) perfect att1","(Home) perfect att2","(Home) blocked att2",
                                                            "(Home) failed att2","(Home) perfect blocks","(Home) block net violations",
                                                            "(Home) failed blocks","(Home) failed settings",
                                                            "(Away) perfect serves","(Away) very good serves",
                                                            "(Away) poor passes","(Away) perfect att1",
                                                            "(Away) blocked att2","(Away) perfect blocks",
                                                            "(Away) failed settings"),]

#----Step2: Save in a single pdf all the necessary plots for the assessment of the convergence
ggmcmc(gg_posterior_values_betas_home, 
       file = "converg_betas_home_zdts_ta_skills_revised.pdf", plot=c( "running","traceplot",
                                                                       "geweke","Rhat","autocorrelation"))

ggmcmc(gg_posterior_values_betas_away, 
       file = "converg_betas_away_zdts_ta_skills_revised.pdf", plot=c( "running","traceplot",
                                                                       "geweke","Rhat","autocorrelation"))


ggmcmc(gg_posterior_values_gammas_home, 
       file = "converg_gammas_home_zdts_ta_skills_revised.pdf", plot=c( "running",
                                                                        "geweke","Rhat","autocorrelation"))

# This pdf plot will be included in the Appendix
selected_ggs<-ggs_running(gg_posterior_values_gammas_home_away_selected)+
        facet_wrap(~ Parameter, scales = "free" )+  ggtitle("Selected by BVS Algorithm \n(ZDTS-Formulation a)")+
        scale_y_continuous(limits=c(0.4,1), breaks = c(seq(0.4,1, by = 0.1 )))+
        scale_x_continuous(limits=c(0,T-warmup), breaks = c(seq(0,T-warmup, by = 10000  )))

not_selected_ggs<- ggs_running(gg_posterior_values_gammas_home_away_not_selected)+
        facet_wrap(~ Parameter, scales = "free" )+ ggtitle("Not Selected by BVS Algorithm \n(ZDTS-Formulation a)")+
        scale_y_continuous(limits=c(0,0.6), breaks = c(seq(0,0.6, by = 0.1 )))+
        scale_x_continuous(limits=c(0,T-warmup), breaks = c(seq(0,T-warmup, by = 10000 )))  

pdf(file="Post_Incl_Probs_ZDTS_TA_Skills.pdf", width =19, height =11)
grid.arrange(selected_ggs,not_selected_ggs,ncol=2)
dev.off()  
