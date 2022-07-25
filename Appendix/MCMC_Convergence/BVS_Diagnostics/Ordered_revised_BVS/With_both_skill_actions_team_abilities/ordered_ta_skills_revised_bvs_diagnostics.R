#  Load the posterior densities (objects) of both gammas and betas for further manipulation in terms of convergence diagnostics, posterior summary statistics, etc...

load("BVS_Ordered_TA_Skills_gammas_collinearity")# saved in folder of my desktop
load("BVS_Ordered_TA_Skills_betas_collinearity")# saved in folder of my desktop

# Store both gammas and betas posterior values after discarding the warmup from T iterations (here, we have chosen to discard the 20% of total T iterations).
warmup<-3000
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
# Separate the post. inclus. prob. of skill actions to two categories: a) the selected from the BVS algorithm skill actions (selected) and b) the not seletec ones (not_selected)
gg_posterior_values_gammas_selected<-gg_posterior_values_gammas[
  gg_posterior_values_gammas$Parameter%in%c("perfect_serves","failed_serves",
                                            "perfect_att1","failed_att1",
                                            "perfect_att2","failed_att2",
                                            "perfect_blocks","failed_settings")
  ,]

gg_posterior_values_gammas_not_selected<-gg_posterior_values_gammas[
  gg_posterior_values_gammas$Parameter%in%c("very_good_serves","perfect_passes",
                                            "very_good_passes","blocked_att1","blocked_att2",
                                            "net_violation_blocks","failed_blocks"),]

#----Step2: Save in a single pdf all the necessary plots for the assessment of the convergence

# This pdf plot will be included in the Appendix
selected_ggs<-ggs_running(gg_posterior_values_gammas_selected)+
  facet_wrap(~ Parameter, scales = "free" )+  ggtitle("Selected by BVS Algorithm \n(Ordered multinomial-Formulation a)")+
  scale_y_continuous(limits=c(0.4,1), breaks = c(seq(0.4,1, by = 0.1 )))+
  scale_x_continuous(limits=c(0,T-warmup), breaks = c(seq(0,T-warmup, by = 3000 )))

not_selected_ggs<- ggs_running(gg_posterior_values_gammas_not_selected)+
  facet_wrap(~ Parameter, scales = "free", ncol = 4,nrow=2 )+ ggtitle("Not Selected by BVS Algorithm \n(Ordered multinomial-Formulation a)")+
  scale_y_continuous(limits=c(0,0.6), breaks = c(seq(0,0.6, by = 0.1 )))+
  scale_x_continuous(limits=c(0,T-warmup), breaks = c(seq(0,T-warmup, by = 3000 )))  

pdf(file="Post_Incl_Probs_Ordered_TA_Skills.pdf", width =14.5, height =8.5)
grid.arrange(selected_ggs,not_selected_ggs,ncol=2)
dev.off()  

pdf(file="Post_Incl_Probs_without_selected_Ordered_TA_Skills.pdf",width =10.5, height =5.5)
grid.arrange(not_selected_ggs)
dev.off()  
