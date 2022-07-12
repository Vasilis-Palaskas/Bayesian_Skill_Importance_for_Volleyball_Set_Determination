

load("revised_ordered_skills_after_BVS")


######-------------ordered Logistic model with only skills
##---Parameters Names

skill_events_differences <- c("perfect_serves","failed_serves",
                              "perfect_att1", "failed_att1",
                              "perfect_att2","failed_att2","perfect_blocks",
                              "failed_settings")
# cutpoints<-c("c_1","c_2","c_3","c_4","c_5","c_6")
cutpoints<-c("c_1","c_2","c_3","c_4","c_5")

###--------------Posterior Summary Statistics-Analysis------------------------########

#####----------------------Posterior summary----------------------------######

names(ordered_skills_after_BVS_model)[1:8]<-skill_events_differences
names(ordered_skills_after_BVS_model)[c(14:18)]<-cutpoints



print(ordered_skills_after_BVS_model,
      pars=c(
        "beta","first_temp_Intercept",
        "temp_Intercept"),probs = c(0.025,0.5,0.975), digits=2)


# #-Access summary statistics
# df_of_draws <- as.data.frame(ordered_skills_after_BVS_model)
# print(colnames(df_of_draws))
# fit_summary <- summary(ordered_skills_after_BVS_model)
betas_summary <- summary(ordered_skills_after_BVS_model, pars = c("beta"), probs = c(0.025, 0.5,0.95))$summary
print(round(betas_summary[,c(1,3,4,5,6)],2))
betas_summary_main<-round(betas_summary[,c(1,3,4,5,6)],2)
xtable(betas_summary_main)

## Extraction of model parameters
sims <- rstan::extract(ordered_skills_after_BVS_model)

beta <- sims$beta
first_temp_Intercept<- sims$first_temp_Intercept
temp_Intercept<- sims$temp_Intercept
# temp_intercepts<-cbind(first_temp_Intercept,temp_Intercept)
temp_intercepts<-cbind(temp_Intercept)

## Order of ability parameters (based on the posterior means)
beta_hat <- apply(beta,2, median)
first_temp_Intercept_hat <- apply(first_temp_Intercept,1,median)
temp_intercepts_hat <- apply(temp_intercepts,2,median)

beta_hat_ord <- order(beta_hat, decreasing = TRUE)
first_temp_Intercept_hat_ord <- order(first_temp_Intercept_hat, decreasing = TRUE)
temp_intercepts_hat_ord <- order(temp_intercepts_hat, decreasing = TRUE)


##---Proper parameters renaming
colnames(beta)<-skill_events_differences
colnames(temp_intercepts)<-cutpoints



## Data frame of parameters in terms of convenience in both tables and graphs

beta<-as.data.frame(beta)
temp_intercepts<-as.data.frame(temp_intercepts)

###-----MCMC Posterior 95% uncertainty intervals


color_scheme_set("brightblue")


pdf(file="Ordered_Only_Skills_cutpoints.pdf", width =12, height =7.5)

mcmc_intervals(temp_intercepts[,c(temp_intercepts_hat_ord)],
               prob = 0.95,prob_outer=0.95,
               point_est = c( "mean"))+ggtitle("Cutpoints")+xlim(-6,6)+
  scale_x_continuous(breaks = seq(from = -6, to = 6, by = 1))+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 20))
dev.off()

pdf(file="revised_Ordered_Only_Skills_Skills_Differences.pdf", width =12, height =7.5)

mcmc_intervals(beta[,c(beta_hat_ord)],
               prob = 0.95,prob_outer=0.95,
               point_est = c( "mean"))+ggtitle("Skill Differences")+xlim(-1,1)+
  scale_x_continuous(breaks = seq(from = -1, to = 1, by = 0.1))+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 20))

dev.off()
