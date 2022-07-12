#--------------------------------------------------------------------------------------------------------------------------
##### --------Bayesian Comparison between MAD of two fitted models --------#################

library(gridExtra)
library(ggpubr)
library(rstan)
library(ggplot2)
library(bayesplot)
# In order to obtain the ppc bar plots, it is essential
# to run the codes related with the goodness-of-fit evaluation of
# both models

source(file.choose())#\R Codes\4.3- Goodness of fit model comparison\In Sample Diagnostics\In_Sample_ordered.R
source(file.choose())#\R Codes\4.3- Goodness of fit model comparison\In Sample Diagnostics\In_Sample_ZDTS.R

##Ordered
#	load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_points_ordered

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_set_diff_ordered

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_freq_ordered

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_rel_freq_ordered

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_total_set_diff_ordered

##ZDTS
	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_points_zdts

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_set_diff_zdts

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_freq_zdts

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_rel_freq_zdts

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/deviance_total_set_diff_zdts

##### Conclusion of the MAD measures of the Ordered

deviance_points_ordered
deviance_set_diff_ordered
deviance_freq_ordered
deviance_rel_freq_ordered
deviance_total_set_diff_ordered

##### Conclusion of the MAD measures of the ZDTS

deviance_points_zdts
deviance_set_diff_zdts
deviance_freq_zdts
deviance_rel_freq_zdts*100
deviance_total_set_diff_zdts



#### Deviance points
deviance_points<-data.frame(deviance_points_zdts,deviance_points_ordered)
deviance_set_diff<-data.frame(deviance_set_diff_zdts,deviance_set_diff_ordered)
deviance_freq<-data.frame(deviance_freq_zdts,deviance_freq_ordered)
deviance_rel_freq<-data.frame(deviance_rel_freq_zdts,deviance_rel_freq_ordered)
deviance_total_set_diff<-data.frame(deviance_total_set_diff_zdts,deviance_total_set_diff_ordered)

colnames(deviance_points)<-c("ZDTS","Ordered-Multinomial")
colnames(deviance_set_diff)<-c("ZDTS","Ordered-Multinomial")
colnames(deviance_freq)<-c("ZDTS","Ordered-Multinomial")
colnames(deviance_rel_freq)<-c("ZDTS","Ordered-Multinomial")
colnames(deviance_total_set_diff)<-c("ZDTS","Ordered-Multinomial")

#Table with posterior means of each measure (Table 6)
apply(deviance_freq,2,mean)
apply(deviance_rel_freq,2,mean)
apply(deviance_set_diff,2,mean)
apply(deviance_points,2,mean)
apply(deviance_total_set_diff,2,mean)

apply(deviance_freq,2,sd)
apply(deviance_rel_freq,2,sd)
apply(deviance_set_diff,2,sd)
apply(deviance_points,2,sd)
apply(deviance_total_set_diff,2,sd)


# Density areas of MAD measures of Table 6(Electronic Appendix)
plot1<-mcmc_areas(deviance_points,
    prob = 0.95,point_est = c( "mean"))+ggtitle("Q=Expected total points")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))
	
plot2<-mcmc_areas(deviance_set_diff,
    prob = 0.95,point_est = c( "mean"))+ggtitle("      Q=Expected set difference")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_blank(), 
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23),
  axis.ticks.y = element_blank(),axis.line.y=element_blank())
		
plot3<-mcmc_areas(deviance_freq,
    prob = 0.95,point_est = c( "mean"))+ggtitle("Q=Frequencies of set differences")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))
		
	plot4<-	mcmc_areas(deviance_rel_freq*100,
    prob = 0.95,point_est = c( "mean"))+ggtitle("    Q= Relative Frequencies of set differences (%)")+scale_x_continuous(lim=c(0,10))+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23),axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),axis.line.y=element_blank())
		
		plot5<-mcmc_areas(deviance_total_set_diff,
    prob = 0.95,point_est = c( "mean"))+ggtitle("Q=Expected total set differences")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))

grid.arrange(plot3,plot4,ncol=2)

ggarrange( plot1,plot2,plot5,nrow=2,ncol=2)






