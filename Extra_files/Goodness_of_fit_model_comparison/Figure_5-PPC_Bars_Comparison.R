###--------------Visualization of PPC Graphs (Section 3 in Paper)-----------###

#Load the proper libraries

library(gridExtra)
library(rstan)
library(ggplot2)
library(bayesplot)

# In order to obtain the ppc bar plots, it is essential
# to run the codes related with the goodness-of-fit evaluation of
# both models

source(file.choose())#\R Codes\4.3- Goodness of fit model comparison\In Sample Diagnostics\In_Sample_ordered.R
source(file.choose())#\R Codes\4.3- Goodness of fit model comparison\In Sample Diagnostics\In_Sample_ZDTS.R


###--Load the proper quantities 

##Ordered
	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/	load(file.choose())#/Output/MAD/In sample Predicted differences (ordered)(Multinomial values)
#multi_pred_differences_out_ordered


##ZDTS
	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/In-Sample/	load(file.choose())#/Output/MAD/In sample Predicted differences (ZDTS)(Multinomial values)
#multi_pred_differences_out

limits <- c(0, 50)
breaks <- seq(limits[1], limits[2], by=10)

# assign common axis to both plots
# p1.common.y <- p1 + scale_y_continuous(limits=limits, breaks=breaks)
# p2.common.y <- p2 + scale_x_continuous(limits=limits, breaks=breaks)
plot1<-ppc_bars(dataList_new_final$dif_sets,multi_pred_differences_out_ordered,prob = 0.95, width = 0.9, size = 1, fatten = 3,
<<<<<<< HEAD
                freq = TRUE)+labs(x="Set difference")+scale_x_discrete(limits=c("-3","-2","-1","1","2","3"))+labs(x="Set Difference",y="Number of games")+ggtitle("Ordered-Multinomial")+
=======
                freq = TRUE)+labs(x="Set difference")+scale_y_continuous(limits=limits, breaks=breaks)+
  scale_x_discrete(limits=c("-3","-2","-1","1","2","3"))+labs(x="Set Difference",y="Number of games")+ggtitle("Ordered-Multinomial")+
>>>>>>> 22b9bddcfc22627cff6a6c1ea91f412b9b1be541
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))+theme(legend.position="none")



plot2<-ppc_bars(dataList_new_final$dif_sets,multi_pred_differences_out,prob = 0.95, width = 0.9, size = 1, fatten = 3,
<<<<<<< HEAD
                freq = TRUE)+labs(x="Set difference")+labs(x="Set Difference",y="")+scale_x_discrete(limits=c("-3","-2","-1","1","2","3"))+ggtitle("ZDTS")+
=======
                freq = TRUE)+labs(x="Set difference")+labs(x="Set Difference",y="")+scale_y_continuous(limits=limits, breaks=breaks)+
  scale_x_discrete(limits=c("-3","-2","-1","1","2","3"))+ggtitle("ZDTS")+
>>>>>>> 22b9bddcfc22627cff6a6c1ea91f412b9b1be541
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))+ theme(legend.text = element_text(size = 20))##, face = "bold"


grid.arrange(plot1, plot2, ncol=2)
