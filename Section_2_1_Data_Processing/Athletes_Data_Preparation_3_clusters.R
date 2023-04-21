
###### SECTION 7.1: Preparation &  Cleaning of athletes' data
# install.packages("fmsb")

# Loading packages
library(ClusterR)
library(cluster)
library(fmsb)
library(scales)

###### Import file, rename columns and impute missing values---------------------

# Data Import
volley_athletes<-read.csv("C:\\Users\\vasileios palaskas\\Documents\\GitHub\\Bayesian_Skill_Importance_for_Volleyball_Set_Determination\\Section_2_1_Data_Processing\\Data\\All_teams_selected_data_per_player.csv",
                          header=T,encoding = "UTF-8")
head(volley_athletes,30)
# Renaming in terms of consistency with the team-level dataset 
# (competing teams with their skill actions executions per match)
colnames(volley_athletes)<-c("team","Player","Position","total_serves","failed_serves",
                             "very_good_serves","perfect_serves","total_passes","failed_passes",
                             "poor_passes","very_good_passes","perfect_passes",
                             "total_att1","failed_att1","blocked_att1","perfect_att1",
                             "total_att2","failed_att2","blocked_att2","perfect_att2",
                             "total_blocks","failed_blocks","net_violation_blocks",
                             "perfect_blocks","total_settings","failed_settings")
head(volley_athletes,30)
# Factorise the teams
volley_athletes$team<-factor(volley_athletes$team,levels = levels(data_by_sets$home_Team))


# Remove rows where they reflect the total executions by each team since we are now interesting only for the athletes' executions.
volley_athletes<-volley_athletes[!volley_athletes$Player=="Team",]
head(volley_athletes,20)
dim(volley_athletes)###In total 188 and 26 variables

volley_athletes$poor_passes<-as.numeric(volley_athletes$poor_passes)# Numerize poor passes

# Replace NA with 0 across statistics/skills executions per athlete

nas <- which(is.na(volley_athletes$col))
emptystrs <- which(volley_athletes$col == "")
nas_or_empty <- which(is.na(volley_athletes$col)|volley_athletes$col == "")
volley_athletes[volley_athletes==""|is.na(volley_athletes)]<-0                                                                                                                           

# Replace "0" value with O to correct the mistake located on the data concerning the field Position O of athletes
volley_athletes$Position[volley_athletes$Position=="0"]<-"O"
str(volley_athletes)

# Union of Papadimitriou observations since they refer to a common player whow transfered during the midseason 
volley_athletes[nrow(volley_athletes) + 1,] <- c("Pamvochaikos","PAPADIMITRIOU","L",
      apply(volley_athletes[volley_athletes$Player%in%c("4 PAPADIMITRIOU","20 PAPADIMITRIOU ","22 PAPADIMITRIOU"),c(4:26)],2,sum) )
#convert to numeric the false considered characters of frequencies of skill actions (converted to characters due to the above command)
volley_athletes <- volley_athletes %>% mutate_at(names(volley_athletes)[c(4:26)], as.numeric)
# Union of DIMITRIADIS observations since they refer to a common player

volley_athletes[nrow(volley_athletes) + 1,] <- c("Foinikas Syrou","DIMITRIADIS","S",
                                                 apply(volley_athletes[volley_athletes$Player%in%c("4 DIMITRIADIS"),c(4:26)],2,sum) )
#convert to numeric the false considered characters of frequencies of skill actions (converted to characters due to the above command)
volley_athletes <- volley_athletes %>% mutate_at(names(volley_athletes)[c(4:26)], as.numeric)

# delete duplicate observations of Papadimitriou & Dimitriadis which were unified at a single last row of the dataset (see above command)
volley_athletes<-volley_athletes[!volley_athletes$Player%in%c("4 PAPADIMITRIOU","20 PAPADIMITRIOU ","22 PAPADIMITRIOU","4 DIMITRIADIS"),]

# Distinguish the athletes' data by creating two different datasets:

# a) Athletes' data with the cumulative/total frequencies of all skill actions in the end of the regular season 
volley_athletes_all_season_statistics<-volley_athletes

# b) Athletes' data with the  statistics executions/frequencies in each regular season match (n=11*2 round robin)
# This dataset is obtained by dividing the cumulative frequencies of the skill actions with 
# the number of all regular season matches (n=11*2 round robin) and then each row
# reflects for an athlete his skill events frequencies in each match..
volley_athletes_per_game_statistics<-volley_athletes
volley_athletes_per_game_statistics[,colnames(volley_athletes_per_game_statistics)%in%c("total_serves","failed_serves",
                                                                                        "very_good_serves","perfect_serves","total_passes","failed_passes",
                                                                                        "poor_passes","very_good_passes","perfect_passes",
                                                                                        "total_att1","failed_att1","blocked_att1","perfect_att1",
                                                                                        "total_att2","failed_att2","blocked_att2","perfect_att2",
                                                                                        "total_blocks","failed_blocks","net_violation_blocks",
                                                                                        "perfect_blocks","total_settings","failed_settings")]<-round( (volley_athletes_per_game_statistics[,
                                                                                                                            colnames(volley_athletes_per_game_statistics)%in%c("total_serves","failed_serves",
                                                                                                                            "very_good_serves","perfect_serves","total_passes","failed_passes",
                                                                                                                            "poor_passes","very_good_passes","perfect_passes",
                                                                                                                            "total_att1","failed_att1","blocked_att1","perfect_att1",
                                                                                                                            "total_att2","failed_att2","blocked_att2","perfect_att2",
                                                                                                                            "total_blocks","failed_blocks","net_violation_blocks",
                                                                                                                            "perfect_blocks","total_settings","failed_settings")])/22,2)
head(volley_athletes_per_game_statistics,20)

####### Now, data processing in order to decide which athletes should be excluded from the
####### analysis due to their limited skill actions. Also, a correction factor will be
####### created in order to account for the fair comparison/evaluation between athletes with different total 
####### skill action frequencies.-----------------------------------------------------------


# Calculate the sum of skill actions per main category of actions (total serves, etc...) per athlete
# in each match of the regular season.                                                                                             
apply(volley_athletes_per_game_statistics[,colnames(volley_athletes_per_game_statistics)%in%c("total_serves",
                                                                                              "total_passes",
                                                                                              "total_att1",
                                                                                              "total_att2",
                                                                                              "total_blocks",
                                                                                              "total_settings" )],1,sum)
dim(volley_athletes_per_game_statistics)#185 athletes

# Calculate for each athlete the total/cumulative skill actions/executions by each athlete in the end of the season
volley_athletes_all_season_statistics$total_skill_actions<-apply(volley_athletes_all_season_statistics[,
                                                                   colnames(volley_athletes_all_season_statistics)%in%c(
                                                                   "total_serves","total_passes",
                                                                   "total_att1", "total_att2", "total_blocks",
                                                                   "total_settings" )],1,sum)
dim(volley_athletes_all_season_statistics)#185 athletes

# Arrange the total skill actions in descending order.
volley_athletes_all_season_statistics<-volley_athletes_all_season_statistics%>% 
  arrange(desc(total_skill_actions) )


# Subsets including athletes of each Position. Those ones will reflect the cumulative/total skill actions per athlete for each position
position_s_athletes<-volley_athletes_all_season_statistics[volley_athletes_all_season_statistics$Position=="S",]
position_h_athletes<-volley_athletes_all_season_statistics[volley_athletes_all_season_statistics$Position=="H",]
position_l_athletes<-volley_athletes_all_season_statistics[volley_athletes_all_season_statistics$Position=="L",]
position_m_athletes<-volley_athletes_all_season_statistics[volley_athletes_all_season_statistics$Position=="M",]
position_o_athletes<-volley_athletes_all_season_statistics[volley_athletes_all_season_statistics$Position=="O",]

# Univariate analysis of athletes' cumsum of total skill actions
# K-Means Clustering Analysis for each field Position subset of athletes (3 clusters)
# -----------------------------------------------------------------------------------

# Univariate Analysis
library(ggplot2)

# Basic density plot in ggplot2

# Total Skill actions-Field Position
ggplot(volley_athletes_all_season_statistics, aes(x =Position,y= total_skill_actions, colour = Position)) +
  geom_boxplot() + scale_x_discrete(name ="Field Position") +
  scale_y_continuous(name ="Total Skill actions",
                     limits = c(0,1800), breaks = c(seq(0,300, by = 150),seq(300,1000, by = 100),
                     seq(1000,1800, by = 200)) ) + theme(text = element_text(size = 20))        +
  ggtitle("Boxplot of the total skill actions  performed\n by athletes of each field Position")

ggplot(volley_athletes_all_season_statistics, aes(x = total_skill_actions, colour = Position)) +
  geom_density(lwd = 1.2, linetype = 1) + scale_x_discrete(name ="Total Skill actions of athletes") +
  scale_y_continuous(name ="Density") + theme(text = element_text(size = 20))        +
  ggtitle("Density of the total skill actions  performed\n by athletes of each field Position")

# S Position Clustering--------------------------------------------------
# Fitting K-Means clustering Model with 3 clusters to S Position athletes.
set.seed(240) # Setting seed
kmeans.re_position_s_athletes_3 <- kmeans(position_s_athletes[,ncol(position_s_athletes)], centers = 3, nstart = 20)
min(position_s_athletes$total_skill_actions[kmeans.re_position_s_athletes_3$"cluster"==3])#373
table(kmeans.re_position_s_athletes_3$cluster)#22/31 (7 from clusters 2 & 15 from cluster 3, two first clusters)--9 out

# Make the cluster ordered
kmeans.re_position_s_athletes_3_cluster_data<-data.frame(cluster=kmeans.re_position_s_athletes_3$cluster)
kmeans.re_position_s_athletes_3_cluster_data = kmeans.re_position_s_athletes_3_cluster_data%>% dplyr::mutate(
  cluster_new= case_when(
    (cluster == 2) ~ 1,
    (cluster == 3)  ~ 2,
    (cluster == 1)  ~ 3
    ) )
position_s_athletes$cluster_new=kmeans.re_position_s_athletes_3_cluster_data$cluster_new# include cluster column

# Visualization of Clustering
# 
# position_s_athletes_skill_actions<-c("failed_serves","very_good_serves","perfect_serves","total_serves",
#                                     # "failed_passes","poor_passes","very_good_passes","perfect_passes", "total_passes",were excluded based on S. Drikos suggestion and too few frequenecies
#                                      "failed_att1","blocked_att1","perfect_att1","total_att1",
#                                      "failed_att2","blocked_att2","perfect_att2","total_att2",
#                                      "failed_blocks","net_violation_blocks",
#                                      "perfect_blocks","total_blocks","failed_settings","total_settings")
# 
# position_s_athletes_radar_df<-position_s_athletes%>%select(all_of(position_s_athletes_skill_actions),cluster_new )%>%
#   as.data.frame()# Dataframe with variables required for radar chart plot (1st dataframe)
# # univariate summary statistics
# summary(position_s_athletes_radar_df)
# table(position_s_athletes_radar_df$poor_passes)
# table(position_s_athletes_radar_df$failed_passes)
# table(position_s_athletes_radar_df$very_good_passes)
# table(position_s_athletes_radar_df$perfect_passes)
# 
# 
# # calculate the adjusted skill actions percentage dataframe
# # position_s_athletes_radar_df_adjusted_per_skill_category<-position_s_athletes_radar_df
# # position_s_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_s_athletes_radar_df_adjusted_per_skill_category)]<-position_s_athletes_radar_df_adjusted_per_skill_category[,
# #    -ncol(position_s_athletes_radar_df_adjusted_per_skill_category)]/apply(position_s_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_s_athletes_radar_df_adjusted_per_skill_category)],1,sum)#2nd dataframe
# 
# position_s_athletes_radar_df_adjusted_per_skill_category<-position_s_athletes_radar_df
# # adjustment for serves
# position_s_athletes_radar_df_adjusted_per_skill_category[,c(1:3)]<-position_s_athletes_radar_df_adjusted_per_skill_category[,c(1:3)]/
#   position_s_athletes_radar_df_adjusted_per_skill_category$total_serves
#                                   # apply(position_s_athletes_radar_df_adjusted_per_skill_category[,c(1:3)],1,sum)#2nd dataframe
# # adjustment for att1
# position_s_athletes_radar_df_adjusted_per_skill_category[,c(5:7)]<-position_s_athletes_radar_df_adjusted_per_skill_category[,c(5:7)]/
#   position_s_athletes_radar_df_adjusted_per_skill_category$total_att1
# # adjustment for att2
# position_s_athletes_radar_df_adjusted_per_skill_category[,c(9:11)]<-position_s_athletes_radar_df_adjusted_per_skill_category[,c(9:11)]/
#   position_s_athletes_radar_df_adjusted_per_skill_category$total_att2
# # adjustment for  blocks
# position_s_athletes_radar_df_adjusted_per_skill_category[,c(13:15)]<-position_s_athletes_radar_df_adjusted_per_skill_category[,c(13:15)]/
#   position_s_athletes_radar_df_adjusted_per_skill_category$total_blocks
# # adjustment for  settings
# position_s_athletes_radar_df_adjusted_per_skill_category[,c(17:17)]<-position_s_athletes_radar_df_adjusted_per_skill_category[,c(17:17)]/
#   position_s_athletes_radar_df_adjusted_per_skill_category$total_settings
# 
# 
# position_s_athletes_radar_df_adjusted_per_skill_category<-position_s_athletes_radar_df_adjusted_per_skill_category %>% 
#   mutate_all(~replace(., is.nan(.), 0))%>%
#   as.data.frame()
# # # Reverse the negative skills so all skills present the same inference for high values
# # position_s_athletes_radar_df[,colnames(position_s_athletes_radar_df)%in%c(
# #   "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #   "failed_settings")]<--position_s_athletes_radar_df[,colnames(position_s_athletes_radar_df)%in%c(
# #     "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #     "failed_settings")]
# 
# #### Compare every profile to an average profile
# 
# # Average skill actions by each cluster for both 1st and 2nd dataframes
# average_profiles_position_s_athletes_radar_df<-position_s_athletes_radar_df%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_s_athletes_radar_df<-average_profiles_position_s_athletes_radar_df[,-1]#remove cluster column
# rownames(average_profiles_position_s_athletes_radar_df)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# average_profiles_position_s_athletes_radar_df_adjusted_per_skill_category<-position_s_athletes_radar_df_adjusted_per_skill_category%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_s_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_s_athletes_radar_df_adjusted_per_skill_category[,-1]#remove cluster column
# rownames(average_profiles_position_s_athletes_radar_df_adjusted_per_skill_category)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# # remove very good passes since they have zero values in both 1st and 2nd df.
# average_profiles_position_s_athletes_radar_df<-average_profiles_position_s_athletes_radar_df[,!colnames(average_profiles_position_s_athletes_radar_df)%in%
#                                                                                                c("total_serves_1",
#                                                                                                   "total_att1_1","total_att2_1",
#                                                                                                  "total_blocks_1","total_settings_1")]
# average_profiles_position_s_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_s_athletes_radar_df_adjusted_per_skill_category[,!colnames(average_profiles_position_s_athletes_radar_df_adjusted_per_skill_category)%in%c(
#   "total_serves_1",   "total_att1_1","total_att2_1",  "total_blocks_1","total_settings_1")]
# 
# 
# # Rescale each variable to range between 0 and 1 in both 1st and 2nd df.
# average_profiles_position_s_athletes_radar_df_scaled <- round(apply(average_profiles_position_s_athletes_radar_df, 2,
#                                                                     scales::rescale), 2)
# average_profiles_position_s_athletes_radar_df_scaled <- as.data.frame(average_profiles_position_s_athletes_radar_df_scaled)
# head(average_profiles_position_s_athletes_radar_df_scaled)  
# 
# average_profiles_position_s_athletes_radar_df_scaled_adjusted <- round(apply(average_profiles_position_s_athletes_radar_df_adjusted_per_skill_category, 2,
#                                                                     scales::rescale), 2)
# average_profiles_position_s_athletes_radar_df_scaled_adjusted <- as.data.frame(average_profiles_position_s_athletes_radar_df_scaled_adjusted)
# head(average_profiles_position_s_athletes_radar_df_scaled_adjusted) 
# 
# # 1ST df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_s_athletes_radar_df_scaled, 2, max)
# col_min <- apply(average_profiles_position_s_athletes_radar_df_scaled, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_s_athletes_radar_df_scaled, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_s_athletes_radar_df_scaled2 <- as.data.frame(rbind(col_summary,
#                                           average_profiles_position_s_athletes_radar_df_scaled))
# head(average_profiles_position_s_athletes_radar_df_scaled2)
# 
# 
# pdf(file="Clustering_analysis_Position_S_frequencies.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_s_athletes_radar_df_scaled2)) {
#   radarchart(
#     average_profiles_position_s_athletes_radar_df_scaled2[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels = c("Failed Serves","Very good Serves","Perfect Serves",
#                                           "Failed attack 1","Blocked attack 1","Perfect attack 1",
#                                           "Failed attack 2","Blocked attack 2","Perfect attack 2",
#                                           "Failed Blocks","Block net violation",
#                                           "Perfect Blocks","Failed Settings"),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_s_athletes_radar_df_scaled2)[i]
#   )
# }
# dev.off()
# 
# 
# # 2ND df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_s_athletes_radar_df_scaled_adjusted, 2, max)
# col_min <- apply(average_profiles_position_s_athletes_radar_df_scaled_adjusted, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_s_athletes_radar_df_scaled_adjusted, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_s_athletes_radar_df_scaled2_adjusted <- as.data.frame(rbind(col_summary,
#                                                                              average_profiles_position_s_athletes_radar_df_scaled_adjusted))
# head(average_profiles_position_s_athletes_radar_df_scaled2_adjusted)
# 
# 
# pdf(file="Clustering_analysis_Position_S_frequencies_adjusted.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_s_athletes_radar_df_scaled2_adjusted)) {
#   radarchart(
#     average_profiles_position_s_athletes_radar_df_scaled2_adjusted[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels =  c("Failed Serves","Very good Serves","Perfect Serves",
#                                            "Failed attack 1","Blocked attack 1","Perfect attack 1",
#                                            "Failed attack 2","Blocked attack 2","Perfect attack 2",
#                                            "Failed Blocks","Block net violation",
#                                            "Perfect Blocks","Failed Settings"),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_s_athletes_radar_df_scaled2_adjusted)[i]
#   )
# }
# dev.off()


# H Position Clustering Analysis --------------

set.seed(240) # Setting seed
kmeans.re_position_h_athletes_3 <- kmeans(position_h_athletes[,ncol(position_h_athletes)], centers = 3, nstart = 20)
min(position_h_athletes$total_skill_actions[kmeans.re_position_h_athletes_3$"cluster"==1])#513
table(kmeans.re_position_h_athletes_3$cluster)#31/64, 33 out (14 from cluster 1 and 17 from cluster 2)

# Make the cluster ordered
kmeans.re_position_h_athletes_3_cluster_data<-data.frame(cluster=kmeans.re_position_h_athletes_3$cluster)
kmeans.re_position_h_athletes_3_cluster_data =kmeans.re_position_h_athletes_3_cluster_data%>% dplyr::mutate(
  cluster_new= case_when(
    (cluster == 3) ~ 1,
    (cluster == 2)  ~ 3,
    (cluster == 1)  ~ 2
  )
)
position_h_athletes$cluster_new=kmeans.re_position_h_athletes_3_cluster_data$cluster_new
# Visualization of Clustering
# 
# position_h_athletes_skill_actions<-c("failed_serves","very_good_serves","perfect_serves","total_serves",
#                                      "failed_passes","poor_passes","very_good_passes","perfect_passes",
#                                      "total_passes",
#                                      # were excluded based on S. Drikos suggestion and too few frequenecies
#                                      "failed_att1","blocked_att1","perfect_att1","total_att1",
#                                      "failed_att2","blocked_att2","perfect_att2","total_att2",
#                                      "failed_blocks","net_violation_blocks","perfect_blocks","total_blocks"
#                                      ,"failed_settings","total_settings"
# )
# 
# position_h_athletes_radar_df<-position_h_athletes%>%select(all_of(position_h_athletes_skill_actions),cluster_new )%>%
#   as.data.frame()# Dataframe with variables required for radar chart plot (1st dataframe)
# # univariate summary statistics
# summary(position_h_athletes_radar_df)
# table(position_h_athletes_radar_df$poor_passes)
# table(position_h_athletes_radar_df$failed_passes)
# table(position_h_athletes_radar_df$very_good_passes)
# table(position_h_athletes_radar_df$perfect_passes)
# 
# 
# # calculate the adjusted skill actions percentage dataframe
# # position_h_athletes_radar_df_adjusted_per_skill_category<-position_h_athletes_radar_df
# # position_h_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_h_athletes_radar_df_adjusted_per_skill_category)]<-position_h_athletes_radar_df_adjusted_per_skill_category[,
# #    -ncol(position_h_athletes_radar_df_adjusted_per_skill_category)]/apply(position_h_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_h_athletes_radar_df_adjusted_per_skill_category)],1,sum)#2nd dataframe
# 
# position_h_athletes_radar_df_adjusted_per_skill_category<-position_h_athletes_radar_df
# # adjustment for serves
# position_h_athletes_radar_df_adjusted_per_skill_category[,c(1:3)]<-position_h_athletes_radar_df_adjusted_per_skill_category[,c(1:3)]/
#   position_h_athletes_radar_df_adjusted_per_skill_category$total_serves
# # adjustment for passes
# position_h_athletes_radar_df_adjusted_per_skill_category[,c(5:8)]<-position_h_athletes_radar_df_adjusted_per_skill_category[,c(5:8)]/
#   position_h_athletes_radar_df_adjusted_per_skill_category$total_passes
# # adjustment for att1
# position_h_athletes_radar_df_adjusted_per_skill_category[,c(10:12)]<-position_h_athletes_radar_df_adjusted_per_skill_category[,c(10:12)]/
#   position_h_athletes_radar_df_adjusted_per_skill_category$total_att1
# # adjustment for  att2
# position_h_athletes_radar_df_adjusted_per_skill_category[,c(14:16)]<-position_h_athletes_radar_df_adjusted_per_skill_category[,c(14:16)]/
#   position_h_athletes_radar_df_adjusted_per_skill_category$total_att2
# # adjustment for blocks
# position_h_athletes_radar_df_adjusted_per_skill_category[,c(18:20)]<-position_h_athletes_radar_df_adjusted_per_skill_category[,c(18:20)]/
#   position_h_athletes_radar_df_adjusted_per_skill_category$total_blocks
# # adjustment for settings
# position_h_athletes_radar_df_adjusted_per_skill_category[,c(22)]<-position_h_athletes_radar_df_adjusted_per_skill_category[,c(22)]/
#   position_h_athletes_radar_df_adjusted_per_skill_category$total_settings
# 
# position_h_athletes_radar_df_adjusted_per_skill_category<-position_h_athletes_radar_df_adjusted_per_skill_category %>% 
#   mutate_all(~replace(., is.nan(.), 0))%>%
#   as.data.frame()
# # # Reverse the negative skills so all skills present the same inference for high values
# # position_h_athletes_radar_df[,colnames(position_h_athletes_radar_df)%in%c(
# #   "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #   "failed_settings")]<--position_h_athletes_radar_df[,colnames(position_h_athletes_radar_df)%in%c(
# #     "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #     "failed_settings")]
# 
# #### Compare every profile to an average profile
# 
# # Average skill actions by each cluster for both 1st and 2nd dataframes
# average_profiles_position_h_athletes_radar_df<-position_h_athletes_radar_df%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_h_athletes_radar_df<-average_profiles_position_h_athletes_radar_df[,-1]#remove cluster column
# rownames(average_profiles_position_h_athletes_radar_df)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# average_profiles_position_h_athletes_radar_df_adjusted_per_skill_category<-position_h_athletes_radar_df_adjusted_per_skill_category%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_h_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_h_athletes_radar_df_adjusted_per_skill_category[,-1]#remove cluster column
# rownames(average_profiles_position_h_athletes_radar_df_adjusted_per_skill_category)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# # remove very good passes since they have zero values in both 1st and 2nd df.
# average_profiles_position_h_athletes_radar_df<-average_profiles_position_h_athletes_radar_df[,!colnames(average_profiles_position_h_athletes_radar_df)%in%
#                                                                                                c("total_serves_1","total_passes_1",
#                                                                                                  "total_att1_1","total_att2_1",
#                                                                                                  "total_blocks_1","total_settings_1")]
# average_profiles_position_h_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_h_athletes_radar_df_adjusted_per_skill_category[,!colnames(average_profiles_position_h_athletes_radar_df_adjusted_per_skill_category)%in%c(
#   "total_serves_1","total_passes_1",
#   "total_att1_1","total_att2_1",
#   "total_blocks_1","total_settings_1")]
# 
# 
# # Rescale each variable to range between 0 and 1 in both 1st and 2nd df.
# average_profiles_position_h_athletes_radar_df_scaled <- round(apply(average_profiles_position_h_athletes_radar_df, 2,
#                                                                     scales::rescale), 2)
# average_profiles_position_h_athletes_radar_df_scaled <- as.data.frame(average_profiles_position_h_athletes_radar_df_scaled)
# head(average_profiles_position_h_athletes_radar_df_scaled)  
# 
# average_profiles_position_h_athletes_radar_df_scaled_adjusted <- round(apply(average_profiles_position_h_athletes_radar_df_adjusted_per_skill_category, 2,
#                                                                              scales::rescale), 2)
# average_profiles_position_h_athletes_radar_df_scaled_adjusted <- as.data.frame(average_profiles_position_h_athletes_radar_df_scaled_adjusted)
# head(average_profiles_position_h_athletes_radar_df_scaled_adjusted) 
# 
# # 1ST df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_h_athletes_radar_df_scaled, 2, max)
# col_min <- apply(average_profiles_position_h_athletes_radar_df_scaled, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_h_athletes_radar_df_scaled, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_h_athletes_radar_df_scaled2 <- as.data.frame(rbind(col_summary,
#                                                                              average_profiles_position_h_athletes_radar_df_scaled))
# head(average_profiles_position_h_athletes_radar_df_scaled2)
# 
# 
# pdf(file="Clustering_analysis_position_h_frequencies.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_h_athletes_radar_df_scaled2)) {
#   radarchart(
#     average_profiles_position_h_athletes_radar_df_scaled2[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels = c("Failed Serves","Very good Serves","Perfect Serves",
#                                           "Failed Passes","Poor Passes","Very good Passes","Perfect Passes",
#                                           "Failed attack 1","Blocked attack 1","Perfect attack 1",
#                                           "Failed attack 2","Blocked attack 2","Perfect attack 2",
#                                           "Failed Blocks","Block net violation",
#                                           "Perfect Blocks","Failed Settings"),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_h_athletes_radar_df_scaled2)[i]
#   )
# }
# dev.off()
# 
# 
# # 2ND df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_h_athletes_radar_df_scaled_adjusted, 2, max)
# col_min <- apply(average_profiles_position_h_athletes_radar_df_scaled_adjusted, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_h_athletes_radar_df_scaled_adjusted, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_h_athletes_radar_df_scaled2_adjusted <- as.data.frame(rbind(col_summary,
#                                                                                       average_profiles_position_h_athletes_radar_df_scaled_adjusted))
# head(average_profiles_position_h_athletes_radar_df_scaled2_adjusted)
# 
# 
# pdf(file="Clustering_analysis_position_h_frequencies_adjusted.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_h_athletes_radar_df_scaled2_adjusted)) {
#   radarchart(
#     average_profiles_position_h_athletes_radar_df_scaled2_adjusted[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels =   c("Failed Serves","Very good Serves","Perfect Serves",
#                                             "Failed Passes","Poor Passes","Very good Passes","Perfect Passes",
#                                             "Failed attack 1","Blocked attack 1","Perfect attack 1",
#                                             "Failed attack 2","Blocked attack 2","Perfect attack 2",
#                                             "Failed Blocks","Block net violation",
#                                             "Perfect Blocks","Failed Settings"),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_h_athletes_radar_df_scaled2_adjusted)[i]
#   )
# }
# dev.off()


# L Position: ----------------

set.seed(240) # Setting seed
kmeans.re_position_l_athletes_3 <- kmeans(position_l_athletes[,ncol(position_l_athletes)], centers = 3, nstart = 20)
min(position_l_athletes$total_skill_actions[kmeans.re_position_l_athletes_3$"cluster"==2])#275
table(kmeans.re_position_l_athletes_3$cluster)#12/24   (4 from cluster 1 and 8 from cluster 2)
# Make the cluster ordered
kmeans.re_position_l_athletes_3_cluster_data<-data.frame(cluster=kmeans.re_position_l_athletes_3$cluster)
kmeans.re_position_l_athletes_3_cluster_data =kmeans.re_position_l_athletes_3_cluster_data%>% dplyr::mutate(
  cluster_new= case_when(
    (cluster == 3) ~ 1,
    (cluster == 2)  ~ 3,
    (cluster == 1)  ~ 2
  )
)

position_l_athletes$cluster_new=kmeans.re_position_l_athletes_3_cluster_data$cluster_new

# 
# # Visualization of Clustering
# 
# position_l_athletes_skill_actions<-c(#"failed_serves","very_good_serves","perfect_serves","total_serves",
#   "failed_passes","poor_passes","very_good_passes","perfect_passes",
#   "total_passes",
#   # were excluded based on S. Drikos suggestion and too few frequenecies
#   # "failed_att1","blocked_att1","perfect_att1","total_att1",
#   #"failed_att2","blocked_att2","perfect_att2","total_att2",
#   #"failed_blocks","net_violation_blocks","perfect_blocks","total_blocks",
#   "failed_settings","total_settings"
# )
# # position_l_athletes_skill_actions<-c(#"failed_serves","very_good_serves","perfect_serves","total_serves",
# #   "failed_passes","poor_passes","very_good_passes","perfect_passes",
# #   "total_passes",
# #  # were excluded based on S. Drikos suggestion and too few frequenecies
# #   "failed_att1","blocked_att1","perfect_att1","total_att1",
# #   "failed_att2","blocked_att2","perfect_att2","total_att2",
# #   "failed_blocks","net_violation_blocks","perfect_blocks","total_blocks",
# #   "failed_settings","total_settings"
# # )
# position_l_athletes_radar_df<-position_l_athletes%>%select(all_of(position_l_athletes_skill_actions),cluster_new )%>%
#   as.data.frame()# Dataframe with variables required for radar chart plot (1st dataframe)
# # univariate summary statistics
# summary(position_l_athletes_radar_df)
# table(position_l_athletes_radar_df$poor_passes)
# table(position_l_athletes_radar_df$failed_passes)
# table(position_l_athletes_radar_df$very_good_passes)
# table(position_l_athletes_radar_df$perfect_passes)
# 
# 
# # calculate the adjusted skill actions percentage dataframe
# # position_l_athletes_radar_df_adjusted_per_skill_category<-position_l_athletes_radar_df
# # position_l_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_l_athletes_radar_df_adjusted_per_skill_category)]<-position_l_athletes_radar_df_adjusted_per_skill_category[,
# #    -ncol(position_l_athletes_radar_df_adjusted_per_skill_category)]/apply(position_l_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_l_athletes_radar_df_adjusted_per_skill_category)],1,sum)#2nd dataframe
# 
# position_l_athletes_radar_df_adjusted_per_skill_category<-position_l_athletes_radar_df
# 
# # adjustment for passes
# position_l_athletes_radar_df_adjusted_per_skill_category[,c(1:4)]<-position_l_athletes_radar_df_adjusted_per_skill_category[,c(1:4)]/
#   position_l_athletes_radar_df_adjusted_per_skill_category$total_passes
# 
# # adjustment for settings
# position_l_athletes_radar_df_adjusted_per_skill_category[,c(6)]<-position_l_athletes_radar_df_adjusted_per_skill_category[,c(6)]/
#   position_l_athletes_radar_df_adjusted_per_skill_category$total_settings
# 
# position_l_athletes_radar_df_adjusted_per_skill_category<-position_l_athletes_radar_df_adjusted_per_skill_category %>% 
#   mutate_all(~replace(., is.nan(.), 0))%>%
#   as.data.frame()
# # # Reverse the negative skills so all skills present the same inference for high values
# # position_l_athletes_radar_df[,colnames(position_l_athletes_radar_df)%in%c(
# #   "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #   "failed_settings")]<--position_l_athletes_radar_df[,colnames(position_l_athletes_radar_df)%in%c(
# #     "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #     "failed_settings")]
# 
# #### Compare every profile to an average profile
# 
# # Average skill actions by each cluster for both 1st and 2nd dataframes
# average_profiles_position_l_athletes_radar_df<-position_l_athletes_radar_df%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_l_athletes_radar_df<-average_profiles_position_l_athletes_radar_df[,-1]#remove cluster column
# rownames(average_profiles_position_l_athletes_radar_df)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# average_profiles_position_l_athletes_radar_df_adjusted_per_skill_category<-position_l_athletes_radar_df_adjusted_per_skill_category%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_l_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_l_athletes_radar_df_adjusted_per_skill_category[,-1]#remove cluster column
# rownames(average_profiles_position_l_athletes_radar_df_adjusted_per_skill_category)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# # remove very good passes since they have zero values in both 1st and 2nd df.
# average_profiles_position_l_athletes_radar_df<-average_profiles_position_l_athletes_radar_df[,!colnames(average_profiles_position_l_athletes_radar_df)%in%
#                                                                                                c("total_serves_1","total_passes_1",
#                                                                                                  "total_att1_1","total_att2_1",
#                                                                                                  "total_blocks_1","total_settings_1")]
# average_profiles_position_l_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_l_athletes_radar_df_adjusted_per_skill_category[,!colnames(average_profiles_position_l_athletes_radar_df_adjusted_per_skill_category)%in%c(
#   "total_serves_1","total_passes_1",
#   "total_att1_1","total_att2_1",
#   "total_blocks_1","total_settings_1")]
# 
# 
# # Rescale each variable to range between 0 and 1 in both 1st and 2nd df.
# average_profiles_position_l_athletes_radar_df_scaled <- round(apply(average_profiles_position_l_athletes_radar_df, 2,
#                                                                     scales::rescale), 2)
# average_profiles_position_l_athletes_radar_df_scaled <- as.data.frame(average_profiles_position_l_athletes_radar_df_scaled)
# head(average_profiles_position_l_athletes_radar_df_scaled)  
# 
# average_profiles_position_l_athletes_radar_df_scaled_adjusted <- round(apply(average_profiles_position_l_athletes_radar_df_adjusted_per_skill_category, 2,
#                                                                              scales::rescale), 2)
# average_profiles_position_l_athletes_radar_df_scaled_adjusted <- as.data.frame(average_profiles_position_l_athletes_radar_df_scaled_adjusted)
# head(average_profiles_position_l_athletes_radar_df_scaled_adjusted) 
# 
# # 1ST df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_l_athletes_radar_df_scaled, 2, max)
# col_min <- apply(average_profiles_position_l_athletes_radar_df_scaled, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_l_athletes_radar_df_scaled, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_l_athletes_radar_df_scaled2 <- as.data.frame(rbind(col_summary,
#                                                                              average_profiles_position_l_athletes_radar_df_scaled))
# head(average_profiles_position_l_athletes_radar_df_scaled2)
# 
# 
# pdf(file="Clustering_analysis_position_l_frequencies.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_l_athletes_radar_df_scaled2)) {
#   radarchart(
#     average_profiles_position_l_athletes_radar_df_scaled2[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels = c(#"Failed Serves","Very good Serves","Perfect Serves",
#       "Failed Passes","Poor Passes","Very good Passes","Perfect Passes",
#       # "Failed attack 1","Blocked attack 1","Perfect attack 1",
#       #"Failed attack 2","Blocked attack 2","Perfect attack 2",
#       #"Failed Blocks","Block net violation", "Perfect Blocks",
#       "Failed Settings"),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_l_athletes_radar_df_scaled2)[i]
#   )
# }
# dev.off()
# 
# 
# # 2ND df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_l_athletes_radar_df_scaled_adjusted, 2, max)
# col_min <- apply(average_profiles_position_l_athletes_radar_df_scaled_adjusted, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_l_athletes_radar_df_scaled_adjusted, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_l_athletes_radar_df_scaled2_adjusted <- as.data.frame(rbind(col_summary,
#                                                                                       average_profiles_position_l_athletes_radar_df_scaled_adjusted))
# head(average_profiles_position_l_athletes_radar_df_scaled2_adjusted)
# 
# 
# pdf(file="Clustering_analysis_position_l_frequencies_adjusted.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_l_athletes_radar_df_scaled2_adjusted)) {
#   radarchart(
#     average_profiles_position_l_athletes_radar_df_scaled2_adjusted[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels =   c(#"Failed Serves","Very good Serves","Perfect Serves",
#       "Failed Passes","Poor Passes","Very good Passes","Perfect Passes",
#       # "Failed attack 1","Blocked attack 1","Perfect attack 1",
#       #"Failed attack 2","Blocked attack 2","Perfect attack 2",
#       #"Failed Blocks","Block net violation", "Perfect Blocks",
#       "Failed Settings"),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_l_athletes_radar_df_scaled2_adjusted)[i]
#   )
# }
# dev.off()
# 


# M Position: ------------------

set.seed(240) # Setting seed
kmeans.re_position_m_athletes_3 <- kmeans(position_m_athletes[,ncol(position_m_athletes)], centers = 3, nstart = 20)
min(position_m_athletes$total_skill_actions[kmeans.re_position_m_athletes_3$"cluster"==2])#295
table(kmeans.re_position_m_athletes_3$cluster)#31/44, 13 out (10 from cluster 1 and 21 from cluster 2)

# Make the cluster ordered
kmeans.re_position_m_athletes_3_cluster_data<-data.frame(cluster=kmeans.re_position_m_athletes_3$cluster)
kmeans.re_position_m_athletes_3_cluster_data =kmeans.re_position_m_athletes_3_cluster_data%>% dplyr::mutate(
  cluster_new= case_when(
    (cluster == 3) ~ 1,
    (cluster == 1)  ~ 3,
    (cluster == 2)  ~ 2
  )
)
position_m_athletes$cluster_new=kmeans.re_position_m_athletes_3_cluster_data$cluster_new
# # Visualization of Clustering
# 
# position_m_athletes_skill_actions<-c("failed_serves","very_good_serves","perfect_serves","total_serves",
#                                      #"failed_passes","poor_passes","very_good_passes","perfect_passes","total_passes",
#                                      # were excluded based on S. Drikos suggestion and too few frequenecies
#                                      "failed_att1","blocked_att1","perfect_att1","total_att1",
#                                      "failed_att2","blocked_att2","perfect_att2","total_att2",
#                                      "failed_blocks","net_violation_blocks","perfect_blocks","total_blocks"
#                                      #, "failed_settings","total_settings"
# )
# # position_m_athletes_skill_actions<-c("failed_serves","very_good_serves","perfect_serves","total_serves",
# #   "failed_passes","poor_passes","very_good_passes","perfect_passes",
# #   "total_passes",
# #  # were excluded based on S. Drikos suggestion and too few frequenecies
# #   "failed_att1","blocked_att1","perfect_att1","total_att1",
# #   "failed_att2","blocked_att2","perfect_att2","total_att2",
# #   "failed_blocks","net_violation_blocks","perfect_blocks","total_blocks",
# #   "failed_settings","total_settings"
# # )
# position_m_athletes_radar_df<-position_m_athletes%>%select(all_of(position_m_athletes_skill_actions),cluster_new )%>%
#   as.data.frame()# Dataframe with variables required for radar chart plot (1st dataframe)
# # univariate summary statistics
# summary(position_m_athletes_radar_df)
# table(position_m_athletes_radar_df$poor_passes)
# table(position_m_athletes_radar_df$failed_passes)
# table(position_m_athletes_radar_df$very_good_passes)
# table(position_m_athletes_radar_df$perfect_passes)
# 
# 
# # calculate the adjusted skill actions percentage dataframe
# # position_m_athletes_radar_df_adjusted_per_skill_category<-position_m_athletes_radar_df
# # position_m_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_m_athletes_radar_df_adjusted_per_skill_category)]<-position_m_athletes_radar_df_adjusted_per_skill_category[,
# #    -ncol(position_m_athletes_radar_df_adjusted_per_skill_category)]/apply(position_m_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_m_athletes_radar_df_adjusted_per_skill_category)],1,sum)#2nd dataframe
# 
# position_m_athletes_radar_df_adjusted_per_skill_category<-position_m_athletes_radar_df
# 
# # adjustment for serves
# position_m_athletes_radar_df_adjusted_per_skill_category[,c(1:3)]<-position_m_athletes_radar_df_adjusted_per_skill_category[,c(1:3)]/
#   position_m_athletes_radar_df_adjusted_per_skill_category$total_serves
# 
# # adjustment for att1
# position_m_athletes_radar_df_adjusted_per_skill_category[,c(5:7)]<-position_m_athletes_radar_df_adjusted_per_skill_category[,c(5:7)]/
#   position_m_athletes_radar_df_adjusted_per_skill_category$total_att1
# 
# # adjustment for att2
# position_m_athletes_radar_df_adjusted_per_skill_category[,c(9:11)]<-position_m_athletes_radar_df_adjusted_per_skill_category[,c(9:11)]/
#   position_m_athletes_radar_df_adjusted_per_skill_category$total_att2
# 
# # adjustment for blocks
# position_m_athletes_radar_df_adjusted_per_skill_category[,c(13:15)]<-position_m_athletes_radar_df_adjusted_per_skill_category[,c(13:15)]/
#   position_m_athletes_radar_df_adjusted_per_skill_category$total_blocks
# 
# position_m_athletes_radar_df_adjusted_per_skill_category<-position_m_athletes_radar_df_adjusted_per_skill_category %>% 
#   mutate_all(~replace(., is.nan(.), 0))%>%
#   as.data.frame()
# # # Reverse the negative skills so all skills present the same inference for high values
# # position_m_athletes_radar_df[,colnames(position_m_athletes_radar_df)%in%c(
# #   "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #   "failed_settings")]<--position_m_athletes_radar_df[,colnames(position_m_athletes_radar_df)%in%c(
# #     "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #     "failed_settings")]
# 
# #### Compare every profile to an average profile
# 
# # Average skill actions by each cluster for both 1st and 2nd dataframes
# average_profiles_position_m_athletes_radar_df<-position_m_athletes_radar_df%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_m_athletes_radar_df<-average_profiles_position_m_athletes_radar_df[,-1]#remove cluster column
# rownames(average_profiles_position_m_athletes_radar_df)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# average_profiles_position_m_athletes_radar_df_adjusted_per_skill_category<-position_m_athletes_radar_df_adjusted_per_skill_category%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_m_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_m_athletes_radar_df_adjusted_per_skill_category[,-1]#remove cluster column
# rownames(average_profiles_position_m_athletes_radar_df_adjusted_per_skill_category)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# # remove very good passes since they have zero values in both 1st and 2nd df.
# average_profiles_position_m_athletes_radar_df<-average_profiles_position_m_athletes_radar_df[,!colnames(average_profiles_position_m_athletes_radar_df)%in%
#                                                                                                c("total_serves_1","total_passes_1",
#                                                                                                  "total_att1_1","total_att2_1",
#                                                                                                  "total_blocks_1","total_settings_1")]
# average_profiles_position_m_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_m_athletes_radar_df_adjusted_per_skill_category[,!colnames(average_profiles_position_m_athletes_radar_df_adjusted_per_skill_category)%in%c(
#   "total_serves_1","total_passes_1",
#   "total_att1_1","total_att2_1",
#   "total_blocks_1","total_settings_1")]
# 
# 
# # Rescale each variable to range between 0 and 1 in both 1st and 2nd df.
# average_profiles_position_m_athletes_radar_df_scaled <- round(apply(average_profiles_position_m_athletes_radar_df, 2,
#                                                                     scales::rescale), 2)
# average_profiles_position_m_athletes_radar_df_scaled <- as.data.frame(average_profiles_position_m_athletes_radar_df_scaled)
# head(average_profiles_position_m_athletes_radar_df_scaled)  
# 
# average_profiles_position_m_athletes_radar_df_scaled_adjusted <- round(apply(average_profiles_position_m_athletes_radar_df_adjusted_per_skill_category, 2,
#                                                                              scales::rescale), 2)
# average_profiles_position_m_athletes_radar_df_scaled_adjusted <- as.data.frame(average_profiles_position_m_athletes_radar_df_scaled_adjusted)
# head(average_profiles_position_m_athletes_radar_df_scaled_adjusted) 
# 
# # 1ST df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_m_athletes_radar_df_scaled, 2, max)
# col_min <- apply(average_profiles_position_m_athletes_radar_df_scaled, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_m_athletes_radar_df_scaled, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_m_athletes_radar_df_scaled2 <- as.data.frame(rbind(col_summary,
#                                                                              average_profiles_position_m_athletes_radar_df_scaled))
# head(average_profiles_position_m_athletes_radar_df_scaled2)
# 
# 
# pdf(file="Clustering_analysis_position_m_frequencies.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_m_athletes_radar_df_scaled2)) {
#   radarchart(
#     average_profiles_position_m_athletes_radar_df_scaled2[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels = c("Failed Serves","Very good Serves","Perfect Serves",
#                                           # "Failed Passes","Poor Passes","Very good Passes","Perfect Passes",
#                                           "Failed attack 1","Blocked attack 1","Perfect attack 1",
#                                           "Failed attack 2","Blocked attack 2","Perfect attack 2",
#                                           "Failed Blocks","Block net violation", "Perfect Blocks"
#                                           #, "Failed Settings"
#     ),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_m_athletes_radar_df_scaled2)[i]
#   )
# }
# dev.off()
# 
# 
# # 2ND df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_m_athletes_radar_df_scaled_adjusted, 2, max)
# col_min <- apply(average_profiles_position_m_athletes_radar_df_scaled_adjusted, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_m_athletes_radar_df_scaled_adjusted, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_m_athletes_radar_df_scaled2_adjusted <- as.data.frame(rbind(col_summary,
#                                                                                       average_profiles_position_m_athletes_radar_df_scaled_adjusted))
# head(average_profiles_position_m_athletes_radar_df_scaled2_adjusted)
# 
# 
# pdf(file="Clustering_analysis_position_m_frequencies_adjusted.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_m_athletes_radar_df_scaled2_adjusted)) {
#   radarchart(
#     average_profiles_position_m_athletes_radar_df_scaled2_adjusted[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels =  c("Failed Serves","Very good Serves","Perfect Serves",
#                                            # "Failed Passes","Poor Passes","Very good Passes","Perfect Passes",
#                                            "Failed attack 1","Blocked attack 1","Perfect attack 1",
#                                            "Failed attack 2","Blocked attack 2","Perfect attack 2",
#                                            "Failed Blocks","Block net violation", "Perfect Blocks"
#                                            #, "Failed Settings"
#     ),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_m_athletes_radar_df_scaled2_adjusted)[i]
#   )
# }
# dev.off()

# O Position: ------------------------------
set.seed(240) # Setting seed
kmeans.re_position_o_athletes_3 <- kmeans(position_o_athletes[,ncol(position_o_athletes)], centers = 3, nstart = 20)
min(position_o_athletes$total_skill_actions[kmeans.re_position_o_athletes_3$"cluster"==3])#339
table(kmeans.re_position_o_athletes_3$cluster)#16/22, (6 out) 7 from cluster 1 and 9 from cluster 2

# Make the cluster ordered
kmeans.re_position_o_athletes_3_cluster_data<-data.frame(cluster=kmeans.re_position_o_athletes_3$cluster)
kmeans.re_position_o_athletes_3_cluster_data =kmeans.re_position_o_athletes_3_cluster_data%>% dplyr::mutate(
  cluster_new= case_when(
    (cluster == 2) ~ 1,
    (cluster == 3)  ~ 2,
    (cluster == 1)  ~ 3
  )
)
position_o_athletes$cluster_new=kmeans.re_position_o_athletes_3_cluster_data$cluster_new

# Visualization of Clustering

# position_o_athletes_skill_actions<-c("failed_serves","very_good_serves","perfect_serves","total_serves",
#                                      #"failed_passes","poor_passes","very_good_passes","perfect_passes","total_passes",
#                                      # were excluded based on S. Drikos suggestion and too few frequenecies
#                                      "failed_att1","blocked_att1","perfect_att1","total_att1",
#                                      "failed_att2","blocked_att2","perfect_att2","total_att2",
#                                      "failed_blocks","net_violation_blocks","perfect_blocks","total_blocks"
#                                      #, "failed_settings","total_settings"
# )
# # position_o_athletes_skill_actions<-c("failed_serves","very_good_serves","perfect_serves","total_serves",
# #   "failed_passes","poor_passes","very_good_passes","perfect_passes",
# #   "total_passes",
# #  # were excluded based on S. Drikos suggestion and too few frequenecies
# #   "failed_att1","blocked_att1","perfect_att1","total_att1",
# #   "failed_att2","blocked_att2","perfect_att2","total_att2",
# #   "failed_blocks","net_violation_blocks","perfect_blocks","total_blocks",
# #   "failed_settings","total_settings"
# # )
# position_o_athletes_radar_df<-position_o_athletes%>%select(all_of(position_o_athletes_skill_actions),cluster_new )%>%
#   as.data.frame()# Dataframe with variables required for radar chart plot (1st dataframe)
# # univariate summary statistics
# summary(position_o_athletes_radar_df)
# table(position_o_athletes_radar_df$poor_passes)
# table(position_o_athletes_radar_df$failed_passes)
# table(position_o_athletes_radar_df$very_good_passes)
# table(position_o_athletes_radar_df$perfect_passes)
# 
# 
# # calculate the adjusted skill actions percentage dataframe
# # position_o_athletes_radar_df_adjusted_per_skill_category<-position_o_athletes_radar_df
# # position_o_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_o_athletes_radar_df_adjusted_per_skill_category)]<-position_o_athletes_radar_df_adjusted_per_skill_category[,
# #    -ncol(position_o_athletes_radar_df_adjusted_per_skill_category)]/apply(position_o_athletes_radar_df_adjusted_per_skill_category[,-ncol(position_o_athletes_radar_df_adjusted_per_skill_category)],1,sum)#2nd dataframe
# 
# position_o_athletes_radar_df_adjusted_per_skill_category<-position_o_athletes_radar_df
# 
# # adjustment for serves
# position_o_athletes_radar_df_adjusted_per_skill_category[,c(1:3)]<-position_o_athletes_radar_df_adjusted_per_skill_category[,c(1:3)]/
#   position_o_athletes_radar_df_adjusted_per_skill_category$total_serves
# 
# # adjustment for att1
# position_o_athletes_radar_df_adjusted_per_skill_category[,c(5:7)]<-position_o_athletes_radar_df_adjusted_per_skill_category[,c(5:7)]/
#   position_o_athletes_radar_df_adjusted_per_skill_category$total_att1
# 
# # adjustment for att2
# position_o_athletes_radar_df_adjusted_per_skill_category[,c(9:11)]<-position_o_athletes_radar_df_adjusted_per_skill_category[,c(9:11)]/
#   position_o_athletes_radar_df_adjusted_per_skill_category$total_att2
# 
# # adjustment for blocks
# position_o_athletes_radar_df_adjusted_per_skill_category[,c(13:15)]<-position_o_athletes_radar_df_adjusted_per_skill_category[,c(13:15)]/
#   position_o_athletes_radar_df_adjusted_per_skill_category$total_blocks
# 
# position_o_athletes_radar_df_adjusted_per_skill_category<-position_o_athletes_radar_df_adjusted_per_skill_category %>% 
#   mutate_all(~replace(., is.nan(.), 0))%>%
#   as.data.frame()
# # # Reverse the negative skills so all skills present the same inference for high values
# # position_o_athletes_radar_df[,colnames(position_o_athletes_radar_df)%in%c(
# #   "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #   "failed_settings")]<--position_o_athletes_radar_df[,colnames(position_o_athletes_radar_df)%in%c(
# #     "failed_serves","failed_passes","poor_passes","failed_att1","blocked_att1", "failed_att2","blocked_att2","failed_blocks",
# #     "failed_settings")]
# 
# #### Compare every profile to an average profile
# 
# # Average skill actions by each cluster for both 1st and 2nd dataframes
# average_profiles_position_o_athletes_radar_df<-position_o_athletes_radar_df%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_o_athletes_radar_df<-average_profiles_position_o_athletes_radar_df[,-1]#remove cluster column
# rownames(average_profiles_position_o_athletes_radar_df)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# average_profiles_position_o_athletes_radar_df_adjusted_per_skill_category<-position_o_athletes_radar_df_adjusted_per_skill_category%>%group_by(cluster_new)%>% 
#   summarise(across(everything(), list(mean)))%>%as.data.frame()
# average_profiles_position_o_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_o_athletes_radar_df_adjusted_per_skill_category[,-1]#remove cluster column
# rownames(average_profiles_position_o_athletes_radar_df_adjusted_per_skill_category)<-c("Cluster_1","Cluster_2","Cluster_3")
# 
# # remove very good passes since they have zero values in both 1st and 2nd df.
# average_profiles_position_o_athletes_radar_df<-average_profiles_position_o_athletes_radar_df[,!colnames(average_profiles_position_o_athletes_radar_df)%in%
#                                                                                                c("total_serves_1","total_passes_1",
#                                                                                                  "total_att1_1","total_att2_1",
#                                                                                                  "total_blocks_1","total_settings_1")]
# average_profiles_position_o_athletes_radar_df_adjusted_per_skill_category<-average_profiles_position_o_athletes_radar_df_adjusted_per_skill_category[,!colnames(average_profiles_position_o_athletes_radar_df_adjusted_per_skill_category)%in%c(
#   "total_serves_1","total_passes_1",
#   "total_att1_1","total_att2_1",
#   "total_blocks_1","total_settings_1")]
# 
# 
# # Rescale each variable to range between 0 and 1 in both 1st and 2nd df.
# average_profiles_position_o_athletes_radar_df_scaled <- round(apply(average_profiles_position_o_athletes_radar_df, 2,
#                                                                     scales::rescale), 2)
# average_profiles_position_o_athletes_radar_df_scaled <- as.data.frame(average_profiles_position_o_athletes_radar_df_scaled)
# head(average_profiles_position_o_athletes_radar_df_scaled)  
# 
# average_profiles_position_o_athletes_radar_df_scaled_adjusted <- round(apply(average_profiles_position_o_athletes_radar_df_adjusted_per_skill_category, 2,
#                                                                              scales::rescale), 2)
# average_profiles_position_o_athletes_radar_df_scaled_adjusted <- as.data.frame(average_profiles_position_o_athletes_radar_df_scaled_adjusted)
# head(average_profiles_position_o_athletes_radar_df_scaled_adjusted) 
# 
# # 1ST df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_o_athletes_radar_df_scaled, 2, max)
# col_min <- apply(average_profiles_position_o_athletes_radar_df_scaled, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_o_athletes_radar_df_scaled, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_o_athletes_radar_df_scaled2 <- as.data.frame(rbind(col_summary,
#                                                                              average_profiles_position_o_athletes_radar_df_scaled))
# head(average_profiles_position_o_athletes_radar_df_scaled2)
# 
# 
# pdf(file="Clustering_analysis_position_o_frequencies.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_o_athletes_radar_df_scaled2)) {
#   radarchart(
#     average_profiles_position_o_athletes_radar_df_scaled2[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels = c("Failed Serves","Very good Serves","Perfect Serves",
#                                           # "Failed Passes","Poor Passes","Very good Passes","Perfect Passes",
#                                           "Failed attack 1","Blocked attack 1","Perfect attack 1",
#                                           "Failed attack 2","Blocked attack 2","Perfect attack 2",
#                                           "Failed Blocks","Block net violation", "Perfect Blocks"
#                                           #, "Failed Settings"
#     ),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_o_athletes_radar_df_scaled2)[i]
#   )
# }
# dev.off()
# 
# 
# # 2ND df visualisation through radar plot
# 
# # Variables summary for the 1st var
# # Get the minimum and the max of every column  
# col_max <- apply(average_profiles_position_o_athletes_radar_df_scaled_adjusted, 2, max)
# col_min <- apply(average_profiles_position_o_athletes_radar_df_scaled_adjusted, 2, min)
# # Calculate the average profile 
# col_mean <- apply(average_profiles_position_o_athletes_radar_df_scaled_adjusted, 2, mean)
# # Put together the summary of columns
# col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# 
# 
# # Bind variables summary to the data
# average_profiles_position_o_athletes_radar_df_scaled2_adjusted <- as.data.frame(rbind(col_summary,
#                                                                                       average_profiles_position_o_athletes_radar_df_scaled_adjusted))
# head(average_profiles_position_o_athletes_radar_df_scaled2_adjusted)
# 
# 
# pdf(file="Clustering_analysis_position_o_frequencies_adjusted.pdf", width =16, height =9.5)
# #Produce radar plots showing both the average profile and the individual profile:
# opar <- par() 
# # Define settings for plotting in a 3x4 grid, with appropriate margins:
# par(mar = rep(1,4))
# par(mfrow = c(2,2))
# # Produce a radar-chart for each student
# for (i in 4:nrow(average_profiles_position_o_athletes_radar_df_scaled2_adjusted)) {
#   radarchart(
#     average_profiles_position_o_athletes_radar_df_scaled2_adjusted[c(1:3, i), ],vlcex=1.0,
#     pfcol = c("#99999980",NA),vlabels =  c("Failed Serves","Very good Serves","Perfect Serves",
#                                            # "Failed Passes","Poor Passes","Very good Passes","Perfect Passes",
#                                            "Failed attack 1","Blocked attack 1","Perfect attack 1",
#                                            "Failed attack 2","Blocked attack 2","Perfect attack 2",
#                                            "Failed Blocks","Block net violation", "Perfect Blocks"
#                                            #, "Failed Settings"
#     ),
#     pcol= c(NA,2), plty = 2, plwd = 2,
#     title = row.names(average_profiles_position_o_athletes_radar_df_scaled2_adjusted)[i]
#   )
# }
# dev.off()


# Prepare the final data for model-based evaluation in script Revised_strategy_Athletes_Evaluation.R----------------
# Join the clusters

volley_athletes_all_season_statistics<-rbind(position_h_athletes,
                                             position_l_athletes,
                                             position_m_athletes,
                                             position_s_athletes,
                                             position_o_athletes)
# Which threshold should fulfill athletes of each field Position related to their total actions in order to be included
min(position_o_athletes$total_skill_actions[kmeans.re_position_o_athletes_3$"cluster"==3])#339
min(position_h_athletes$total_skill_actions[kmeans.re_position_h_athletes_3$"cluster"==3])#1088
min(position_s_athletes$total_skill_actions[kmeans.re_position_s_athletes_3$"cluster"==3])#373
min(position_m_athletes$total_skill_actions[kmeans.re_position_m_athletes_3$"cluster"==3])#614
min(position_l_athletes$total_skill_actions[kmeans.re_position_l_athletes_3$"cluster"==3])#533

# Remove athletes of 3rd cluster

volley_athletes_all_season_statistics<-volley_athletes_all_season_statistics[volley_athletes_all_season_statistics$cluster_new!=3,]
# dim(athletes_to_be_included)#112/185~ almost a 40% of noise athletes

# Calculate the Average, Median total/cumulative sum of the skill actions across athletes of each Position
# in order to achieve a proper normalization by dividing total/cumulative sum of the skill actions  with
# their with the average ones (Correction factor).

volley_athletes_all_season_statistics_per_Position<-volley_athletes_all_season_statistics %>%
  group_by(Position) %>% dplyr::summarize(average_total_skill_actions= mean(total_skill_actions, na.rm=TRUE),
                                          median_total_skill_actions=median(total_skill_actions, na.rm=TRUE),
                                          average_total_serves= mean(total_serves, na.rm=TRUE),
                                          average_total_passes= mean(total_passes, na.rm=TRUE),
                                          average_total_att1= mean(total_att1, na.rm=TRUE),
                                          average_total_att2= mean(total_att2, na.rm=TRUE),
                                          average_total_blocks= mean(total_blocks, na.rm=TRUE),
                                          average_total_settings= mean(total_settings, na.rm=TRUE) )%>% as.data.frame()

# Join the correction factor to the corresponding athletes' of each Position.
volley_athletes_all_season_statistics<-left_join(volley_athletes_all_season_statistics,
                                                 volley_athletes_all_season_statistics_per_Position,by="Position")
dim(volley_athletes_all_season_statistics)
# For the dataset of athletes' skill actions per match, 
# we filter out the excluded ones (we will study the 112 out of the total 188, 60% of total athletes will be analysed)
volley_athletes_per_game_statistics<-volley_athletes_per_game_statistics[
  volley_athletes_per_game_statistics$Player%in%volley_athletes_all_season_statistics$Player,]
dim(volley_athletes_all_season_statistics)