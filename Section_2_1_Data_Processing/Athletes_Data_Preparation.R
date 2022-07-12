
#Read and further processing of the athletes' data

# setwd("C:/Users/vasileios palaskas/Desktop/Github folder/Bayesian_Variable_Selection_volley_athletesball/ZDTS_Skills_Revised")
volley_athletes<-read.csv("C:\\Users\\vasileios palaskas\\Desktop\\Github folder\\Bayesian_Variable_Selection_Volleyball\\Extra_Data_Processes\\Data\\All_teams_selected_data_per_player.csv",
                 header=T)#Load Data\Final_Regular_Season_data.csv
head(volley_athletes,30)
head(data_by_sets)
colnames(volley_athletes)<-c("team","Player","Position","total_serves","failed_serves",
                             "very_good_serves","perfect_serves","total_passes","failed_passes",
                             "poor_passes","very_good_passes","perfect_passes",
                             "total_att1","failed_att1","blocked_att1","perfect_att1",
                             "total_att2","failed_att2","blocked_att2","perfect_att2",
                             "total_blocks","failed_blocks","net_violation_blocks",
                             "perfect_blocks","total_settings","failed_settings")
head(volley_athletes,30)

str(volley_athletes)
volley_athletes$team<-factor(volley_athletes$team,levels = levels(data_by_sets$home_Team))
dim(volley_athletes)###200 and 26 variables

#--Remove statistics and rows of total executions by each team
volley_athletes<-volley_athletes[!volley_athletes$Player=="Team",]
head(volley_athletes,20)
dim(volley_athletes)###188 and 26 variables
#--Change the structure of poor passes
volley_athletes$poor_passes<-as.numeric(volley_athletes$poor_passes)

#---Replace NA with 0 across statistics/skills executions

nas <- which(is.na(volley_athletes$col))
emptystrs <- which(volley_athletes$col == "")
nas_or_empty <- which(is.na(volley_athletes$col)|volley_athletes$col == "")
volley_athletes[volley_athletes==""|is.na(volley_athletes)]<-0                                                                                                                           
head(volley_athletes,20)
dim(volley_athletes)###188 and 26 variables
volley_athletes_all_season_statistics<-volley_athletes

#--- Divide all statistics executions by the number of all regular season matches (n=11*2 round robin)
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

