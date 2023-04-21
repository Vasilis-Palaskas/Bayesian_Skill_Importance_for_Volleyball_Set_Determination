
# Radial Plots for each field Position cluster-------------------
volley_athletes_all_season_statistics_inverse_negative_skills<-volley_athletes_all_season_statistics

volley_athletes_all_season_statistics_inverse_negative_skills$failed_serves<-1/volley_athletes_all_season_statistics$failed_serves
volley_athletes_all_season_statistics_inverse_negative_skills$failed_passes<-1/volley_athletes_all_season_statistics$failed_passes
volley_athletes_all_season_statistics_inverse_negative_skills$poor_passes<-1/volley_athletes_all_season_statistics$poor_passes
volley_athletes_all_season_statistics_inverse_negative_skills$failed_att1<-1/volley_athletes_all_season_statistics$failed_att1
volley_athletes_all_season_statistics_inverse_negative_skills$blocked_att1<-1/volley_athletes_all_season_statistics$blocked_att1
volley_athletes_all_season_statistics_inverse_negative_skills$failed_att2<-1/volley_athletes_all_season_statistics$failed_att2
volley_athletes_all_season_statistics_inverse_negative_skills$blocked_att2<-1/volley_athletes_all_season_statistics$blocked_att2
volley_athletes_all_season_statistics_inverse_negative_skills$failed_blocks<-1/volley_athletes_all_season_statistics$failed_blocks
volley_athletes_all_season_statistics_inverse_negative_skills$net_violation_blocks<-1/volley_athletes_all_season_statistics$net_violation_blocks
volley_athletes_all_season_statistics_inverse_negative_skills$failed_settings<-1/volley_athletes_all_season_statistics$failed_settings

volley_athletes_all_season_statistics_inverse_negative_skills<-do.call(data.frame,                      # Replace Inf in data by NA
        lapply(volley_athletes_all_season_statistics_inverse_negative_skills,
               function(x) replace(x, is.infinite(x), 0)))

# Skills names
skill_actions<-c("failed_serves",
"very_good_serves","perfect_serves","failed_passes",
"poor_passes","very_good_passes","perfect_passes",
"failed_att1","blocked_att1","perfect_att1",
"failed_att2","blocked_att2","perfect_att2",
"failed_blocks","net_violation_blocks",
"perfect_blocks","failed_settings")
# Radial Plots for Position S


position_s_radial_analysis<-volley_athletes_all_season_statistics_inverse_negative_skills[
  volley_athletes_all_season_statistics_inverse_negative_skills$Position=="S",colnames(volley_athletes_all_season_statistics_inverse_negative_skills)%in%c(
    skill_actions)]
# Radial Plots for Position H

position_h_radial_analysis<-volley_athletes_all_season_statistics_inverse_negative_skills[
  volley_athletes_all_season_statistics_inverse_negative_skills$Position=="H",colnames(volley_athletes_all_season_statistics_inverse_negative_skills)%in%c(
    skill_actions)]
# Radial Plots for Position L

position_l_radial_analysis<-volley_athletes_all_season_statistics_inverse_negative_skills[
  volley_athletes_all_season_statistics_inverse_negative_skills$Position=="L",colnames(volley_athletes_all_season_statistics_inverse_negative_skills)%in%c(
    skill_actions)]
# Radial Plots for Position M

position_m_radial_analysis<-volley_athletes_all_season_statistics_inverse_negative_skills[
  volley_athletes_all_season_statistics_inverse_negative_skills$Position=="M",colnames(volley_athletes_all_season_statistics_inverse_negative_skills)%in%c(
    skill_actions)]
# Radial Plots for Position O

position_o_radial_analysis<-volley_athletes_all_season_statistics_inverse_negative_skills[
  volley_athletes_all_season_statistics_inverse_negative_skills$Position=="O",colnames(volley_athletes_all_season_statistics_inverse_negative_skills)%in%c(
    skill_actions)]
# 1) Reverse the negative impact skills in order to have meaning/impact as the main ones.
#
# Quantile of total skill actions
quantile(position_s_athletes$total_skill_actions,probs = 0.3)#373
quantile(position_h_athletes$total_skill_actions,probs = 0.3)#73.7
quantile(position_l_athletes$total_skill_actions,probs = 0.3)#58.8
quantile(position_m_athletes$total_skill_actions,probs = 0.3)#289.8
quantile(position_o_athletes$total_skill_actions,probs = 0.3)#351



# 1) Reverse the negative impact skills in order to have meaning/impact as the main ones.
#
# Quantile of total skill actions
# quantile(position_s_athletes$total_skill_actions,probs = 0.3)#373
# quantile(position_h_athletes$total_skill_actions,probs = 0.3)#73.7
# quantile(position_l_athletes$total_skill_actions,probs = 0.3)#58.8
# quantile(position_m_athletes$total_skill_actions,probs = 0.3)#289.8
# quantile(position_o_athletes$total_skill_actions,probs = 0.3)#351

#  Remove the noise data of athletes by keeping athletes implemented more cumulative sum skill actions in the end of the season
#  than the minimum skill actions of the second worse cluster of the cumulative sum skill actions of their own Position.
position_s_athletes_removing_least_skill_actions<-position_s_athletes[position_s_athletes$total_skill_actions>
                                                                        373 ,]#
position_h_athletes_removing_least_skill_actions<-position_h_athletes[position_h_athletes$total_skill_actions>
                                                                        513 ,]#
position_l_athletes_removing_least_skill_actions<-position_l_athletes[position_l_athletes$total_skill_actions>
                                                                        275 ,]#
position_m_athletes_removing_least_skill_actions<-position_m_athletes[position_m_athletes$total_skill_actions>
                                                                        295 ,]# 
position_o_athletes_removing_least_skill_actions<-position_o_athletes[position_o_athletes$total_skill_actions>
                                                                        339 ,]# 

# Create the Pool of athletes finally selected based on the selection/filtering threshold across all Positions/
volley_athletes_all_season_statistics<-rbind(position_s_athletes_removing_least_skill_actions,
                                             position_h_athletes_removing_least_skill_actions,
                                             position_l_athletes_removing_least_skill_actions,
                                             position_m_athletes_removing_least_skill_actions,
                                             position_o_athletes_removing_least_skill_actions)
dim(volley_athletes_all_season_statistics)#107 athletes will be studied out of the total 188 ones.

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

# For the dataset of athletes' skill actions per match, we filter out the excluded ones (we will study the 148 out of the total 188)
volley_athletes_per_game_statistics<-volley_athletes_per_game_statistics[
  volley_athletes_per_game_statistics$Player%in%volley_athletes_all_season_statistics$Player,]
dim(volley_athletes_per_game_statistics)#185 athletes
