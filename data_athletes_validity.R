
head(volley_athletes_per_game_statistics,20)
head(volley_athletes_all_season_statistics,20)


dim(volley_athletes_per_game_statistics)
dim(volley_athletes_all_season_statistics)
# Validity that the actions are actually divided by 22 to extract statistics in the dataset per game
volley_athletes_per_game_statistics[volley_athletes_per_game_statistics$Player%in%"1 MOUCHLIAS D.",]
volley_athletes_all_season_statistics[volley_athletes_all_season_statistics$Player%in%"1 MOUCHLIAS D.",]

volley_athletes_per_game_statistics[volley_athletes_per_game_statistics$Player%in%"12 STIGGAS",]
volley_athletes_all_season_statistics[volley_athletes_all_season_statistics$Player%in%"12 STIGGAS",]
# Check the total frequencies of the skill actions 
apply(volley_athletes_all_season_statistics[,colnames(volley_athletes_all_season_statistics)%in%c("total_serves",
                                                                                                  "total_passes",
                                                                                                  "total_att1",
                                                                                                  "total_att2",
                                                                                                  "total_blocks",
                                                                                                  "total_settings" )],1,sum)

apply(volley_athletes_per_game_statistics[,colnames(volley_athletes_per_game_statistics)%in%c("total_serves",
                                                                                              "total_passes",
                                                                                              "total_att1",
                                                                                              "total_att2",
                                                                                              "total_blocks",
                                                                                              "total_settings" )],1,sum)

# Model parameters
print(ZDTS_only_Skills_after_BVS,
      pars=c("mu","home",
             "beta_home","beta_away","dev"),probs = c(0.025,0.5,0.975), digits=2)
# Dataset incluiding athletes' actions per game (naive assumption of average annual skill actions as skill actions per game)
head(data_by_sets_athletes)


# Dataframes representing in the home, away matches, respectively, their team, Position, BVS-Selected skill actions

head(home_data_by_sets_athletes_distinct)
head(away_data_by_sets_athletes_distinct)

# 11+10 matches Petkovic
home_data_by_sets_athletes_distinct[home_data_by_sets_athletes_distinct$Home_Player.x%in%"1 PETKOVIC V.",]
away_data_by_sets_athletes_distinct[away_data_by_sets_athletes_distinct$Away_Player.y%in%"1 PETKOVIC V.",]

# 11+11 matches MOUCHLIAS D.
home_data_by_sets_athletes_distinct[home_data_by_sets_athletes_distinct$Home_Player.x%in%"1 MOUCHLIAS D.",]
away_data_by_sets_athletes_distinct[away_data_by_sets_athletes_distinct$Away_Player.y%in%"1 MOUCHLIAS D.",]
# 11+10 matches 11 GOMEZ E. (Paok has played twice in his home ground with kifisia)
home_data_by_sets_athletes_distinct[home_data_by_sets_athletes_distinct$Home_Player.x%in%"11 GOMEZ E.",]
away_data_by_sets_athletes_distinct[away_data_by_sets_athletes_distinct$Away_Player.y%in%"11 GOMEZ E.",]
