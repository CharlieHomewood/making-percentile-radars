##### load packages #####

if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE); require(tidyverse)
}

if (!require(rvest)) {
  install.packages("rvest", dependencies = TRUE); require(rvest)
}

# Only keep columns relevant to each position's percentile radar
# This is left to my own judgement as to which stats "matter most" for each
# position. There is going to be debate over which metric we ought to consider
# when evaluating player performance and when comparing players - but that's
# part of the fun in following a sport.
# 
# In addition, some broad position categories will be divided further, 
# reflecting important differences in the profile of these sub-categories.
# As such, Defenders will be split into "Central Defenders" and "Wide Defenders"; 
# and midfielders will be split into "Defensive Midfielders" and "Attacking Midfielders".
# Percentiles from these subcategories will still be calculated from the overall
# broader positonal category, all that will change is which metric are displayed 
# on the percentile radars.


# GK table

percentile_plot_data_GK <- final_table_GK %>% 
  
  select(
    
    # basic player info
    Player, Pos, Nation, Squad, Comp, Age, Min,
    
    # 8 metrics for the percentile radar
    
      # Defensive performance
      `CleanSheets`, `CleanSheets_percentile`,
      `GoalsAgainst`, `GoalsAgainst_percentile`,
      
      # Goal-prevention
      `PSxG_mins_GoalsAgainst_per_90`, `PSxG_mins_GoalsAgainst_per_90_percentile`,
      `Save%`, `Save%_percentile`, 
      
      # Aerial ability
      `CrossesStopped%`, `CrossesStopped%_percentile`,
      `GoalsFromCornersConcede`, `GoalsFromCornersConcede_percentile`,
      
      # Misc
      `PassCompleted%_+40yrds`, `PassCompleted%_+40yrds_percentile`,
      `PensSaved%`, `PensSaved%_percentile`
      
    ) %>% 
  
  mutate_if(is.numeric, round, 2)

# Central Defenders table

percentile_plot_data_DF <- final_table_DF %>% 
  
  select(
    
    # basic player info
    Player, Pos, Nation, Squad, Comp, Age, Min,
    
    # 12 metrics for the percentile radar
    
      # Core Defending
      `Blocks_per90`, `Blocks_per90_percentile`,
      `DribblersTackled%`, `DribblersTackled%_percentile`,
      `Interceptions_per90`, `Interceptions_per90_percentile`,
      
      # Passing ability
      `TotalPassCompleted%`, `TotalPassCompleted%_percentile`,
      `PrgPass_per90`, `PrgPass_per90_percentile`,
      `Cross_per90`, `Cross_per90_percentile`,
    
      # Creativity
      `Ast_per90`, `Ast_per90_percentile`,
      `xA_per90`, `xA_per90_percentile`,
      `SCA_per90`, `SCA_per90_percentile`, 
    
      # Aerial ability
      `AerialsWon%`, `AerialsWon%_percentile`,
      
      # Defensive mistakes
      `FoulsCommitted_per90`, `FoulsCommitted_per90_percentile`,
      `MistakesLeadToShot_per90`, `MistakesLeadToShot_per90_percentile`
    
  ) %>% 
  
  mutate_if(is.numeric, round, 2)

# Midfielders table

percentile_plot_data_MF <- final_table_MF %>% 
  
  select(
    
    # basic player info
    Player, Pos, Nation, Squad, Comp, Age, Min,
    
    # 12 metrics for the percentile radar
    
      # Core Defending
      `DribblersTackled%`, `DribblersTackled%_percentile`,
      `Interceptions_per90`, `Interceptions_per90_percentile`,
      
      # Shooting
      `Gls_per90`, `Gls_per90_percentile`,
      `npxG_per90`, `npxG_per90_percentile`,
    
      # Creativity
      `Ast_per90`, `Ast_per90_percentile`,
      `xA_per90`, `xA_per90_percentile`,
      `SCA_per90`, `SCA_per90_percentile`,
      
      # Passing
      `TotalPassCompleted%`, `TotalPassCompleted%_percentile`,
      `PrgPass_per90`, `PrgPass_per90_percentile`, 
      `FinalThirdPass_per90`, `FinalThirdPass_per90_percentile`,
      
      # Defensive mistakes
      `FoulsCommitted_per90`, `FoulsCommitted_per90_percentile`,
      `MistakesLeadToShot_per90`, `MistakesLeadToShot_per90_percentile`
    
    
  ) %>% 
  
  mutate_if(is.numeric, round, 2)


# FW table

percentile_plot_data_FW <- final_table_FW %>% 
  
  select(
    
    # basic player info
    Player, Pos, Nation, Squad, Comp, Age, Min,
    
    # 12 metrics for the percentile radar
    
      # Shooting
      `Gls_per90`, `Gls_per90_percentile`,
      "`npxG/Sh`_per90", "`npxG/Sh`_per90_percentile",
      "`np:G-xG`_per90", "`np:G-xG`_per90_percentile",
      `ShotsOnTarget%`, `ShotsOnTarget%_percentile`,
      `Goals_per_ShotOnTarget_per90`, `Goals_per_ShotOnTarget_per90_percentile`,
      
      # Creativity
      `Ast_per90`, `Ast_per90_percentile`,
      `xA_per90`, `xA_per90_percentile`,
      `SCA_per90`, `SCA_per90_percentile`,
      
      # Dribbling
      `TakeOnsAttempted_per90`, `TakeOnsAttempted_per90_percentile`,
      `TakeOnsSuccess%`, `TakeOnsSuccess%_percentile`,
    
      # Hold-up play
      `AerialsWon%`, `AerialsWon%_percentile`,
      `Miscontrols_per90`, `Miscontrols_per90_percentile`
    
  ) %>% 
  
  mutate_if(is.numeric, round, 2)

################################################################################

# Rename variables to improve readability on the plots

percentile_plot_data_GK <- percentile_plot_data_GK %>% 
  rename(
    `Club` = "Squad",
    `Division` = "Comp",
    `Minutes Played` = "Min",
    `Clean Sheets Per 90` = "CleanSheets",
    `Clean Sheets Percentile` = "CleanSheets_percentile",
    `Goals Against Per 90` = "GoalsAgainst",
    `Goals Against Percentile` = "GoalsAgainst_percentile",
    `PSxG Minus Goals Against Per 90` = "PSxG_mins_GoalsAgainst_per_90",
    `PSxG Minus Goals Against Percentile` = "PSxG_mins_GoalsAgainst_per_90_percentile",
    `Save %` = "Save%",
    `Save % Percentile` = "Save%_percentile",
    `Crosses Stopped %` = "CrossesStopped%",
    `Crosses Stopped % Percentile` = "CrossesStopped%_percentile",
    `Goals Conceded From Corners Per 90` = "GoalsFromCornersConcede",
    `Goals Conceded From Corners Percentile` = "GoalsFromCornersConcede_percentile",
    `Pass Completion % (+40yrds)` = "PassCompleted%_+40yrds",
    `Pass Completion % (+40yrds) Percentile` = "PassCompleted%_+40yrds_percentile",
    `Pens Saved %` = "PensSaved%",
    `Pens Saved % Percentile` = "PensSaved%_percentile"
  )

percentile_plot_data_DF <- percentile_plot_data_DF %>% 
  rename(
    `Club` = "Squad",
    `Division` = "Comp",
    `Minutes Played` = "Min",
    `Blocks Per 90` = "Blocks_per90",
    `Blocks Percentile` = "Blocks_per90_percentile",
    `Dribblers Tackled %` = "DribblersTackled%",
    `Dribblers Tackled % Percentile` = "DribblersTackled%_percentile",
    `Interceptions Per 90` = "Interceptions_per90",
    `Interceptions Per 90 Percentile` = "Interceptions_per90_percentile",
    `Total Pass Completion %` = "TotalPassCompleted%",
    `Total Pass Completion % Percentile` = "TotalPassCompleted%_percentile",
    `PrgPass Per 90` = "PrgPass_per90",
    `PrgPass Percentile` = "PrgPass_per90_percentile",
    `Cross Per 90` = "Cross_per90",
    `Cross Percentile` = "Cross_per90_percentile",
    `Assists Per 90` = "Ast_per90",
    `Assists Percentile` = "Ast_per90_percentile",
    `xA Per 90` = "xA_per90",
    `xA Per 90 Percentile` = "xA_per90_percentile",
    `SCA Per 90` = "SCA_per90",
    `SCA Percentile` = "SCA_per90_percentile",
    `Aerials Won %` = "AerialsWon%",
    `Aerials Won % Percentile` = "AerialsWon%_percentile",
    `Fouls Committed Per 90` = "FoulsCommitted_per90",
    `Fouls Committed Percentile` = "FoulsCommitted_per90_percentile",
    `Mistakes Leading To Shot Per 90` = "MistakesLeadToShot_per90",
    `Mistakes Leading To Shot Percentile` = "MistakesLeadToShot_per90_percentile"
  )

percentile_plot_data_MF <- percentile_plot_data_MF %>% 
  rename(
    `Club` = "Squad",
    `Division` = "Comp",
    `Minutes Played` = "Min",
    `Dribblers Tackled %` = "DribblersTackled%",
    `Dribblers Tackled % Percentile` = "DribblersTackled%_percentile",
    `Interceptions Per 90` = "Interceptions_per90",
    `Interceptions Per 90 Percentile` = "Interceptions_per90_percentile",
    `Goals Per 90` = "Gls_per90",
    `Goals Percentile` = "Gls_per90_percentile",
    `npxG Per 90` = "npxG_per90",
    `npxG Percentile` = "npxG_per90_percentile",
    `Assists Per 90` = "Ast_per90",
    `Assists Percentile` = "Ast_per90_percentile",
    `xA Per 90` = "xA_per90",
    `xA Per 90 Percentile` = "xA_per90_percentile",
    `SCA Per 90` = "SCA_per90",
    `SCA Percentile` = "SCA_per90_percentile",
    `Total Pass Completion %` = "TotalPassCompleted%",
    `Total Pass Completion % Percentile` = "TotalPassCompleted%_percentile",
    `PrgPass Per 90` = "PrgPass_per90",
    `PrgPass Percentile` = "PrgPass_per90_percentile",
    `Final Third Passes Per 90` = "FinalThirdPass_per90",
    `Final Third Passes Percentile` = "FinalThirdPass_per90_percentile",
    `Fouls Committed Per 90` = "FoulsCommitted_per90",
    `Fouls Committed Percentile` = "FoulsCommitted_per90_percentile",
    `Mistakes Leading To Shot Per 90` = "MistakesLeadToShot_per90",
    `Mistakes Leading To Shot Percentile` = "MistakesLeadToShot_per90_percentile"
  )


percentile_plot_data_FW <- percentile_plot_data_FW %>% 
  rename(
    `Club` = "Squad",
    `Division` = "Comp",
    `Minutes Played` = "Min",
    `Goals Per 90` = "Gls_per90",
    `Goals Percentile` = "Gls_per90_percentile",
    `npxG/Shot Per 90` = "`npxG/Sh`_per90",
    `npxG/Shot Percentile` = "`npxG/Sh`_per90_percentile",
    `np:G-xG Per 90` = "`np:G-xG`_per90",
    `np:G-xG Percentile` = "`np:G-xG`_per90_percentile",
    `Shots On Target %` = "ShotsOnTarget%",
    `Shots On Target % Percentile` = "ShotsOnTarget%_percentile",
    `Goals/Shot On Target Per 90` = "Goals_per_ShotOnTarget_per90",
    `Goals/Shot On Target Percentile` = "Goals_per_ShotOnTarget_per90_percentile",
    `Assists Per 90` = "Ast_per90",
    `Assists Percentile` = "Ast_per90_percentile",
    `xA Per 90` = "xA_per90",
    `xA Per 90 Percentile` = "xA_per90_percentile",
    `SCA Per 90` = "SCA_per90",
    `SCA Percentile` = "SCA_per90_percentile",
    `Take Ons Attempted Per 90` = "TakeOnsAttempted_per90",
    `Take Ons Attempted Percentile` = "TakeOnsAttempted_per90_percentile",
    `Take Ons Success %` = "TakeOnsSuccess%",
    `Take Ons Success % Percentile` = "TakeOnsSuccess%_percentile",
    `Aerials Won %` = "AerialsWon%",
    `Aerials Won % Percentile` = "AerialsWon%_percentile",
    `Miscontrols Per 90` = "Miscontrols_per90",
    `Miscontrols Percentile` = "Miscontrols_per90_percentile"
    
  )

################################################################################

# Defining percentile radar bar orders and colours

percentile_radar_bar_orders <- list(
  
  "Goalkeepers" = c(
    
    "Clean Sheets", "Goals Against", 
    
    "PSxG Minus Goals Against", "Save %",
    
    "Crosses Stopped %", "Goals Conceded From Corners",
    
    "Pass Completion % (+40yrds)", "Pens Saved %"
    
    ),
  
  "Defenders" = c(
    
    "Blocks Percentile", "Dribblers Tackled % Percentile", "Interceptions Per 90 Percentile",
    
    "Total Pass Completion % Percentile", "PrgPass Percentile", "Cross Percentile",
    
    "Assists Percentile", "xA Per 90 Percentile", "SCA Percentile",
    
    "Aerials Won % Percentile",
    
    "Fouls Committed Percentile", "Mistakes Leading To Shot Percentile"
    
  ),
  
  "Midfielders" = c(
    
    "Dribblers Tackled % Percentile", "Interceptions Per 90 Percentile",
    
    "Goals Percentile", "npxG Percentile",
    
    "Assists Percentile", "xA Per 90 Percentile", "SCA Percentile",
    
    "Total Pass Completion % Percentile", "PrgPass Percentile", "Final Third Passes Percentile",
    
    "Fouls Committed Percentile", "Mistakes Leading To Shot Percentile"
    
  ),
  
  "Forwards" = c(
    
    "Goals Percentile", "npxG/Shot Percentile", "np:G-xG Percentile", 
    "Shots On Target % Percentile", "Goals/Shot On Target Percentile",
    
    "Assists Percentile", "xA Per 90 Percentile", "SCA Percentile",
    
    "Take Ons Attempted Percentile", "Take Ons Success % Percentile",
    
    "Aerials Won % Percentile", "Miscontrols Percentile"
    
  )
   
)

percentile_radar_bar_colours <- list(
  
  "Goalkeepers" = c(
    
    rep("#629dfc", 2),
    rep("#fc6262", 2),
    rep("#83fc62", 2),
    rep("#fcfc62", 2)
    
  ),
  
  "Defenders" = c(
    
    rep("#629dfc", 3),
    rep("#fc6262", 3),
    rep("#83fc62", 3),
    rep("#fcfc62", 1),
    rep("#a862fc", 2)
    
  ),
  
  "Midfielders" = c(
    
    rep("#629dfc", 2),
    rep("#fc6262", 2),
    rep("#83fc62", 3),
    rep("#fcfc62", 3),
    rep("#a862fc", 2)
    
  ),
  
  "Forwards" = c(
    
    rep("#629dfc", 5),
    rep("#fc6262", 3),
    rep("#83fc62", 2),
    rep("#fcfc62", 2)
    
  )
  
)


















