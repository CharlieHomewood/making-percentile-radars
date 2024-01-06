##### load packages #####

if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE); require(tidyverse)
}

if (!require(rvest)) {
  install.packages("rvest", dependencies = TRUE); require(rvest)
}

##### web scraping #####

# read main webpage html
urls <- read_html("https://fbref.com/en/comps/Big5/Big-5-European-Leagues-Stats") %>% 
  # select all anchor nodes
  html_nodes("a") %>% 
  # grab the href attributes from those anchor nodes
  html_attr("href") %>%  
  # filter to urls we want, save as object
  str_subset(., "players/Big-5") %>% 
  # remove duplicates by using unique()
  unique() %>% 
  paste0("https://fbref.com", .)

# make an empty list for the tables to go in
tables <- list()

# loop through urls to extract tables
for (url in urls) {
  
  # each url contains a name that indicates what the table represents
  # extract the name from the url
  # Explaining: [^/]+
  ## ^ negates the term succeeding it. 
  ## + says the pattern should match multiple occurrences
  # flatten_chr() turns a list into a character vector
  # nth(6) selects the 6th value in the character vector (i.e. the table name)
  name <- url %>% str_extract_all("[^/]+") %>% flatten_chr() %>% nth(6)
  
  # save each html table as "table"
  tables[[name]] <- read_html(url) %>%
    html_nodes("table") %>%
    html_table() %>% 
    lapply(as.data.frame)
  
  # print console message to show progress
  print(paste0("New table saved: ", name))
  
  # delete the temporary objects used in the function
  rm(url, name)
  
}

##### data cleaning #####

# the for loop creates a structure like this:
# tables
#  - [[1]]
#   - *table 1*
#  - [[2]]
#   - *table 2*
# ...
# 
# we need to flatten out the first layer of sub-containers so that we just have:
# tables
# - *table 1*
# - *table 2*
# ...
tables <- unlist(tables, recursive = FALSE)

# recursive = false prevents the unlist() function from flattening all the table
# columns as well

# the next function applies a few format changes to each table.

tables <- lapply(tables, function(table) {
  
  # replace blank spaces in first row values with underscores
  table[1, ] <- gsub("\\s", "_", table[1, ])  
  
  # append first row values to beginning of column names
  colnames(table) <- sub("^(\\S+)\\s.*", "\\1", paste0(table[1, ], " ", colnames(table)))
  
  # last column is just a series of "matches", not needed
  table <- table[, 1:(ncol(table) - 1)]
  
  # filter where Age != "Age" to get rid of header rows
  table <- table %>% 
    filter(Age != "Age")
  
  # makes numeric "id" column, removes old "Rk" column
  table <- table %>%
    rename(id = Rk) %>% 
    mutate(id = as.numeric(id))
  
  # take the changes we've made and apply them to the "tables" list  
  return(table)
  
  rm(table)
  
})

# parse out the goalkeeper tables from the "tables" list. Goalkeeper tables only 
# contain goalkeepers and so have different number of rows.

gk_tables <- tables[c(2,3)]
tables <- tables[-c(2,3)]

# Some column names are duplicates and others are not clear enough. Need to fix.

### Tables ###

# loop through each table
### this is for checking column names, not actually for cleaning the data directly.
for (i in seq_along(tables)) {
  
  # get the table name
  table_name <- names(tables)[i]
  
  # print the table name
  cat("Table Name:", table_name, "\n")
  
  # get the column names of the current table
  col_names <- colnames(tables[[i]])
  
  # print the column names
  cat("Column Names:", paste(col_names, collapse = ", "), "\n\n")
  
  # delete temp objects
  rm(table_name, col_names, i)
  
}

# stats #

# the "stats" table as a "per 90" stats category. I want to append "per 90" to
# the end of these column names.

# rename these columns
tables[["stats"]] <- tables[["stats"]] %>%
  rename_with(
    ~paste(
      sub("\\.[0-9]+", "", .), 
      " per 90", 
      sep = ""
    ),
    .cols = 28:length(.)
  )

# extra renaming for clarity / personal preference
tables[["stats"]] <- tables[["stats"]] %>% 
  rename(
    Pens = PK,
    PensAtt = PKatt,
    Yellows = CrdY,
    Reds = CrdR,
    PrgCarries = PrgC,
    PrgPass = PrgP,
    PrgPassRecieved = PrgR
  )

# shooting #
tables[["shooting"]] <- tables[["shooting"]] %>% 
  rename(
    Shots = Sh,
    ShotsOnTarget = SoT,
    "ShotsOnTarget%" = "SoT%",
    "Shots_per_90" = "Sh/90",
    "ShotsOnTarget_per_90" = "SoT/90",
    "Goals_per_shot" = "G/Sh",
    "Goals_per_ShotOnTarget" = "G/SoT",
    AvrShotDistance = Dist,
    FreeKicks = FK,
    Pens = PK,
    PensAtt = PKatt
  )

# passing #
tables[["passing"]] <- tables[["passing"]] %>% 
  rename(
    TotalPassCompleted = Cmp,
    TotalPassAttempted = Att,
    "TotalPassCompleted%" = "Cmp%",
    TotalPassDist = TotDist,
    TotalProgPassDist = PrgDist,
    ShortPassCompleted = Cmp.1,
    ShortPassAttempted = Att.1,
    "ShortPassCompleted%" = "Cmp%.1", 
    MedPassCompleted = Cmp.2, 
    MedPassAttempted = Att.2, 
    "MedPassCompleted%" = "Cmp%.2", 
    LongPassCompleted = Cmp.3, 
    LongPassAttempted = Att.3, 
    "LongPassCompleted%" = "Cmp%.3",
    KeyPasses = KP,
    FinalThirdPass = "1/3",
    PassIntoPenArea = PPA,
    CrossIntoPenArea = CrsPA,
    PrgPass = PrgP
  )

# passing_types #
tables[["passing_types"]] <- tables[["passing_types"]] %>% 
  rename(
    TotalPassAttempted = Att,
    LiveBallPass = Live,
    DeadBallPass = Dead,
    FreeKickPass = FK,
    ThroughBallPass = TB,
    SwitchPass = Sw,
    Cross = Crs,
    ThrowIn = TI,
    CornerAttempts = CK,
    InSwingCorners = In,
    OutSwingCorners = Out,
    StraightCorners = Str,
    TotalPassCompleted = Cmp,
    OffsidePass = Off,
    BlockedPass = Blocks
  )

# gca (goal-creating actions) #
tables[["gca"]] <- tables[["gca"]] %>% 
  rename(
    "SCA_per_90" = "SCA90",
    LiveBallPassSCA = PassLive,
    DeadBallPassSCA = PassDead,
    TakeOnsSCA = TO,
    ShotSCA = Sh,
    FoulsSCA = Fld,
    DefendActionSCA = Def,
    "GCA_per_90" = "GCA90",
    LiveBallPassGCA = "PassLive.1",
    DeadBallPassGCA = "PassDead.1",
    TakeOnsGCA = "TO.1",
    ShotGCA = "Sh.1",
    FoulsGCA = "Fld.1",
    DefendActionGCA = "Def.1",
  )

# defense #
tables[["defense"]] <- tables[["defense"]] %>% 
  rename(
    PlayersTackled = Tkl,
    TacklesWon = TklW,
    "TacklesDef3rd" = "Def_3rd",
    "TacklesMid3rd" = "Mid_3rd",
    "TacklesAtt3rd" = "Att_3rd",
    DribblersTackled = "Tkl.1",
    DribblerTackleAttempts = Att,
    "DribblersTackled%" = "Tkl%",
    DribblersTackledLost = Lost,
    ShotsBlocked = Sh,
    PassesBlocked = Pass,
    Interceptions = Int,
    "Tackles+Interceptions" = "Tkl+Int",
    Clearances = Clr,
    MistakesLeadToShot = Err
  )

# possession #
tables[["possession"]] <- tables[["possession"]] %>% 
  rename(
    TouchesDefPenArea = Def_Pen,
    "TouchesDef3rd" = "Def_3rd",
    "TouchesMid3rd" = "Mid_3rd",
    "TouchesAtt3rd" = "Att_3rd",
    TouchesAttPenArea = Att_Pen,
    LiveBallTouches = Live,
    TakeOnsAttempted = Att,
    TakeOnsSuccess = Succ,
    "TakeOnsSuccess%" = "Succ%",
    TackledDuringTakeOn = Tkld,
    "TackledDuringTakeOn%" = "Tkld%",
    TotalCarryDist = "TotDist",
    PrgCarryDist = "PrgDist",
    PrgCarries = PrgC,
    FinalThirdCarries = "1/3",
    PenAreaCarries = CPA,
    Miscontrols = Mis,
    Dispossessed = Dis,
    PassRecieved = Rec,
    PrgPassRecieved = PrgR
  )

# playingtime #
tables[["playingtime"]] <- tables[["playingtime"]] %>% 
  rename(
    Min_per_MP = "Mn/MP",
    "Min_%ofSquad" = "Min%",
    Min_per_start = "Mn/Start",
    MatchesCompleted = Compl,
    MatchesStartedAsSub = Subs,
    Min_per_sub = "Mn/Sub",
    NotUsedSub = unSub,
    Points_per_match = PPM,
    TeamGoalOnPitch = onG,
    TeamConcedeOnPitch = onGA,
    TeamGoal_minus_TeamConcede_OnPitch = "+/-",
    TeamGoal_minus_TeamConcede_OnPitch_per_90 = "+/-90",
    NetGoalOnPitch_minus_NetGoalOffPitch_per_90 = "On-Off",
    # yes, I know these names are long :)
    # It's hard to summarise these variables
    # if you use this code, feel free to rename
    xG_OnPitch = onxG,
    xGA_OnPitch = onxGA,
    xG_minus_xGA_OnPitch = "xG+/-",
    xG_minus_xGA_OnPitch_per_90 = "xG+/-90",
    NetxG_minus_NetxG_OnPitch_per_90 = "On-Off.1"
  )

# misc #
tables[["misc"]] <- tables[["misc"]] %>% 
  rename(
    Yellows = CrdY,
    Reds = CrdR,
    SecondYellows = "2CrdY",
    FoulsCommitted = Fls,
    FoulsDrawn = Fld,
    Offsides = Off,
    Cross = Crs,
    Interceptions = Int,
    TacklesWon = TklW,
    PensWon = PKwon,
    PensAllowed = PKcon,
    OwnGoals = OG, 
    Recoveries = Recov,
    AerialsWon = Won,
    AerialsLost = Lost,
    "AerialsWon%" = "Won%"
  )

### gk_tables ###

# loop through each table
### this is for checking column names, not actually for cleaning the data directly.
for (i in seq_along(gk_tables)) {
  
  # get the table name
  table_name <- names(gk_tables)[i]
  
  # print the table name
  cat("Table Name:", table_name, "\n")
  
  # get the column names of the current table
  col_names <- colnames(gk_tables[[i]])
  
  # print the column names
  cat("Column Names:", paste(col_names, collapse = ", "), "\n\n")
  
  # delete temp objects
  rm(table_name, col_names, i)
}

# keepers #
gk_tables[["keepers"]] <- gk_tables[["keepers"]] %>% 
  rename(
    GoalsAgainst = GA,
    "GoalsAgainst_per_90" = "GA90",
    ShotOnTargetAgainst = SoTA,
    Wins = W,
    Draws = D,
    Losses = L,
    CleanSheets = CS,
    "CleanSheet%" = "CS%",
    PensFaced = PKatt,
    PensAllowed = PKA,
    PensSaved = PKsv,
    PensMissesFaced = PKm,
    "PensSaved%" = "Save%.1"
  )

# keepersadv #
gk_tables[["keepersadv"]] <- gk_tables[["keepersadv"]] %>% 
  rename(
    GoalsAgainst = GA,
    PensAllowed = PKA,
    FreeKickConcede = FK,
    GoalsFromCornersConcede = CK,
    OwnGoalsConcedeFromOthers = OG,
    PSxG_per_ShotOnTarget = "PSxG/SoT",
    PSxG_mins_GoalsAgainst = "PSxG+/-",
    PSxG_mins_GoalsAgainst_per_90 = "/90",
    "TotalPassCompleted_+40yrds" = Cmp,
    "TotalPassAttempted_+40yrds" = Att,
    "PassCompleted%_+40yrds" = "Cmp%",
    TotalPassAttempted_NoGoalKicks = "Att_(GK)",
    ThrowsAttempted = Thr,
    "PassAttempted_+40yrds%_NoGoalKicks" = "Launch%",
    AvrPassLength_NoGoalKicks = AvgLen,
    GoalKicksAttempted = "Att.1",
    "GoalKick_+40yrds%" = "Launch%.1",
    AvrGoalKickDist = AvgLen.1,
    CrossesFaced = Opp,
    CrossesStopped = Stp,
    "CrossesStopped%" = "Stp%",
    DefActionsOutsidePenArea = "#OPA",
    DefActionsOutsidePenArea_per_90 = "#OPA/90",
    AvrDefActionDistFromGoal = AvgDist
  )

# at present, whilst gk_tables are separated, there are still goalkeepers in 
# the "tables" data frames. 

# empty list for goalkeeper tables
gk_tables_1 <- list() 

# loop through each table in "tables"
for (i in seq_along(tables)) {
  
  # grab rows where Pos == "GK", save to "table"
  table <- tables[[i]][tables[[i]]$Pos == "GK", ]
  
  # add "table" to the "gk_tables_1" list
  gk_tables_1[[names(tables)[i]]] <- table
  
  # remove rows where Pos == "GK" from original table
  tables[[i]] <- tables[[i]][tables[[i]]$Pos != "GK", ]
  
  # delete temp objects
  rm(table, i)
  
}

# now we have three table lists:
## "tables", containing all outfield players (no goalkeepers)
## "gk_tables", containing goalkeeper-specific tables
## "gk_tables_1", containing non goalkeeper-specific tables but with only 
## goalkeepers 

# for "gk_tables", all data frames have equal rows

# BUT:
# for "tables", all data frames have equal rows except "playingtime"
# for "gk_tables_1", all data frames have equal rows except "playingtime"

# problem is, playingtime contains players who were unused subs, and thus did
# not record any data in the other tables. We need to store these players in a 
# separate data frame (unused_subs)

unused_subs <- unique(bind_rows(
  anti_join(tables[["playingtime"]], tables[["stats"]], by = "Player"),
  filter(tables[["playingtime"]], is.na(Min) | Min == "" | Min == 0)
))

tables[["playingtime"]] <- anti_join(tables[["playingtime"]], unused_subs, by = "90s")

unused_subs_gk <- unique(bind_rows(
  anti_join(gk_tables_1[["playingtime"]], gk_tables_1[["stats"]], by = "Player"),
  filter(gk_tables_1[["playingtime"]], is.na(Min) | Min == "" | Min == 0)
))

gk_tables_1[["playingtime"]] <- anti_join(gk_tables_1[["playingtime"]], unused_subs_gk, by = "Min")

# rows are now all equal across respective lists (i.e. we have the same players 
# in each table across each list)

# combine gk_tables lists together
gk_tables <- append(gk_tables, gk_tables_1)
# remove gk_tables_1
rm(gk_tables_1)

# define first table from each list as the start point
final_table_outfield <- tables[["stats"]]
final_table_gks <- gk_tables[["keepers"]]

# compare subsequent tables in the list to the current table, add columns not 
# found in current table to it and iterate for whole list (for "tables")
for (i in 2:length(tables)) {
  
  new_cols <- setdiff(colnames(tables[[i]]), colnames(final_table_outfield))
  
  final_table_outfield <- cbind(final_table_outfield, tables[[i]][new_cols])
  
  rm(new_cols, i)
  
}

# compare subsequent tables in the list to the current table, add columns not 
# found in current table to it and iterate for whole list (for "gk_tables")
for (i in 2:length(gk_tables)) {
  
  new_cols <- setdiff(colnames(gk_tables[[i]]), colnames(final_table_gks))
  
  final_table_gks <- cbind(final_table_gks, gk_tables[[i]][new_cols])
  
  rm(new_cols, i)
  
}

# need to remove commas from Min column
final_table_outfield$Min <- gsub(",", "", final_table_outfield$Min)
final_table_gks$Min <- gsub(",", "", final_table_gks$Min)

# convert numeric columns to numeric (except age and born, for now)
final_table_outfield[ , 9:ncol(final_table_outfield)] <- lapply(
  final_table_outfield[ , 9:ncol(final_table_outfield)],
  as.numeric
)

final_table_gks[ , 9:ncol(final_table_gks)] <- lapply(
  final_table_gks[ , 9:ncol(final_table_gks)],
  as.numeric
)

# some players have multiple rows (due to transfers during season)
# need to combine these rows together. Can combine the numeric columns directly,
# but need to combine the character columns in an appropriate way.
# i.e. squad cell value reads: team 1, team 2 
# also, some players have the same name. Need to combine duplicates without 
# combining different players who have the same name. Can group_by name, age, 
# and nationality.
### however, if two players have the same name, age and nationality
### then they will merge. This is unlikely but a possibility. I can't think of
### another way to ensure different players with identical information don't
### merge.

### final_table_outfield ###

final_table_outfield$Nation <- str_remove(final_table_outfield$Nation, "^\\w+\\s")
final_table_outfield$Comp <- str_remove(final_table_outfield$Comp, "^\\w+\\s")

final_table_outfield <- final_table_outfield %>%
  group_by(Player, Age, Nation) %>%
  summarise(
    across(where(is.character), ~paste(., collapse = ", ")),
    across(where(is.numeric), sum)
  ) 

final_table_outfield <- final_table_outfield %>% 
  filter(MP <= 38)

final_table_outfield$Nation <- str_remove(final_table_outfield$Nation, "^[^,]+,\\s*")
final_table_outfield$Pos <- str_remove(final_table_outfield$Pos, ",.*$")
final_table_outfield$Age <- str_remove(final_table_outfield$Age, "^[^,]+,\\s*")
final_table_outfield$Age <- str_remove(final_table_outfield$Age, "-.*$")
final_table_outfield$Born <- str_remove(final_table_outfield$Born, "^[^,]+,\\s*")

final_table_outfield$Comp <- sapply(final_table_outfield$Comp, function(x) {
  parts <- strsplit(x, ", ")[[1]]
  if (length(parts) > 1 && parts[1] == parts[2]) {
    parts[1]
  } else {
    x
  }
})

final_table_outfield <- final_table_outfield %>% relocate(id, Player, Pos, Nation, Squad, Comp, Age)

final_table_outfield[is.na(final_table_outfield)] <- 0

final_table_outfield[ , 7:ncol(final_table_outfield)] <- lapply(
  final_table_outfield[ , 7:ncol(final_table_outfield)], 
  as.numeric
)

### final_table_gks ###

final_table_gks$Nation <- str_remove(final_table_gks$Nation, "^\\w+\\s")
final_table_gks$Comp <- str_remove(final_table_gks$Comp, "^\\w+\\s")

final_table_gks <- final_table_gks %>%
  group_by(Player, Age, Nation) %>%
  summarise(
    across(where(is.character), ~paste(., collapse = ", ")),
    across(where(is.numeric), sum)
  ) 

final_table_gks <- final_table_gks %>% 
  filter(MP <= 38)

final_table_gks$Nation <- str_remove(final_table_gks$Nation, "^[^,]+,\\s*")
final_table_gks$Pos <- str_remove(final_table_gks$Pos, ",.*$")
final_table_gks$Age <- str_remove(final_table_gks$Age, "^[^,]+,\\s*")
final_table_gks$Age <- str_remove(final_table_gks$Age, "-.*$")
final_table_gks$Born <- str_remove(final_table_gks$Born, "^[^,]+,\\s*")

final_table_gks$Comp <- sapply(final_table_gks$Comp, function(x) {
  parts <- strsplit(x, ", ")[[1]]
  if (length(parts) > 1 && parts[1] == parts[2]) {
    parts[1]
  } else {
    x
  }
})

final_table_gks <- final_table_gks %>% relocate(id, Player, Pos, Nation, Squad, Comp, Age)

final_table_gks[is.na(final_table_gks)] <- 0

final_table_gks[ , 7:ncol(final_table_gks)] <- lapply(
  final_table_gks[ , 7:ncol(final_table_gks)], 
  as.numeric
)

### unused_subs ###

unused_subs$Nation <- str_remove(unused_subs$Nation, "^\\w+\\s")
unused_subs$Comp <- str_remove(unused_subs$Comp, "^\\w+\\s")

unused_subs <- unused_subs %>%
  group_by(Player, Age, Nation) %>%
  summarise(
    across(where(is.character), ~paste(., collapse = ", ")),
    across(where(is.numeric), sum)
  ) 

unused_subs$Nation <- str_remove(unused_subs$Nation, "^[^,]+,\\s*")
unused_subs$Pos <- str_remove(unused_subs$Pos, ",.*$")
unused_subs$Age <- str_remove(unused_subs$Age, "^[^,]+,\\s*")
unused_subs$Age <- str_remove(unused_subs$Age, "-.*$")
unused_subs$Born <- str_remove(unused_subs$Born, "^[^,]+,\\s*")

unused_subs$Comp <- sapply(unused_subs$Comp, function(x) {
  parts <- strsplit(x, ", ")[[1]]
  if (length(parts) > 1 && parts[1] == parts[2]) {
    parts[1]
  } else {
    x
  }
})

unused_subs <- unused_subs %>% relocate(id, Player, Pos, Nation, Squad, Comp, Age)

unused_subs[is.na(unused_subs)] <- 0

unused_subs[ , 7:ncol(unused_subs)] <- lapply(
  unused_subs[ , 7:ncol(unused_subs)], 
  as.numeric
)

### unused_subs_gk ###

unused_subs_gk$Nation <- str_remove(unused_subs_gk$Nation, "^\\w+\\s")
unused_subs_gk$Comp <- str_remove(unused_subs_gk$Comp, "^\\w+\\s")

unused_subs_gk <- unused_subs_gk %>%
  group_by(Player, Age, Nation) %>%
  summarise(
    across(where(is.character), ~paste(., collapse = ", ")),
    across(where(is.numeric), sum)
  ) 

unused_subs_gk$Nation <- str_remove(unused_subs_gk$Nation, "^[^,]+,\\s*")
unused_subs_gk$Pos <- str_remove(unused_subs_gk$Pos, ",.*$")
unused_subs_gk$Age <- str_remove(unused_subs_gk$Age, "^[^,]+,\\s*")
unused_subs_gk$Age <- str_remove(unused_subs_gk$Age, "-.*$")
unused_subs_gk$Born <- str_remove(unused_subs_gk$Born, "^[^,]+,\\s*")

unused_subs_gk$Comp <- sapply(unused_subs_gk$Comp, function(x) {
  parts <- strsplit(x, ", ")[[1]]
  if (length(parts) > 1 && parts[1] == parts[2]) {
    parts[1]
  } else {
    x
  }
})

unused_subs_gk <- unused_subs_gk %>% relocate(id, Player, Pos, Nation, Squad, Comp, Age)

unused_subs_gk[is.na(unused_subs_gk)] <- 0

unused_subs_gk[ , 7:ncol(unused_subs_gk)] <- lapply(
  unused_subs_gk[ , 7:ncol(unused_subs_gk)], 
  as.numeric
)

# remove obsolete objects
rm(gk_tables, tables, urls)
