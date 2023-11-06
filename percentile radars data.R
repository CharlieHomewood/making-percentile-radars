##### load packages #####

if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE); require(tidyverse)
}

if (!require(rvest)) {
  install.packages("rvest", dependencies = TRUE); require(rvest)
}

### players who played less than 450 mins (5 games) are excluded ###
# Whilst this threshold is arbitrary, it is generally used.
# This also alters our percentile calculations

final_table_outfield <- final_table_outfield %>% 
  filter(Min >= 450) %>% 
  as.data.frame()

final_table_gks <- final_table_gks %>% 
  filter(Min >= 450) %>% 
  as.data.frame()

### need columns to be converted to per 90 ###

final_table_outfield_per90 <- final_table_outfield %>% 
  select(-contains("90"), -id, -Born, -MP, -Starts) %>% 
  mutate(Min_calc = Min) %>% 
  relocate(Min_calc) %>% 
  mutate(Min = as.character(Min)) %>% 
  mutate(across(where(is.numeric) & !contains(c("%", "90")), ~(. / Min_calc) * 90)) %>% 
  select(-Min_calc) %>% 
  mutate(Min = as.numeric(Min))

final_table_gks_per90 <- final_table_gks %>% 
  select(-contains("90"), -id, -Born, -MP, -Starts) %>% 
  mutate(Min_calc = Min) %>% 
  relocate(Min_calc) %>% 
  mutate(Min = as.character(Min)) %>% 
  mutate(across(where(is.numeric) & !contains(c("%", "90")), ~(. / Min_calc) * 90)) %>% 
  select(-Min_calc) %>% 
  mutate(Min = as.numeric(Min))

final_table_gks_per90 <- cbind(
  final_table_gks_per90, 
  select(final_table_gks, "PSxG_mins_GoalsAgainst_per_90")
  )

# It's better to compare percentiles against players playing in the same position.
# e.g. a defender scoring 5 goals is not impressive compared to forwards, but it
# is more impressive compared to just defenders.
# As such, we're going to filter rows by position and then create separate table
# for each position

table_DF <- final_table_outfield_per90 %>% 
  filter(grepl("DF", Pos)) 

table_DF <- table_DF %>%
  rename_with(~paste0(., "_per90")) %>% 
  rename_at(
    .vars = vars(1:7),
    .funs = funs(gsub("_per90", "", .))
    )

table_MF <- final_table_outfield_per90 %>% 
  filter(grepl("MF", Pos))

table_MF <- table_MF %>%
  rename_with(~paste0(., "_per90")) %>% 
  rename_at(
    .vars = vars(1:7),
    .funs = funs(gsub("_per90", "", .))
    )

table_FW <- final_table_outfield_per90 %>% 
  filter(grepl("FW", Pos))

table_FW <- table_FW %>%
  rename_with(~paste0(., "_per90")) %>% 
  rename_at(
    .vars = vars(1:7),
    .funs = funs(gsub("_per90", "", .))
    )

### percentiles ###

percentiles_DF <- data.frame(
  matrix(
    NA, 
    nrow = nrow(
      table_DF %>% 
        filter(grepl("DF", Pos))
      )
    )
  )

percentiles_MF <- data.frame(
  matrix(
    NA, 
    nrow = nrow(
      table_MF %>% 
        filter(grepl("MF", Pos))
    )
  )
)

percentiles_FW <- data.frame(
  matrix(
    NA, 
    nrow = nrow(
      table_FW %>% 
        filter(grepl("FW", Pos))
    )
  )
)

percentiles_gks <- data.frame(
  matrix(
    NA, 
    nrow = nrow(
      final_table_gks_per90
    )
  )
)

### table_DF ###

for (col in colnames(table_DF)) {
  
  if (is.numeric(table_DF[[col]])) {

    x <- ecdf(table_DF[[col]])(table_DF[[col]])
    
    x <- round(x * 100, 0)
    
    percentiles_DF[[col]] <- x
    
    rm(col, x)
    
  }
  
}

percentiles_DF <- percentiles_DF[ ,-1]

percentiles_DF <- percentiles_DF %>% 
  rename_with(~paste0(., "_percentile"))

final_table_DF <- cbind(table_DF, percentiles_DF)

final_table_DF[, 6:ncol(final_table_DF)] <- lapply(final_table_DF[, 6:ncol(final_table_DF)], as.numeric)

### table_MF ###

for (col in colnames(table_MF)) {
  
  if (is.numeric(table_MF[[col]])) {
    
    x <- ecdf(table_MF[[col]])(table_MF[[col]])
    
    x <- round(x * 100, 0)
    
    percentiles_MF[[col]] <- x
    
    rm(col, x)
    
  }
  
}

percentiles_MF <- percentiles_MF[ ,-1]

percentiles_MF <- percentiles_MF %>% 
  rename_with(~paste0(., "_percentile"))

final_table_MF <- cbind(table_MF, percentiles_MF)

final_table_MF[, 6:ncol(final_table_MF)] <- lapply(final_table_MF[, 6:ncol(final_table_MF)], as.numeric)

### table_FW ###

for (col in colnames(table_FW)) {
  
  if (is.numeric(table_FW[[col]])) {
    
    x <- percent_rank(rank(table_FW[[col]]) / length(table_FW[[col]]))
    
    x <- round(x * 100, 0)
    
    percentiles_FW[[col]] <- x
    
    rm(col, x)
    
  }
  
}

percentiles_FW <- percentiles_FW[ ,-1]

percentiles_FW <- percentiles_FW %>% 
  rename_with(~paste0(., "_percentile"))

final_table_FW <- cbind(table_FW, percentiles_FW)

final_table_FW[, 6:ncol(final_table_FW)] <- lapply(final_table_FW[, 6:ncol(final_table_FW)], as.numeric)

### create percentiles for final_table_gks ###

for (col in colnames(final_table_gks_per90)) {
  
  if (is.numeric(final_table_gks_per90[[col]])) {
    
    x <- ecdf(final_table_gks_per90[[col]])(final_table_gks_per90[[col]])
    
    x <- round(x * 100, 0)
    
    percentiles_gks[[col]] <- x
    
    rm(col, x)
    
  }
  
}

percentiles_gks <- percentiles_gks[ ,-1]

percentiles_gks <- percentiles_gks %>% 
  rename_with(~paste0(., "_percentile"))

final_table_GK <- cbind(final_table_gks_per90, percentiles_gks)

final_table_GK[, 6:ncol(final_table_GK)] <- lapply(final_table_GK[, 6:ncol(final_table_GK)], as.numeric)

##### NOTE #####
###
### percentile data is available on fbref, but I didn't want to scrape it directly
### as I'm unsure how feasible that would be given the layout of that data.
### (i.e. each player has a separate webpage containing their data and I would have to
### search for every player - men and women, big5 leagues and beyond - and then filter
### down. I'm not sure if I would be able to/allowed to make that many http requests.)
###
### As such, the data calculated here is different from that on fbref. In addition,
### fbref creates their percentiles by using opta data, who break down player position
### more granularly than fbref displays and so this also impacts the percentile scores.
###
### I also want to note that, as someone who follows football closely, I disagree with
### some of the positions assigned to certain players in fbref's data (e.g. Falcao is not 
### a midfielder, Lallana is not a forward etc. etc.). I also think this does impact the 
### utility of this kind of analysis, as players who ought to be compared against their 
### fellow positional-peers are instead categorised in the wrong place.
### 
### To address this, I could edit the data, but this might create issues with how I cite the
### source of the data. I'm not sure how to address this, so for now the player positions remain
### unchanged.

