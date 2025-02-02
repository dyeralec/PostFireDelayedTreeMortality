#load required packages
require(RSQLite)
require(dplyr)
require(tidyr)
require(ggplot2)

#Turn off scientific notation for session (scientific numbers cause problems with long CN fields)
options(scipen = 999)

#allows seeing all columns in dataframe view
#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

#Define pathways to MI, WI, MN state SQLite FIADB databases
OR_FIADB = "..SQLite_FIADB_OR.db"


##Load in relevant tables from OR state FIADB
{
  #OR
  con <- dbConnect(SQLite() ,OR_FIADB)
  TREEINIT <- dbReadTable(con, 'FVS_TREEINIT_PLOT')
  STANDINIT <- dbReadTable(con, 'FVS_STANDINIT_PLOT')
  COND <- dbReadTable(con, 'COND')
  PLOT <- dbReadTable(con, 'PLOT')
  EVAL <- dbReadTable(con, 'POP_PLOT_STRATUM_ASSGN')
  
  #Disconnect from FIADB
  dbDisconnect(con)
  
}

##Filter tables
{
  #Rename some CN fields for easier/less confusing linking
  PLOT <- rename(PLOT, PLT_CN = CN)
  COND <- rename(COND, COND_CN = CN)
  TREEINIT <- rename(TREEINIT, PLT_CN = PLOT_CN)
  STANDINIT <- rename(STANDINIT, PLT_CN = STAND_CN)
  
  COND <- subset(COND, COND_STATUS_CD == 1 & CONDPROP_UNADJ == 1)
  
  EVAL <- subset(EVAL, EVALID == "411901")
  
  #Append COND_CN field to TREEDATA
  STANDINIT <- merge(x =STANDINIT, y = COND[ , c("PLT_CN","COND_CN")], by = c("PLT_CN"), all.x=TRUE)
  
  STANDINIT <- subset(STANDINIT, VARIANT == "WC" & !is.na(AGE) & !is.na(COND_CN))
  
  #Subset STANDINIT records to only those associated with desired EVALIDs
  STANDINIT <- filter(STANDINIT, PLT_CN %in% EVAL$PLT_CN)
  
  #Subset TREEINIT records to only those associated with reduced set of plots/conditions
  TREEINIT <- filter(TREEINIT, PLT_CN %in% STANDINIT$PLT_CN)
  
  #Drop trees with no recorded CRRATIO
  TREEINIT <- subset(TREEINIT, !is.na(CRRATIO) & !is.na(DIAMETER) & !is.na(HT))
  
  #Drop auxiliary species from treeinit
  TREEINIT <- subset(TREEINIT, (SPECIES == 202 | SPECIES == 263 | SPECIES == 11 | SPECIES == 242 | SPECIES == 264 | SPECIES == 22 | SPECIES == 15 | SPECIES == 17 | SPECIES == 108 | SPECIES == 93 | SPECIES == 19))
}

##Summarize dia, ht, and crratio by species at plot level by stand age and DIA class
{
  #Classify DIAMETER into diameter class
  TREEINIT <-  TREEINIT  %>%
    mutate(DIACLASS = case_when(
      DIAMETER >= 1 & DIAMETER < 10 ~ "1-10",
      DIAMETER >= 10 & DIAMETER < 20 ~ "10-20",
      DIAMETER >= 20  ~ ">20")) 
  
  #fix species ID
  TREEINIT <-  TREEINIT  %>%
    mutate(SPECIES = case_when(
      SPECIES == "17" ~ "15",
      TRUE ~ SPECIES)) 
  
  #Classify SPECIES into SPECIES_NAME
  TREEINIT <-  TREEINIT  %>%
    mutate(SPECIES_NAME = case_when(
      SPECIES == 202 ~ "Douglas-fir",
      SPECIES == 263 ~ "western hemlock",
      SPECIES == 11 ~ "Pacific silver fir",
      SPECIES == 242 ~ "western red cedar",
      SPECIES == 264 ~ "mountain hemlock",
      SPECIES == 22 ~ "noble fir",
      SPECIES == 15 ~ "white/grand fir",
      SPECIES == 108 ~ "lodgepole pine",
      SPECIES == 93 ~ "Engelmann spruce",
      SPECIES == 19 ~ "subalpine fir")) 
  
  TREESUMMARY <- TREEINIT %>% 
    group_by(PLT_CN, SPECIES_NAME) %>% 
    summarise(DIA = weighted.mean(DIAMETER,TREE_COUNT),HT = weighted.mean(HT,TREE_COUNT),CRRATIO = weighted.mean(CRRATIO,TREE_COUNT))
  
  TREESUMMARY <- merge(x = TREESUMMARY, y = STANDINIT[ , c("PLT_CN","AGE")], by = c("PLT_CN"), all.x=TRUE)
  
  TREESUMMARY <- subset(TREESUMMARY, AGE >= 10)
  
  #Classify AGE into AGECLASS
  TREESUMMARY <-  TREESUMMARY  %>%
    mutate(AGECLASS = case_when(
      AGE >= 10 & AGE < 60 ~ "Young",
      AGE >= 60 & AGE < 120 ~ "Mature",
      AGE >= 120 ~ "Supermature"))  
  
  TREESUMMARY <- TREESUMMARY %>% 
    group_by(SPECIES_NAME, AGECLASS) %>% 
    summarise(DIA = mean(DIA),HT = mean(HT),CRRATIO = mean(CRRATIO))
  
  write.csv(TREESUMMARY, "FIA_SPECIES_AGECLASS_SUMMARY.csv", row.names = FALSE) 
  
  TREESUMMARY <- TREEINIT %>% 
    group_by(DIACLASS, SPECIES_NAME) %>% 
    summarise(DIA = weighted.mean(DIAMETER,TREE_COUNT),HT = weighted.mean(HT,TREE_COUNT),CRRATIO = weighted.mean(CRRATIO,TREE_COUNT))
  
  write.csv(TREESUMMARY, "FIA_SPECIES_DIACLASS_SUMMARY.csv", row.names = FALSE) 
  
}
