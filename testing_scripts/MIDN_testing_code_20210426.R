library(forestMIDN)
library(tidyverse)

#----- Testing the import/export functions
#importData(instance = 'local', server = "localhost", new_env = T) # release 1.0.22 4/22
importData(instance = 'server', server = "INP2300VTSQL16\\IRMADEV1", name = "MIDN_Forest_Migration", new_env = T) #
path = "C:/Forest_Health/exports/MIDN"
#exportCSV(path, zip = TRUE)

importCSV(path = path, zip_name = "MIDN_Forest_20210503.zip")# release 1.0.22 4/26

# Function arguments
park = 'all'
from = 2007
to = 2019
QAQC = TRUE
locType = "all"
eventType = 'all'
panels = 1:4

arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                locType = locType, eventType = eventType)

compev_arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                       locType = locType)

# Checking data function
check_data <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", col1, col2)]}
  )) %>% bind_rows()
}

# import old database for comparisons
forestMIDNarch::importData(type = 'file',
  path='D:/NETN/Monitoring_Projects/Forest_Health/Database/MIDN/2021_Forest_Database/MIDN_FVM_BE_MASTER_20210429_Migration.accdb')

#forestMIDNarch::importData()

names(VIEWS_MIDN)
names(VIEWS_MIDN$MIDN_QuadSpecies)

#----- Testing joinLocEvent and migration -----
plotevs_old <- do.call(forestMIDNarch::joinLocEvent, c(arglist, output = 'verbose')) %>% mutate(Year = as.numeric(Year))
plotevs_new <- do.call(joinLocEvent, arglist)
names(plotevs_old)
names(plotevs_new)
nrow(plotevs_old) #1182
nrow(plotevs_new) #1182

pe_merge <- full_join(plotevs_new, plotevs_old, by = c("EventLegacyID" = "Event_ID", "Plot_Name" = "Plot_Name"))

check_data(pe_merge, "ParkSubUnit", "Unit_ID")
check_data(pe_merge,"xCoordinate", "X_Coord")
check_data(pe_merge,"yCoordinate", "Y_Coord")
#check_data(pe_merge,"StartDate", "Start_Date") # they're diff. format, so all show as different
check_data(pe_merge,"PanelCode", "Panel")
check_data(pe_merge,"Event_QAQC", "IsQAQC")
#test <- check_data(pe_merge,"ZoneCode", "UTM_Zone") # diff b/c one has N.
table(pe_merge$ZoneCode, pe_merge$UTM_Zone)
check_data(pe_merge,"Orientation.x", "Orientation.y")
#check_data(pe_merge,"cycle.x", "cycle.y")
check_data(pe_merge,"PlotTypeCode", "Loc_Type")
check_data(pe_merge,"PlotLegacyID", "Location_ID")
check_data(pe_merge,"Aspect.x", "Aspect.y")
check_data(pe_merge,"PhysiographyCode", "Physiographic_Class")
plot_check <- unique(pe_merge[, c("ParkUnit", "Plot_Name")])

table(plot_check$ParkUnit) # That's the correct # of plots/park

#APCO ASIS BOWA COLO FRSP GETT GEWA HOFU PETE RICH SAHI THST VAFO
#28    6    8   48  104   33    8   16   52   32    4    8   28
dir_dif <- check_data(pe_merge,"Directions.x", "Directions.y")
# No issues remaining

#----- Stand Views -----
stand_new <- do.call(joinStandData, arglist)
stand_old <- do.call(forestMIDNarch::joinStandData, arglist)
stand_old2 <- merge(stand_old, stand[,c("Event_ID", "Deer_Browse_Line_pre09_ID")],
                    by = "Event_ID", all.x = T, all.y = T)
stand_old2$Year <- as.numeric(stand_old2$Year)

names(stand_new)
names(stand_old)
stand_merge <- full_join(stand_new, stand_old2, by = c("Plot_Name" = "Plot_Name", "IsQAQC" = "Event_QAQC",
                                                      "StartYear" = "Year"))
names(stand_merge)

check_data(stand_merge, "Stand_Structure.x", "Stand_Structure.y") # Lots of NC
check_data(stand_merge, "Stand_Structure_Code", "Stand_Structure_ID") # Lots of NC
check_data(stand_merge, "Pct_Crown_Closure.x", "Pct_Crown_Closure.y") # Lots of NC
check_data(stand_merge, "Pct_Understory_Low.x", "Pct_Understory_Low.y") #0
check_data(stand_merge, "Pct_Understory_Mid.x", "Pct_Understory_Mid.y")#0
check_data(stand_merge, "Pct_Understory_High.x", "Pct_Understory_High.y")#0
check_data(stand_merge, "Pct_Bare_Soil", "Pct_Bare_Soil_Cover")#0
check_data(stand_merge, "Pct_Rock", "Pct_Rock_Cover")#0
check_data(stand_merge, "Pct_Lichen", "Pct_Lichen_Cover")#0
check_data(stand_merge, "Pct_Bryophyte", "Pct_Bryophyte_Cover")#0
check_data(stand_merge, "Pct_Water", "Pct_Surface_Water_Cover")#0
check_data(stand_merge, "Pct_Bare_Soil", "Pct_Bare_Soil_Cover")#0
check_data(stand_merge, "Pct_Trampled", "Pct_Trampled_Cover")#0
check_data(stand_merge, "Microtopography", "Microtopography_ID") # 20 NC
check_data(stand_merge, "Deer_Browse_Index", "Deer_Browse_Line_ID") # All are 5s that are recorded pre 2010 as DBL pres
table(stand_merge$Deer_Browse_Label, stand_merge$StartYear, useNA = 'always') # NCs and PMs are accurate
table(stand_merge$Deer_Browse_Label, stand_merge$Deer_Browse_Line_pre09_ID) # NCs correc

stand_merge %>% mutate(slp_dif = abs(PlotSlope - Plot_Slope_Deg)) %>%
    select(Plot_Name, StartYear, IsQAQC, EventID, PlotSlope, Plot_Slope_Deg, slp_dif) %>%
    filter(slp_dif > 0.5) # Only difference is QAQC event, which we don't care about

#+++++ No remaining stand height issues +++++

#----- Stand Disturbance -----
stdist_new <- do.call(joinStandDisturbance, arglist)

#--- Tends to crash if run after old DB import, so moved code to beginning
# library(RODBC)
# db <- odbcConnect("MIDNFVM") #
# disturb<-sqlFetch(db,"tbl_Disturbances")
# disttlu<-sqlFetch(db,"tlu_Disturbance_Codes")
# disttlutc<-sqlFetch(db,"tlu_Disturbance_Threshhold_Codes")
# odbcClose(db)

st_dist_o <- merge(plotevs_old[, c("Plot_Name", "Event_ID", "Event_QAQC", "Year")],
                   disturb[, c(2:6)], by = "Event_ID", all.x = T, all.y = T)

dist_merge <- merge(stdist_new, st_dist_o,
                    by.x = c("Plot_Name", "StartYear", "IsQAQC"),#, "Disturbance.Code"),
                    by.y = c("Plot_Name", "Year", "Event_QAQC"),#, "Disturbance_Code"),
                    all.x = T, all.y = T)
dist_merge[which(!complete.cases(dist_merge$DisturbanceCode)),] # None

check_data(dist_merge, "Disturbance_Threshold", "ThresholdCode") %>% arrange(Plot_Name) # PMs from early or dups
check_data(dist_merge, "Disturbance_Notes", "DisturbanceNote") # dups
check_data(dist_merge, "DisturbanceCode", "Disturbance_Code") %>% arrange(Plot_Name, StartYear) # All dups that sorted wrong or NAs to 0s. OK

#----- Tree Height -----
#ht_dif <-
stand_merge %>% mutate(cod_dif = abs(Avg_Height_Codom - Avg_Codom_HT),
                       int_dif = abs(Avg_Height_Inter - Avg_Inter_HT)) %>%
  select(Plot_Name, StartYear, IsQAQC, Avg_Height_Codom, Avg_Codom_HT,
         cod_dif, Avg_Height_Inter, Avg_Inter_HT, int_dif ) %>%
  filter(cod_dif > 0.5 | int_dif > 0.5)
#0 Tree height issues resolved

tree_ht_old <- stand %>% select(Event_ID, Stand_Structure_ID, Tree_1_Number_Codom:Height_Tree_3_Inter) %>%
  filter(Stand_Structure_ID != 5)

tree_ht2 <- merge(plotevs_old[,c("Event_ID", "Plot_Name", "Year", "Event_QAQC")],
                  tree_ht_old, by = "Event_ID", all.x = FALSE, all.y = TRUE) %>% filter(!is.na(Plot_Name))

tree_ht_w1 <- tree_ht2 %>% select(Event_ID:Tree_3_Number_Inter) %>%
  pivot_longer(cols = c(Tree_1_Number_Codom:Tree_3_Number_Inter),
               names_to = "Samp",
               values_to = "Tree_Number") %>%
  mutate(Samp_Num = case_when(str_detect(Samp, "_1_") ~ 1L,
                              str_detect(Samp, "_2_") ~ 2L,
                              str_detect(Samp, "_3_") ~ 3L),
         Crown = ifelse(str_detect(Samp, "Codom"), "Codom", "Inter")
  )

tree_ht_w2 <- tree_ht2 %>% select(Event_ID:Event_QAQC, Height_Tree_1_Codom:Height_Tree_3_Inter) %>%
  pivot_longer(cols = c(Height_Tree_1_Codom:Height_Tree_3_Inter),
               names_to = "Samp_ht",
               values_to = "Height_m") %>%
  mutate(Samp_Num = case_when(str_detect(Samp_ht, "_1_") ~ 1L,
                              str_detect(Samp_ht, "_2_") ~ 2L,
                              str_detect(Samp_ht, "_3_") ~ 3L),
         Crown = ifelse(str_detect(Samp_ht, "Codom"), "Codom", "Inter")
  )

tree_ht3 <- merge(tree_ht_w1, tree_ht_w2, by = c("Event_ID", "Plot_Name", "Year", "Event_QAQC", "Samp_Num", "Crown"),
                  all.x = T, all.y = T) %>% select(-Samp, -Samp_ht) %>% filter(!is.na(Height_m))

head(tree_ht3)

tree_ht_old_sum <- tree_ht3 %>% group_by(Plot_Name, Year, Event_QAQC, Crown) %>%
  summarize(total_ht_old = sum(Height_m, na.rm =T),
            num_trees_old = sum(!is.na(Height_m)),
            .groups = 'drop')

tree_ht_vw <- get("COMN_StandTreeHeights", envir = VIEWS_MIDN)#[, -c(18:24, 27)]
tree_ht_vw$Plot_Name <- paste(tree_ht_vw$ParkUnit, sprintf("%03d", tree_ht_vw$PlotCode), sep = "-")
tree_ht_vw$Event_QAQC <- ifelse(tree_ht_vw$IsQAQC == 0, FALSE, TRUE)

tree_ht_vw_sum <- tree_ht_vw %>% group_by(Plot_Name, StartYear, Event_QAQC, EventID, CrownClassLabel) %>%
  summarize(total_ht_new = sum(Height),
            num_trees_new = sum(!is.na(Height)),
            .groups = 'drop') %>%
  mutate(Crown = ifelse(CrownClassLabel == "Intermediate", "Inter", "Codom"))

tree_height_comps <- merge(tree_ht_vw_sum,
                           tree_ht_old_sum,
                           by.x = c("Plot_Name", "StartYear", "Event_QAQC", "Crown"),
                           by.y = c("Plot_Name", "Year", "Event_QAQC", "Crown"),
                           all.x = T, all.y = T) %>%
  mutate(ht_diff = abs(total_ht_new - total_ht_old),
         n_tree_diff = num_trees_new - num_trees_old)

missing_tree_hts <- tree_height_comps %>% filter(n_tree_diff == -1)
missing_tree_hts # 0 Problem resolved
#+++++ No remaining issues +++++

#----- CWD -----
cwd_old <- do.call(forestMIDNarch::joinCWDData, arglist) %>% mutate(ScientificName = ifelse(Latin_Name == "No species recorded",
                                                                                            "None present", Latin_Name))
cwd_new <- do.call(joinCWDData, arglist)

cwd_merge <- merge(cwd_new, cwd_old, by.x = c("Plot_Name", "StartYear", "IsQAQC", "ScientificName", "DecayClassCode"),
                   by.y = c("Plot_Name", "Year", "Event_QAQC", "ScientificName", "Decay_Class_ID"),
                   all = TRUE)

#cwd_dif <-
  cwd_merge %>% mutate(cwd_dif = abs(CWD_Vol.x - CWD_Vol.y)) %>% filter(cwd_dif > 0.01 & IsQAQC == FALSE) %>%
  select(Plot_Name:ScientificName, DecayClassCode, CWD_Vol.x, CWD_Vol.y, cwd_dif)
# 20 small differences due to rounding

#----- Tree Data
tree_old <- do.call(forestMIDNarch::joinTreeData, c(arglist, list(speciesType = 'all', status = 'all'))) %>%
  mutate(TagCode = as.numeric(Tree_Number_MIDN))
tree_new <- do.call(joinTreeData, c(arglist, list(speciesType = 'all', status = 'all')))

table(tree_new$HWACode, tree_new$ScientificName, useNA = 'always') # Only values for TSUCAN. Good!
non_tsucan <- tree_new %>% filter(ScientificName != "Tsuga canadensis")
table(non_tsucan$HWACode, non_tsucan$StartYear, useNA = 'always') # All NA. Good!

table(tree_new$BBDCode, tree_new$ScientificName, useNA = 'always') # Only values for FAGGRA. Good!
non_faggra <- tree_new %>% filter(ScientificName != "Fagus grandifolia")
table(non_faggra$BBDCode, non_faggra$StartYear, useNA = 'always')# All NA. Good!

tree_merge <- merge(tree_new, tree_old, by.x = c("Plot_Name", "StartYear", "IsQAQC", "TagCode"),
                    by.y = c("Plot_Name", "Year", "Event_QAQC", "TagCode"))

check_trees <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "TagCode", "IsQAQC", "ScientificName",
              "Status_ID", "TreeStatusCode",col1, col2)]}
  )) %>% bind_rows()
}

check_trees(tree_merge, "TSN.x", "TSN.y") #none
check_trees(tree_merge, "Azimuth.x", "Azimuth.y") #none
check_trees(tree_merge, "Fork.x", "Fork.y") #none

tree_merge %>% mutate(dist_diff = abs(Distance.x - Distance.y)) %>%
  filter(dist_diff > 0.1)  #None

tree_merge %>% mutate(dbh_diff = abs(DBH - DBHcm)) %>%
  filter(dbh_diff > 0) %>%
  select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName, DBHcm, DBH, dbh_diff)
# 9 differences are all due to rounding issues. OK

check_trees(tree_merge, "TreeStatusCode", "Status_ID")# none
crown_check <- check_trees(tree_merge, "Crown_Class_ID", "CrownClassCode") #0
table(crown_check$Status_ID, crown_check$Crown_Class_ID, useNA = 'always')
# Differences are because dead and EX trees were correctly scrubbed of their crown class.

tree_simp <- tree_merge %>% select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName,
                                   TreeStatusCode, Status_ID, CrownClassCode, Crown_Class_ID)
table(tree_simp$CrownClassCode, tree_simp$Crown_Class_ID, useNA = 'always')
#12 rogue: 1 NA/2; 5 NA/3; 6NA/5 All Dead trees that shouldn't have a crown class. Good.

check_trees(tree_merge, "Decay_Class_ID", "DecayClassCode")
# No issues. Decays that are diff are AS, which shouldn't have a decay class

check_trees(tree_merge, "IsDBHVerified", "DBH_Verified")
table(tree_merge$IsDBHVerified, tree_merge$DBH_Verified, useNA = 'always')

#check_trees(tree_merge, "Pct_Tot_Foliage_Cond", "Total_Foliage_Condition")
table(tree_merge$Pct_Tot_Foliage_Cond, tree_merge$Total_Foliage_Condition, tree_merge$StartYear, useNA = 'always')
table(tree_merge$Pct_Tot_Foliage_Cond, useNA = 'always')
# 4/27: No trees with 0

# Check for Foliage still 0:
tree_merge %>% filter(Pct_Tot_Foliage_Cond == 0) %>% arrange(Plot_Name, StartYear) %>%
  select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode, CrownClassCode, Pct_Tot_Foliage_Cond)
  # 0 records
# +++++++ Tree data finished checking. No remaining tree-level issues

#----- Tree Foliage Conditions -----
fol_vw <- VIEWS_MIDN$COMN_TreesFoliageCond

live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
dead <- c("2", "DB", "DL", "DM", "DS")
fol_vw$status_simp <- ifelse(fol_vw$TreeStatusCode %in% live, "live",
                             ifelse(fol_vw$TreeStatusCode %in% dead, "dead",
                                    "inactive"))

table(fol_vw$status_simp, fol_vw$FoliageConditionCode, useNA = "always")

table(fol_vw$TotalFoliageConditionCode, fol_vw$StartYear, useNA = 'always') # 2007 is NC correctly
table(fol_vw$TotalFoliageConditionLabel, fol_vw$StartYear, useNA = 'always')

table(fol_vw$PercentLeafAreaLabel, fol_vw$StartYear) # All 2007-2015 NC, correct.
table(fol_vw$PercentLeafAreaLabel, fol_vw$FoliageConditionCode) # NA correctly applied to L, NO, S, W
# Also no 0% left. Those changed to PM correctly

table(fol_vw$TotalFoliageConditionLabel, fol_vw$FoliageConditionCode, useNA = 'always') # NO/NotApp correct
table(fol_vw$PercentLeavesLabel, fol_vw$FoliageConditionCode, useNA = 'always')
# The 2 H and 2 N that were incorrectly NA are now PM
# No more 0%s- all Not applicable instead. Good.

pm_fol <- fol_vw %>% filter(TotalFoliageConditionCode == "PM")
pm_fol$Plot_Name <- paste(pm_fol$ParkUnit, sprintf("%03d", pm_fol$PlotCode), sep = "-")
#write.csv(pm_fol, "./testing_scripts/PM_totfoliage.csv")
# 19 PMs in total foliage, but couldn't fix either because missing foliage condition %s or multiple records

#no_folcond_with_totfol_0p <-
fol_vw %>% filter(FoliageConditionCode == "NO" & TotalFoliageConditionCode %in% c(1:4)) %>%
  select(ParkUnit, PlotCode, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode,
         FoliageConditionCode, TotalFoliageConditionCode)
# 1 record: THST-132-2008 Tag 13374 has TotalFoliageConditionCode > 0 and FoliageConditionCodes missing which
# incorrectly migrated as NO. If Stephen doesn't fix this in the next migration, I'll fix this after the final mig.
# 5/3 fixed

table(fol_vw$PercentLeavesLabel, fol_vw$FoliageConditionCode, useNA = 'always')# No 0s!
fol_vw %>% filter(PercentLeavesLabel == "0%" & FoliageConditionCode %in% c("L", "N")) %>% arrange(ParkUnit, PlotCode, StartYear, TagCode) %>%
  select(ParkUnit, PlotCode, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode,
         PercentLeavesLabel, FoliageConditionCode, TotalFoliageConditionCode) #0

fol_vw %>% filter(TotalFoliageConditionLabel == "0%") %>%
  select(ParkUnit, PlotCode, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode,
         FoliageConditionCode, TotalFoliageConditionLabel) %>%
  arrange(ParkUnit, PlotCode, StartYear, TagCode)#0

compev_arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                       locType = locType)

fol_new <- do.call(joinTreeFoliageCond, compev_arglist)
fol_old1 <- xrfolcond %>% mutate(Cond = case_when(Foliage_Condition_ID == 1 ~ "C",
                                                  Foliage_Condition_ID == 2 ~ "N",
                                                  Foliage_Condition_ID == 3 ~ "H",
                                                  Foliage_Condition_ID == 4 ~ "S",
                                                  Foliage_Condition_ID == 5 ~ "W",
                                                  Foliage_Condition_ID == 6 ~ "L",
                                                  Foliage_Condition_ID == 7 ~ "O",
                                                  Foliage_Condition_ID == 8 ~ "NO",
                                                  TRUE ~ NA_character_),
                                 Pct_Leaves_Aff = case_when(Foliage_Condition_Percent == 0 ~ 0,
                                                            Foliage_Condition_Percent == 1 ~ 5.5,
                                                            Foliage_Condition_Percent == 2 ~ 30,
                                                            Foliage_Condition_Percent == 3 ~ 70,
                                                            Foliage_Condition_Percent == 4 ~ 95,
                                                            TRUE ~ NA_real_),
                                 Pct_Leaf_Area = case_when(Leaf_Area_Percent == 0 ~ 0,
                                                           Leaf_Area_Percent == 1 ~ 5.5,
                                                           Leaf_Area_Percent == 2 ~ 30,
                                                           Leaf_Area_Percent == 3 ~ 70,
                                                           Leaf_Area_Percent == 4 ~ 95,
                                                           TRUE ~ NA_real_)
)

tree_evs_old <- left_join(plotevs_old, trees %>% select(Tree_ID:Tree_Notes),
                          by = intersect(names(plotevs_old), names(trees %>% select(Tree_ID:Tree_Notes)))) %>%
  left_join(., treedata %>% select(Tree_Data_ID:Notes),
            by = intersect(names(.), names(treedata %>% select(Tree_Data_ID:Notes)))) %>%
  select(Plot_Name, Year, Event_QAQC, Event_ID, Tree_ID, Status_ID, Tree_Number_MIDN, Total_Foliage_Condition,
         Tree_Data_ID, Status_ID) %>% filter(Status_ID %in% c("1", "AB", "AF", "AL", "AM", "AS",
                                                              "RB", "RF", "RL", "RS"))

fol_old <- left_join(tree_evs_old, fol_old1, by = "Tree_Data_ID") %>%
  select(Plot_Name, Year, Event_QAQC, Tree_Number_MIDN, Cond, Status_ID, Pct_Leaves_Aff, Pct_Leaf_Area) %>%
  arrange(Plot_Name, Year, Event_QAQC, Tree_Number_MIDN) %>%
  pivot_wider(names_from = Cond,
              values_from = c(Pct_Leaves_Aff, Pct_Leaf_Area),
              values_fill = NA_real_) %>%
  mutate(TagCode = as.numeric(Tree_Number_MIDN)) %>%
  select(-Pct_Leaves_Aff_NA, -Pct_Leaf_Area_NA, -Pct_Leaf_Area_W, -Pct_Leaf_Area_O, -Pct_Leaf_Area_S)

nrow(fol_old) #23401
nrow(fol_new) #23401

fol_merge <- full_join(fol_new, fol_old,
                       by = c("Plot_Name" = "Plot_Name", "StartYear" = "Year", "IsQAQC" = "Event_QAQC", "TagCode" = "TagCode"),
                       suffix = c("_new", "_old"))


check_conds <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "TagCode", "Status_ID", "IsQAQC", col1, col2)]}
  )) %>% bind_rows()
}

names(fol_merge)
# 0s in _new because they were filled in function
lvs_C <- check_conds(fol_merge, "Pct_Leaves_Aff_C_new", "Pct_Leaves_Aff_C_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_H <- check_conds(fol_merge, "Pct_Leaves_Aff_H_new", "Pct_Leaves_Aff_H_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_L <- check_conds(fol_merge, "Pct_Leaves_Aff_L_new", "Pct_Leaves_Aff_L_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_N <- check_conds(fol_merge, "Pct_Leaves_Aff_N_new", "Pct_Leaves_Aff_N_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_S <- check_conds(fol_merge, "Pct_Leaves_Aff_S_new", "Pct_Leaves_Aff_S_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_W <- check_conds(fol_merge, "Pct_Leaves_Aff_W_new", "Pct_Leaves_Aff_W_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_O <- check_conds(fol_merge, "Pct_Leaves_Aff_O_new", "Pct_Leaves_Aff_O_old") # all diffs are NAs converted to 0s. I think they should still be NA.

la_C <- check_conds(fol_merge, "Pct_Leaf_Area_C_new", "Pct_Leaf_Area_C_old") # all diffs are NAs converted to 0s. I think they should still be NA.
la_H <- check_conds(fol_merge, "Pct_Leaf_Area_H_new", "Pct_Leaf_Area_H_old") # all diffs are NAs converted to 0s. I think they should still be NA.
la_N <- check_conds(fol_merge, "Pct_Leaf_Area_N_new", "Pct_Leaf_Area_N_old") # all diffs are NAs converted to 0s. I think they should still be NA.

# +++++ Only remaining issue: 1 Foliage condition being converted to NO that should be PM.
                             # THST-132-2008 Tag 13374
# 5/3 THST-132 issue resolved.

#----- Tree Conditions
# MIDN odl db condition counts
#    H	AD	BBD	CAVL CAVS	 CW	   DBT	EAB	EB	   G	HWA	ID	NO	  OTH	SPB	VIN	  VOB
#13828	968	29	297	 279	2979	2271	9	  3123	172	3	  58	1257	21	8	  1453	189
table(VIEWS_MIDN$COMN_TreesConditions$TreeConditionCode)
# 23418 PM
#    H AD   BBD  CAVL  CAVS    CW   DBT   EAB    EB     G     HWA  ID    NO   OTH   SPB  VINE
#13801 970    29   297   279  2971  2257     9  3119   171    3    58  1257    21     8  1636
table(VIEWS_MIDN$COMN_TreesConditions$TreeConditionCode, VIEWS_MIDN$COMN_TreesConditions$StartYear, useNA = 'always')
con_vw_simp <- con_vw %>% select(ParkUnit, PlotCode, StartYear, IsQAQC, TagCode, TreeConditionCode) %>%
               mutate(pres = 1) %>% filter(TreeConditionCode != "VINE")

con_dup <- con_vw_simp[which(duplicated(con_vw_simp)),]
con_vw_simp <- con_vw_simp[-45773,] # dup causing issues and not important to understand problem

con_wide <- con_vw_simp %>% pivot_wider(names_from = TreeConditionCode,
                                         values_from = pres) %>%
  filter(StartYear > 2007)

table(complete.cases(con_wide$PM)) # 23418 PMs. Not sure why 502 are not PM
# Don't have time to troubleshoot why this is happening.

con_vw <- VIEWS_MIDN$COMN_TreesConditions
live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
dead <- c("2", "DB", "DL", "DM", "DS")
con_vw$status_simp <- ifelse(con_vw$TreeStatusCode %in% live, "live",
                             ifelse(con_vw$TreeStatusCode %in% dead, "dead",
                                    "inactive"))
table(con_vw$status_simp, con_vw$TreeConditionCode, con_vw$StartYear, useNA = "always")
# Still missing NC for dead in 2011, and now have too many PMs for live

dead_trees_with_conds <- con_vw %>%
  filter(TreeStatusCode %in% c("DB", "DL", "DM", "DS")) #%>%
  #filter(!TreeConditionCode %in% c("NO", "CAVS", "CAVL")) #%>%
  #filter(!is.na(TreeConditionCode)) #0

table(dead_trees_with_conds$TreeConditionCode, dead_trees_with_conds$StartYear, useNA = 'always')
# Still need NC for NAs <=2011 and PM for >2011 (1 record in 2012)

cond_new <- do.call(joinTreeConditions, c(compev_arglist, list(status = 'active')))

active <- c("1", "AB" ,"AF", "AL", "AS", "AM", "DB", "DL", "DM", "DS", "RB", "RF", "RL", "RS")

tree_evs_old <- left_join(plotevs_old, trees %>% select(Tree_ID:Tree_Notes),
                          by = intersect(names(plotevs_old), names(trees %>% select(Tree_ID:Tree_Notes)))) %>%
  left_join(., treedata %>% select(Tree_Data_ID:Notes),
            by = intersect(names(.), names(treedata %>% select(Tree_Data_ID:Notes)))) %>%
  select(Plot_Name, Year, Event_QAQC, Event_ID, Tree_ID, Tree_Number_MIDN, Total_Foliage_Condition,
         Tree_Data_ID, Status_ID) %>% filter(Status_ID %in% active)

table(tree_evs_old$Status_ID)

tlucond <- read.csv("D:/NETN/R_Dev/forestNETN/testing_scripts/tlu_Tree_Conditions.csv")
spb <- data.frame(Tree_Condition_ID = 28, Tree_Condition_ORDER = 28, Code = "SPB", Description = "SPB", Type = "Alive")
tlucond <- rbind(tlucond, spb)

cond_old1 <- left_join(xrtreecond %>% select(Tree_Data_ID, Tree_Condition_ID),
                       tlucond %>% select(Tree_Condition_ID, Code), by = c("Tree_Condition_ID")) %>%
  right_join(tree_evs_old, ., by = intersect(names(tree_evs_old), names(.))) %>%
  select(Plot_Name, Year, Event_QAQC, Tree_Number_MIDN, Status_ID, Code) %>%
  filter(!is.na(Code)) %>% filter(!is.na(Plot_Name)) %>%
  arrange(Plot_Name, Year, Tree_Number_MIDN) %>% # drop trees without conditions
  mutate(pres = 1, TagCode = as.numeric(Tree_Number_MIDN)) %>% select(-Tree_Number_MIDN)

cond_old <- cond_old1 %>% mutate(pres = 1) %>% unique() %>%
  pivot_wider(names_from = Code, values_from = pres, values_fill = 0)

names(cond_old)
names(cond_new)
cond_merge <- full_join(cond_new, cond_old, by = c("Plot_Name" = "Plot_Name",
                                                   "StartYear" = "Year",
                                                   "IsQAQC" = "Event_QAQC",
                                                   "TagCode" = "TagCode"),
                        suffix = c("_new", "_old"))


check_conds <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "TagCode", "TreeStatusCode", "IsQAQC", col1, col2)]}
  )) %>% bind_rows()
}

names(cond_merge)
cond_merge[,18:63][is.na(cond_merge[,18:63])] <- 0 # just so rowwise checking works. There were 0s in new and NAs in old
names(cond_merge)
check_conds(cond_merge, "H_new", "H_old") # 0
check_conds(cond_merge, "NO_new", "NO_old") # 0
check_conds(cond_merge, "AD_new", "AD_old") #0
# no ALB, BC, BWA, DOG, GM, RPS, SB, SOD, SW so no check for it
check_conds(cond_merge, "BBD_new", "BBD_old") #0
check_conds(cond_merge, "CAVL_new", "CAVL_old") #0
check_conds(cond_merge, "CAVS_new", "CAVS_old") #0
check_conds(cond_merge, "CW_new", "CW_old") # 0
check_conds(cond_merge, "DBT_new", "DBT_old") #0
check_conds(cond_merge, "EAB_new", "EAB_old") #0
check_conds(cond_merge, "EB_new", "EB_old") #0
check_conds(cond_merge, "G_new", "G_old") #0
check_conds(cond_merge, "HWA_new", "HWA_old") #0
check_conds(cond_merge, "ID_new", "ID_old") #0
check_conds(cond_merge, "OTH_new", "OTH_old") #0
check_conds(cond_merge, "SPB_new", "SPB_old") #0

#++++++++ Remaining issue: NC for 2011 and PM for trees with existing tree conditions

#------ Vines -----
vines_new <- do.call(joinTreeVineSpecies, c(compev_arglist, speciesType = 'all')) %>%
  select(Plot_Name, StartYear, IsQAQC, TagCode, VinePositionCode, VinePositionLabel, ScientificName, TSN)

names(vines_new)
#View(VIEWS_MIDN$COMN_TreesVine)
vines_old <- left_join(xrtreecond %>% select(Tree_Data_ID, Tree_Condition_ID, Species_ID),
                       tlucond %>% select(Tree_Condition_ID, Code), by = c("Tree_Condition_ID")) %>%
  right_join(tree_evs_old, ., by = intersect(names(tree_evs_old), names(.))) %>%
  left_join(., plants %>% select(TSN, Latin_Name), by = c("Species_ID" = "TSN")) %>%
  select(Plot_Name, Year, Event_QAQC, Tree_Number_MIDN, Status_ID, Code, Species_ID, Latin_Name) %>%
  filter(!is.na(Code)) %>% filter(!is.na(Plot_Name)) %>% filter(Code %in% c("VIN", "VOB")) %>%
  arrange(Plot_Name, Year, Tree_Number_MIDN) %>% # drop trees without conditions
  mutate(TagCode = as.numeric(Tree_Number_MIDN)) %>% select(-Tree_Number_MIDN)

nrow(vines_old) #1636
nrow(vines_new) #1634 # Persicaria perfoliata is getting dropped b/c not a woody species

vines_merge <- full_join(vines_new, vines_old, by = c("Plot_Name" = "Plot_Name",
                                                      "StartYear" = "Year",
                                                      "IsQAQC" = "Event_QAQC",
                                                      "TagCode" = "TagCode",
                                                      "TSN" = "Species_ID"),
                         suffix = c("_new", "_old"))
head(vines_merge)
table(vines_merge$VinePositionCode, vines_merge$StartYear, useNA = 'always') # B only >2019
table(vines_merge$Code, vines_merge$StartYear, useNA = 'always') # 2007 and 2019 are off by 1, with NAs
  # Persicaria perfoliata, an herbaceous vine was recorded in old and not migrating to new database. Decided this is OK,
  # b/c unlikely to make it into the crown.
table(vines_merge$ScientificName, vines_merge$Latin_Name, useNA = 'always') # Looks good

# check if trees with multiple vines are migrating
mult_vines <- vines_new %>% group_by(Plot_Name, StartYear, IsQAQC, TagCode) %>% summarize(num_vines = n()) %>% filter(num_vines > 1)
# Multiple vines are now migrating into the database, which is great. The view is actually duplicating them, but my function
# uses unique() to fix it for now.
#++++++ No issues remaining

#----- Quadrat Character -----
qchar_new <- joinQuadData(park = 'all', from = 2007, to = 2019, locType = 'all', eventType = 'all',
                          valueType = 'all', QAQC = T)

qchar_new %>% filter(is.na(CharacterLabel) | num_quads < 12) %>%
  select(Plot_Name, StartYear, IsQAQC, CharacterLabel, num_quads) %>% arrange(Plot_Name, StartYear)
# COLO-380;RICH-63/RICH-73 all showing up correctly

plotevs_old <- forestMIDNarch::joinLocEvent(park = 'all', from = 2007, to = 2019, eventType = 'all',
                                            locType = 'all', QAQC = T, rejected = FALSE)

qchar_old <- merge(plotevs_old, quadchr, by = intersect(names(plotevs_old), names(quadchr)), all.x = T, all.y = F) %>%
  select(Plot_Name,Year, Event_QAQC, Quadrat_ID, A2:CC)

names(quadchr)

quad_names = c('A2', 'A5', 'A8', 'AA', 'B2', 'B5', 'B8', 'BB', 'C2', 'C5', 'C8', 'CC')
head(qchar_old)

qchar_old[ , quad_names][qchar_old[ , quad_names] == 1] <- 0.1
qchar_old[ , quad_names][qchar_old[ , quad_names] == 2] <- 1.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 3] <- 3.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 4] <- 7.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 5] <- 17.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 6] <- 37.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 7] <- 62.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 8] <- 85
qchar_old[ , quad_names][qchar_old[ , quad_names] == 9] <- 97.5

table(qchar_old$Quadrat_ID)
table(qchar_new$CharacterLabel, useNA = 'always')

qchar_old <- qchar_old %>% mutate(Cover_Type = case_when(Quadrat_ID == 2 ~ "Soil",
                                                         Quadrat_ID == 3 ~ "Rock",
                                                         Quadrat_ID == 4 ~ "Stem",
                                                         Quadrat_ID == 5 ~ "Wood",
                                                         Quadrat_ID == 6 ~ "Sphagnum",
                                                         Quadrat_ID == 7 ~ "NonSphagnum",
                                                         Quadrat_ID == 8 ~ "Lichens",
                                                         Quadrat_ID == 9 ~ "Herbs")) %>% # 9 = Herbs MIDN
  select(Plot_Name, Year, Event_QAQC, Cover_Type, A2, A5, A8, AA, B2, B5, B8, BB, C2, C5, C8, CC)

#incomplete_old <-
  qchar_old[which(!complete.cases(qchar_old)),]
# Plots with at least one quad missing quadrat data:
   # SQs should be NS for: COLO-380-2018 : correct
   # RICH-063-2011 AA, B2, B5, B8 (NS correct for data/quads);
   # RICH-073-2015 B5 & B8 (NS correct for data/quads);
   # PMs should replace blanks for: APCO-184-2009 A8 Herbs; FRSP-106-2008 A2 Herbs (I don't see this anymore).

table(qchar_old$Cover_Type, useNA = 'always') # Lichens are the only thing different b/c added later
table(qchar_new$CharacterLabel, useNA = 'always') # Make sure Lichens are NC for early years. They're all NC.
table(qchar_new$Txt_Cov_A2, qchar_new$CharacterLabel, qchar_new$StartYear, useNA = 'always') # No issues

check_qchr <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "CharacterLabel", col1, col2)]}
  )) %>% bind_rows()
}

quadchr_merge <- merge(qchar_new, qchar_old,
                       by.x = c("Plot_Name", "StartYear", "IsQAQC", "CharacterLabel"),
                       by.y = c("Plot_Name", "Year", "Event_QAQC", "Cover_Type"),
                       all.x = T, all.y = T)

check_qchr(quadchr_merge, "Pct_Cov_A2", "A2") #0
check_qchr(quadchr_merge, "Pct_Cov_A5", "A5") #0
check_qchr(quadchr_merge, "Pct_Cov_A8", "A8") #0
check_qchr(quadchr_merge, "Pct_Cov_AA", "AA") #0

check_qchr(quadchr_merge, "Pct_Cov_B2", "B2") #0
check_qchr(quadchr_merge, "Pct_Cov_B5", "B5") #0
check_qchr(quadchr_merge, "Pct_Cov_B8", "B8") #0
check_qchr(quadchr_merge, "Pct_Cov_BB", "BB") #0

check_qchr(quadchr_merge, "Pct_Cov_C2", "C2") #0
check_qchr(quadchr_merge, "Pct_Cov_C5", "C5") #0
check_qchr(quadchr_merge, "Pct_Cov_C8", "C8") #0
check_qchr(quadchr_merge, "Pct_Cov_CC", "CC") #0

qchar_vw <- get("COMN_QuadCharacter", envir = VIEWS_MIDN)
table(qchar_vw$CharacterLabel, qchar_vw$CoverClassLabel)

#+++++ No issues

#----- Quadrat Species -----
quadspp_old <- forestMIDNarch::joinQuadData(from = 2007, to = 2019, QAQC = T, eventType = "all", locType = "all") %>%
               mutate(Year = as.numeric(Year)) %>%
               mutate(Latin_Name2 = ifelse(Latin_Name == "No species recorded", "None present", Latin_Name))

quadspp_new <- joinQuadSpecies(from = 2007, to = 2019,
                               QAQC = T, eventType = 'all', locType = 'all', valueType = 'midpoint')
nrow(quadspp_new)#10172
nrow(quadspp_old) #10169
#3 new quadspp rows.

names(quadspp_new)
names(quadspp_old)

quadspp_merge <- full_join(quadspp_new,
                           quadspp_old,
                           by = c("Plot_Name" = "Plot_Name",
                                  "StartYear" = "Year",
                                  "IsQAQC" = "Event_QAQC",
                                  "ScientificName" = "Latin_Name2")) %>%
  select(Plot_Name, PlotID, EventID, StartYear, cycle.x, cycle.y, IsQAQC,
         Confidence, SQQuadSppCode, TSN.x, TSN.y, ScientificName, Latin_Name,
         num_quads, quad_avg_cov,
         quad_pct_freq, avg.cover, avg.freq, Pct_Cov_A2: Pct_Cov_CC,
         A2:CC)

check_qspp <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "Plot_Name2", "StartYear", "IsQAQC", "ScientificName", "Latin_Name",
              "SQQuadSppCode",
               col1, col2)]}
  )) %>% bind_rows()
}

# Check diff Pct cover
quadspp_merge %>% mutate(cov_diff = quad_avg_cov - avg.cover) %>%
  filter(cov_diff > 0.01) %>%
  select(PlotID, EventID, Plot_Name, StartYear, IsQAQC, TSN.x, ScientificName, num_quads,
         quad_avg_cov, avg.cover, quad_pct_freq, avg.freq, cov_diff) #0

# Check % freq
quadspp_merge %>% mutate(freq_diff = quad_pct_freq - 100*(avg.freq)) %>%
  filter(freq_diff > 0.01) %>%
  select(PlotID, EventID, Plot_Name, StartYear, IsQAQC, TSN.x, ScientificName, num_quads,
         quad_avg_cov, avg.cover, quad_pct_freq, avg.freq, freq_diff) #0

quadsamp$numQuads <- apply(quadsamp[,c(3:14)], 1, sum)
quads1 <- merge(plotevs_old, quadsamp[, c("Event_ID", "numQuads")], all = TRUE)

quadspp <- merge(quads[,c("Event_ID","TSN",
                          "qA2_Cover_Class_ID", "qA5_Cover_Class_ID","qA8_Cover_Class_ID","qAA_Cover_Class_ID",
                          "qB2_Cover_Class_ID", "qB5_Cover_Class_ID","qB8_Cover_Class_ID","qBB_Cover_Class_ID",
                          "qC2_Cover_Class_ID", "qC5_Cover_Class_ID","qC8_Cover_Class_ID","qCC_Cover_Class_ID")],
                 plants[,c("TSN","Latin_Name")],
                 by = "TSN", all.x = T) %>% filter(Event_ID != "4AFBA34C-83F8-4F67-8B7C-8F6E096AB21D")

new_quads <- c("Pct_Cov_A2", "Pct_Cov_A5", "Pct_Cov_A8", "Pct_Cov_AA",
               "Pct_Cov_B2", "Pct_Cov_B5", "Pct_Cov_B8", "Pct_Cov_BB",
               "Pct_Cov_C2", "Pct_Cov_C5", "Pct_Cov_C8", "Pct_Cov_CC"
               )
quads2 <- merge(quads1, quadspp, by = "Event_ID", all.x = T)
names(quads2)
quads2[,14:25][quads2[,14:25]==1]<-0.1
quads2[,14:25][quads2[,14:25]==2]<-1.5
quads2[,14:25][quads2[,14:25]==3]<-3.5
quads2[,14:25][quads2[,14:25]==4]<-7.5
quads2[,14:25][quads2[,14:25]==5]<-17.5
quads2[,14:25][quads2[,14:25]==6]<-37.5
quads2[,14:25][quads2[,14:25]==7]<-62.5
quads2[,14:25][quads2[,14:25]==8]<-85
quads2[,14:25][quads2[,14:25]==9]<-97.5
old.names<-names(quads2[,14:25])
old.names
new.names<-c('A2','A5','A8','AA','B2','B5','B8','BB','C2','C5','C8','CC')
quads2<-quads2 %>% rename_at(all_of(vars(old.names)),~new.names)
quads2[,c(14:25)][is.na(quads2[,c(14:25)])]<-0
table(complete.cases(quads2[,14:25]))
str(quads2)
quads2$Plot_Name2 <- quads2$Plot_Name
quads2$Year <- as.numeric(quads2$Year)
quads2 <- quads2 %>% mutate(Latin_Name2 = ifelse(Latin_Name == "No species recorded", "None present", Latin_Name))

quadspp_merge <- full_join(quadspp_new,
                           quads2 %>% select(Plot_Name, Plot_Name2, Year, Event_QAQC, numQuads,
                                             TSN, Latin_Name, Latin_Name2, A2:CC),
                           by = c("Plot_Name" = "Plot_Name",
                                  "StartYear" = "Year",
                                  "IsQAQC" = "Event_QAQC",
                                  "ScientificName" = "Latin_Name2")) %>% filter(!is.na(Plot_Name)) # drops 1 mostly NA record came in from old

check_qspp(quadspp_merge, "Pct_Cov_A2", "A2") # 3 records. 3 b/c better SQ handling. #VAFO-161 NP is fixed
check_qspp(quadspp_merge, "Pct_Cov_A5", "A5") #  3 records. 3 b/c better SQ handling. VAFO-161 NP is fixed
check_qspp(quadspp_merge, "Pct_Cov_A8", "A8") #  4 records. 3 b/c better SQ handling. VAFO-161 NP is fixed
check_qspp(quadspp_merge, "Pct_Cov_AA", "AA") # 9 records. All b/c better SQ handling

check_qspp(quadspp_merge, "Pct_Cov_B2", "B2") # 9 records. All b/c better SQ handling
check_qspp(quadspp_merge, "Pct_Cov_B5", "B5") # 14 records. All b/c better SQ handling
check_qspp(quadspp_merge, "Pct_Cov_B8", "B8") # 14 records. All b/c better SQ handling
check_qspp(quadspp_merge, "Pct_Cov_BB", "BB") # 3 records. All b/c better SQ handling

check_qspp(quadspp_merge, "Pct_Cov_C2", "C2") # 3 records. All b/c better SQ handling
check_qspp(quadspp_merge, "Pct_Cov_C5", "C5") # 3 records. All b/c better SQ handling
check_qspp(quadspp_merge, "Pct_Cov_C8", "C8") # 3 records. All b/c better SQ handling
check_qspp(quadspp_merge, "Pct_Cov_CC", "CC") # 3 records. All b/c better SQ handling

check_qspp(quadspp_merge, "ScientificName", "Latin_Name") # 5. All no species vs none present. no concerns

quadspp <- get("MIDN_QuadSpecies", envir = VIEWS_MIDN) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQQuadSppCode,
         QuadratCode, TSN, ScientificName, CoverClassCode, CoverClassLabel,
         ConfidenceClassCode, IsCollected, QuadSppNote)

quadspp %>% filter(PlotCode == 161 & StartYear == 2009)
#vafo.161.2009 is fixed!

table(quadspp$SQQuadSppCode)
sort(unique(quadspp$ScientificName)) # No species recorded not on list anymore!
#++++++ VAFO-161-2009 SQ should be NP not NS.

#----- Quad seedlings
# Catching no species recorded and incorrect NP SQ
qseeds <- VIEWS_MIDN$MIDN_QuadSeedlings
table(qseeds$SQSeedlingCode, qseeds$ScientificName)
# There are still 2990 "No species recorded" with NP (3 more than 4/22)
# 5/3 resolved
table(qseeds$SQSeedlingCode) #2990 matches number of "No species recorded"

table(qseeds$SQSeedlingCode) # the numbers below are close to what it should be
# with a few more NS b/c of COLO-380-2018
#NS     NP       SS
#62   2990  107912
table(qseeds$ScientificName, qseeds$SQSeedlingCode, useNA = 'always')
# Still have 2990 "No species recorded" coming in. These shouldn't be migrating.
# It at least matches the number of NP in the higher level table.

seeds_new <- joinQuadSeedlings(locType = 'all', eventType = 'all', QAQC = T, valueType = 'all', canopyForm = 'all')
table(seeds_new$ScientificName, seeds_new$SQSeedlingCode, useNA = 'always')

#------ Quadrat Seedlings ------
seeds_vw <- get("MIDN_QuadSeedlings", envir = VIEWS_MIDN) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQSeedlingCode,
         QuadratCode, TSN, ScientificName, SizeClassCode, SizeClassLabel, Count, CoverClassCode, CoverClassLabel)
sort(unique(seeds_vw$ScientificName)) # "No species recorded" included. Wrong.

table(seeds_vw$SQSeedlingCode)
table(seeds_vw$SQSeedlingCode, seeds_vw$StartYear)
# 62 NS total
# 12 COLO-380; 12 PETE-185; 4 RICH-063; 2 RICH-073; 32 VAFO 9999 = 62; Correct
# 3 ISSUES Fixed:
# VAFO-036-2011 B8 is now NS;
# GETT-258-2010 AA is now NS;
# FRSP-276-2010 CC is now NS;
# No issues

seed_new <- joinQuadSeedlings(from = 2007, to = 2019, eventType = 'all', locType = 'all', QAQC = TRUE)
#seed_inv <- joinQuadSeedlings(speciesType = 'invasive')
table(seed_new$SQSeedlingCode, seed_new$StartYear)
table(seeds_vw$ScientificName) # 2990 No species recorded still
table(seeds_vw$SQSeedlingCode)# 2990 matches number of visits that have NP SQ.
#+++++ Issues remaining: No species recorded needs to be dropped.
# 5/3 "No species recorded" is gone. No issues remaining

#----- Regen Data -----
reg_new <- joinRegenData(eventType = 'all', locType = 'all', QAQC = T, canopyForm = 'all', speciesType = 'all')
reg_old <- forestMIDNarch::joinRegenData(eventType = 'all', locType = 'all', QAQC = T,
                                         from = 2007, to = 2019, canopyForm = 'all') %>%
            filter(seed.den + sap.den > 0) # drops species with 0.1 cover and no tallies

reg_old$Latin_Name[reg_old$Latin_Name == "No species recorded"] <- "None present"
reg_old2 <- reg_old %>% filter(!Latin_Name %in% c("None present", "no species recorded"))

reg_merge <- merge(reg_new, reg_old2, by.x = c("Plot_Name", "StartYear", "IsQAQC", "ScientificName"),
                   by.y = c("Plot_Name", "Year", "Event_QAQC", "Latin_Name"),
                   all.x=T, all.y=T) %>% filter(ScientificName != "None present")
merge_nas <- reg_merge[is.na(reg_merge$Network),] # no As

check_reg <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "ScientificName", col1, col2)]}
  )) %>% bind_rows()
}

head(reg_merge)
# Issues below (n=36) are all due to diff/better error handling in new function
check_reg(reg_merge, "seed_15_30cm", "seed15.30") # 2. APCO-007-2007; APCO-009-2007; Filled 0 for new. OK
check_reg(reg_merge, "seed_30_100cm", "seed30.100") # 2. APCO-007-2007; APCO-009-2007; Filled 0 for new. OK
check_reg(reg_merge, "seed_100_150cm", "seed100.150") # 2. APCO-007-2007; APCO-009-2007; Filled 0 for new. OK
check_reg(reg_merge, "seed_p150cm", "seed150p")# 2. APCO-007-2007; APCO-009-2007; Filled 0 for new. OK
check_reg(reg_merge, "sap_den", "sap.den") # 11. Diff b/c new function includes saps with PM DBH. Old didn't. OK.

#+++++ 5/3 no issues remain

#----- Microplot Shrubs -----
shrubs_vw <- get("COMN_MicroplotShrubs", envir = VIEWS_MIDN) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQShrubCode,
         MicroplotCode, TSN, ScientificName, CoverClassCode, CoverClassLabel)
table(shrubs_vw$SQShrubCode, shrubs_vw$StartYear) # 9 NS PETE-185;VAFO-9999;COLO-380 OK
table(shrubs_vw$CoverClassCode, shrubs_vw$StartYear)
# 2007-8 are all NC, 2009-2019 are comb of NC and other stuff. All looks good.

sort(unique(shrubs_vw$ScientificName)) # No species recorded not on the list. Good.

shrub_old <- forestMIDNarch::joinMicroShrubData(from = 2007, to = 2019, locType = 'all', eventType = 'all', QAQC = T) %>%
               mutate(Year = as.numeric(Year))

shrub_old$Latin_Name[shrub_old$Latin_Name == "No species recorded"] <- "None present"
shrub_new <- joinMicroShrubData(from = 2007, to = 2019, locType = 'all', eventType = 'all', QAQC = T)


shrub_merge <- full_join(shrub_new, shrub_old, by = c("Plot_Name" = "Plot_Name",
                                                      "StartYear" = "Year",
                                                      "IsQAQC" = "Event_QAQC",
                                                      "ScientificName" = "Latin_Name") )

table(complete.cases(shrub_merge$Event_ID)) # 266 FALSE
na_evs <- shrub_merge %>% filter(is.na(Event_ID)) # These are all "None present" OK

check_shrbs <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "StartYear", "IsQAQC",
              "ScientificName", col1, col2)]}
  )) %>% bind_rows()
}

names(shrub_merge)
table(shrub_merge$Pct_Cov_UR, shrub_merge$StartYear, useNA = 'always')

shrub_merge %>% select(Plot_Name, StartYear, IsQAQC, ScientificName, shrub_avg_cov, cover) %>%
  mutate(cov_diff = abs(shrub_avg_cov - cover)) %>% filter(cov_diff > 0.1)
# GETT-252-2010 is different b/c combination of PM and pct_cover. Nothing to change

#++++++ No issues remaining with shrubs

#----- Microplot Saplings ------
saps_vw <- get("MIDN_MicroplotSaplings", envir = VIEWS_MIDN) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, StartDate, IsQAQC, SQSaplingCode,
         MicroplotCode, TSN, ScientificName, DBHcm)
table(saps_vw$ScientificName) # "No species recorded" not included. Good.
sort(unique(saps_vw$ScientificName))  # "No species recorded" not included. Good.

table(saps_vw$SQSaplingCode)
#  NP   NS   SS
# 684  13  14599

sap_NS <- write.csv(saps_vw %>% filter(SQSaplingCode == "NS"), "./testing_scripts/Sapling_NS.csv")
table(saps_vw$SQSaplingCode, saps_vw$StartYear)
    # 10 NS: 2 NS are COLO-380-2018, but UR SQ is SS instead of NS. Change after final migration.
    # GETT-029-2015 QAQC UL and VAFO-040-2015-QAQC UR should be NP, not NS. Change by hand after final migration.

table(saps_vw$SQSaplingCode, saps_vw$MicroplotCode) # B 5; UL 4; UR 3. Eventually should have the same # for all micros

saps_new <- joinMicroSaplings(locType = 'all', QAQC = T, eventType = 'all', canopyForm = 'all', speciesType = 'all')
length(unique(saps_new$EventID)) #1182
table(complete.cases(saps_new$ScientificName)) # all T
table(saps_new$ScientificName)
table(saps_new$SQSaplingCode) # only 4 NS, 684 NP and 14234 SS. No ND. Good.
#++++++ Remaining sapling issues #GETT-029-2015; VAFO-040-2015 have a quad that should be NP not NS.

#----- joinRegenData -----
reg_old <- forestMIDNarch::joinRegenData(from = 2007, to = 2019, QAQC = T, locType = 'all', speciesType = 'all', canopyForm = 'all') %>%
  mutate(Latin_Name2 = ifelse(Latin_Name %in% c("No species recorded", 'no species recorded'), "None present", Latin_Name),
         Latin_Name2 = ifelse(Latin_Name == "MissingData", "Not Sampled", Latin_Name2),
         Year = as.numeric(Year)) %>% filter(sap.den + seed.den > 0)

reg_new <- joinRegenData(from = 2007, to = 2019, QAQC = T, locType = 'all', eventType = 'all', speciesType = 'all', canopyForm = 'all')
length(unique(reg_new$EventID)) #1182

reg_merge <- full_join(reg_new, reg_old, by = c('Plot_Name' = 'Plot_Name',
                                                'StartYear' = 'Year',
                                                'IsQAQC' = "Event_QAQC",
                                                "ScientificName" = "Latin_Name2"),
                       suffix = c("_new", "_old"))

reg_merge_ss <- reg_merge %>% filter(ScientificName != "None present")

check_reg <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "StartYear", "IsQAQC",
              "ScientificName", "num_quads", "num_micros", col1, col2)]}
  )) %>% bind_rows()
}

names(reg_merge)

table(reg_new$ScientificName)
table(reg_old$Latin_Name2)

stock_check <- check_reg(reg_merge_ss, "stock_new", "stock_old") # Different b/c change in stocking.
check_reg(reg_merge_ss, "seed_15_30cm", "seed15.30") # differences are when there are < 12 quads and better SQ handling
check_reg(reg_merge_ss, "seed_30_100cm", "seed30.100") # differences are when there are < 12 quads and better SQ handling
check_reg(reg_merge_ss, "seed_100_150cm", "seed100.150") # differences are when there are < 12 quads and better SQ handling
check_reg(reg_merge_ss, "seed_p150cm", "seed150p") # differences are when there are < 12 quads and better SQ handling
check_reg(reg_merge_ss, "seed_den", "seed.den") # differences are when there are <12 quads and better SQ handling
check_reg(reg_merge_ss, "sap_den", "sap.den") # differences b/c SQs not correct for several plots and better SQ handling
#++++++ No new issues not already reported for seedlings and saplings.

#----- Additional Species -----
addspp_vw <- get("COMN_AdditionalSpecies", envir = VIEWS_MIDN)
addspp_vw$Plot_Name <- paste(addspp_vw$ParkUnit, sprintf("%03d", addspp_vw$PlotCode), sep = "-")
addspp_vw <- addspp_vw %>%
  select(Plot_Name, PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQAddSppCode,
         TSN, ScientificName, ConfidenceClassCode, IsCollected, Note, SQAddSppNotes)
table(addspp_vw$SQAddSppCode)
# NP    NS    SS
# 69   147 4214
# 69   159  4201
table(addspp_vw$StartYear, addspp_vw$SQAddSppCode) # Cleaned up pre-migration database so all 2008 SQs correctly migrate as NS

# addspp08 <- addspp_vw %>% filter(StartYear == 2008 & SQAddSppCode != "NS")
# write.csv(addspp08, "./testing_scripts/addspp_2008_delete.csv")
nrow(filter(addspp_vw, ScientificName == "No species recorded"))
# 69 records with No species recorded still
# matches number of NP SQs. Just need to drop these from tblCOMN_AdditionalSpecies

addspp_new <- do.call(joinAdditionalSpecies, arglist) %>% filter(ScientificName != "Not Sampled")

addspp_old <- forestMIDNarch::joinLocEvent(locType = 'all', eventType = 'all', from = 2006, to = 2019, QAQC = T) %>%
  left_join(., addspp) %>% left_join(., plants[, c("TSN", "Latin_Name")]) %>%
  select(Plot_Name, Year, Event_QAQC, TSN, Latin_Name, Confidence_ID, Collected, Notes) %>%
  mutate(Latin_Name2 = ifelse(Latin_Name == "No species recorded", "None present", Latin_Name))

addspp_merge <- merge(addspp_new, addspp_old,
                      by.x = c("Plot_Name", "StartYear", "IsQAQC", "TSN"),
                      by.y = c("Plot_Name", "Year", "Event_QAQC", "TSN"),
                      all.x = T, all.y = T)

check_spp <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", col1, col2)]}
  )) %>% bind_rows()
}

check_spp(addspp_merge, "ScientificName", "Latin_Name2")
#    Plot_Name StartYear IsQAQC ScientificName                Latin_Name2
# 1  GEWA-320      2015      0   None present                       <NA> # NULL in old DB. Migrating as SS. Change to NP post migration.
# 2  RICH-226      2009      0           <NA> Polystichum acrostichoides # Species is back!

#+++++ We can fix the issues after the final migration, rather than have Stephen fix with code.
#+++++ 2007 & 2008 correctly migrate as NS and No species recorded are now gone.
# No issues remain

#----- Soil data

# need to remove records that weren't sampled but had earthworms recorded
soildata2 <- soildata[!grepl("[++]", soildata$Notes), -c(10:13)] # drop updated/created cols
soildata2 <- soildata2[!grepl("[No soils]", soildata2$Notes),]
names(soildata2)

soil_old <- merge(soildata2, soilsamp[,-c(13:16)],
                  by = intersect(names(soildata2), names(soilsamp[,-c(13:16)])), all = TRUE)
plotevs_old <- forestMIDNarch::joinLocEvent(from = 2007, to = 2019, QAQC = T, locType = 'all', eventType = 'all')
soil_old2 <- merge(plotevs_old, soil_old, by = intersect(names(plotevs_old), names(soil_old)), all.x = FALSE, all.y = TRUE) %>%
  filter(!is.na(Location_ID)) %>% select(Plot_Name, Year, Event_QAQC,
                                         Sampling_Position, Sample_Type, Horizon_Type, Archived,
                                         Sample_Number, Litter_Depth, FF_Depth,
                                         A_Horizon_Depth, Total_Excavation_Depth, Notes, Comments, Sample_Missed)
names(soil_old2)
# convert NA horizons to 0, but not total
soil_old2[,c(9:11)][is.na(soil_old2[,c(9:11)])]<-0

# Check views
soilsamp_vw <- get("COMN_SoilSample", envir = VIEWS_MIDN) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
         SQSoilCode, SampleSequenceCode, SoilLayerLabel,
         Depth_cm, Note) %>%
  filter(StartYear > 2006 & !is.na(SoilLayerLabel) #& StartYear < 2020
  )

table(soilsamp_vw$SQSoilCode) # All SS. Good.
length(unique(soilsamp_vw$EventID)) # 328

soillab_vw <- get("COMN_SoilLab", envir = VIEWS_MIDN) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
         LabLayer, LabDateSoilCollected, UMOSample:ECEC, LabNotes, EventID, PlotID) %>%
  filter(StartYear > 2006 #& StartYear < 2020
  )

length(unique(soillab_vw$EventID)) # 328. Looks similar enough to move on.

# Now to compare old and new

soilold_sum <- soil_old2 %>% group_by(Plot_Name, Year, Event_QAQC, Sample_Type, Archived) %>%
  summarize(litter = mean(Litter_Depth, na.rm = T),
            O_hor = mean(FF_Depth, na.rm = T),
            A_hor = mean(A_Horizon_Depth, na.rm = T),
            Tot_dep = mean(Total_Excavation_Depth, na.rm =T),
            numsamps = length(unique(!is.na(Sample_Number))))

#soilsamp_wide comes from line 127 in joinSoilSampleData.R
soilsamp_wide$Plot_Name <-  paste(soilsamp_wide$ParkUnit, sprintf("%03d", soilsamp_wide$PlotCode), sep = "-")

soilsamp_merge <- merge(soilsamp_wide, soilold_sum,
                        by.x = c("Plot_Name", "StartYear", "IsQAQC"),
                        by.y = c("Plot_Name", "Year", "Event_QAQC"), all = T)

soilsamp_merge %>% filter(is.na(EventID)) #0

check_soils <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "Sampling_Position", "SampleSequenceCode", col1, col2)]}
  )) %>% bind_rows()
}
names(soilsamp_merge)
soilsamp_check <- soilsamp_merge %>% mutate(lit_diff = abs(Litter_cm - litter),
                                           O_diff = abs(O_Horizon_cm - O_hor),
                                           A_diff = abs(A_Horizon_cm - A_hor),
                                           tot_diff = abs(Total_Depth_cm - Tot_dep)) %>%
  filter(lit_diff > 0.5 | O_diff > 0.5 | A_diff > 0.5 | tot_diff > 0.5) #0

check_soils(soilsamp_merge, "Note", "Comments") # 0
# Hard to check soils b/c there's no tab for it in the field app, but based on my comparisions, it all looks good.

# Soil lab data
soillab_old <- merge(soildata2, soillab[,-c(1, 34, 35)],
                     by = intersect(names(soildata2), names(soillab[,-c(1, 34, 35)])), all = TRUE)
soillab_old2 <- merge(plotevs_old, soillab_old, by = intersect(names(plotevs_old), names(soillab_old)),
                      all.x = FALSE, all.y = TRUE) %>%
  filter(!is.na(Location_ID) & Year > 2006) %>% select(Plot_Name, Year, Event_QAQC, Layer, UMO_Sample:ECEC, Notes,
                                                       Sampling_Position, Sample_Type, Archived)
head(soillab_old2)

#++++++++ No lab-related issues to report (though didn't check as thoroughly)

# Done with 4/26 migration check.
# Done with 4/29 migration check. Biggest

taxa_wide <- prepTaxa()

#-------------------------------------
# Testing summary functions with latest updates
plotTreeMap(plotName = "FRSP-001", from = 2016, to = 2019 ) # worked

head(sumQuadGuilds(speciesType = 'invasive'))
head(sumSapDBHDist())
head(sumSpeciesList())
head(sumSpeciesList(speciesType = 'invasive'))
head(sumStrStage())
head(sumTreeDBHDist())

