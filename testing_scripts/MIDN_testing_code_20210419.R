library(forestMIDN)
library(tidyverse)

#----- Testing the import/export functions
#importData(instance = 'local', server = "localhost", new_env = T) # release 1.0.21 with IsGerminant added to NETNquadspp
#importData(instance = 'server', server = "INP2300VTSQL16\\IRMADEV1", new_env = T) # release 1.0.22 w/ migration fixes

path = "C:/Forest_Health/exports/MIDN"

#exportCSV(path, zip = TRUE)
importCSV(path = path, zip_name = "MIDN_Forest_20210419.zip")# release 1.0.22 w/ migration fixes

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

# Checking data function
check_data <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", col1, col2)]}
  )) %>% bind_rows()
}

# import old database for comparisons
forestMIDNarch::importData(type = 'file',
  path='D:/NETN/Monitoring_Projects/Forest_Health/Database/MIDN/2021_Forest_Database/MIDN_FVM_BE_MASTER_20210408_Migration.accdb')

#forestMIDNarch::importData()

names(VIEWS_MIDN)
names(VIEWS_MIDN$MIDN_QuadSpecies)

#----- Testing joinLocEvent and migration -----
arglist <- c()
plotevs_old <- do.call(forestMIDNarch::joinLocEvent, c(arglist, output = 'verbose'))
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
check_data(stand_merge, "Pct_Understory_Low.x", "Pct_Understory_Low.y")
check_data(stand_merge, "Pct_Understory_Mid.x", "Pct_Understory_Mid.y")
check_data(stand_merge, "Pct_Understory_High.x", "Pct_Understory_High.y")
check_data(stand_merge, "Pct_Bare_Soil", "Pct_Bare_Soil_Cover")
check_data(stand_merge, "Pct_Rock", "Pct_Rock_Cover")
check_data(stand_merge, "Pct_Lichen", "Pct_Lichen_Cover")
check_data(stand_merge, "Pct_Bryophyte", "Pct_Bryophyte_Cover")
check_data(stand_merge, "Pct_Water", "Pct_Surface_Water_Cover")
check_data(stand_merge, "Pct_Bare_Soil", "Pct_Bare_Soil_Cover")
check_data(stand_merge, "Pct_Trampled", "Pct_Trampled_Cover")
check_data(stand_merge, "Microtopography", "Microtopography_ID") # Lots of NC
check_data(stand_merge, "Deer_Browse_Index", "Deer_Browse_Line_ID") # All are 5s that are recorded pre 2010 as DBL pres
table(stand_merge$Deer_Browse_Label, stand_merge$StartYear, useNA = 'always') # PMs are accurate
table(stand_merge$Deer_Browse_Label, stand_merge$Deer_Browse_Line_pre09_ID)

ht_dif <- stand_merge %>% mutate(cod_dif = abs(Avg_Height_Codom - Avg_Codom_HT),
                                 int_dif = abs(Avg_Height_Inter - Avg_Inter_HT)) %>%
  select(Plot_Name, StartYear, IsQAQC, Avg_Height_Codom, Avg_Codom_HT,
         cod_dif, Avg_Height_Inter, Avg_Inter_HT, int_dif )

slope_dif <- stand_merge %>% mutate(slp_dif = abs(PlotSlope - Plot_Slope_Deg)) %>%
  select(Plot_Name, StartYear, IsQAQC, EventID, PlotSlope, Plot_Slope_Deg, slp_dif)
  # Only differences are QAQC events, which we don't care about

#----- Stand Disturbance -----
stdist_new <- do.call(joinStandDisturbance, arglist)

library(RODBC)
db <- odbcConnect("MIDNFVM") #
disturb<-sqlFetch(db,"tbl_Disturbances")
disttlu<-sqlFetch(db,"tlu_Disturbance_Codes")
disttlutc<-sqlFetch(db,"tlu_Disturbance_Threshhold_Codes")
odbcClose(db)

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
ht_dif <- stand_merge %>% mutate(cod_dif = abs(Avg_Height_Codom - Avg_Codom_HT),
                                 int_dif = abs(Avg_Height_Inter - Avg_Inter_HT)) %>%
  select(Plot_Name, StartYear, IsQAQC, Avg_Height_Codom, Avg_Codom_HT,
         cod_dif, Avg_Height_Inter, Avg_Inter_HT, int_dif)

# Tree height issues remain. +++I can't seem to find where I reported this now.+++
# Have to start from scratch to find the issues

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
missing_tree_hts # 2 visits affected by duplicate stand heights not getting migrated
#+++++ Only issues with tree height +++++

#----- CWD -----
cwd_old <- do.call(forestMIDNarch::joinCWDData, arglist) %>% mutate(ScientificName = ifelse(Latin_Name == "No species recorded",
                                                                                            "None present", Latin_Name))
cwd_new <- do.call(joinCWDData, arglist)

cwd_merge <- merge(cwd_new, cwd_old, by.x = c("Plot_Name", "StartYear", "IsQAQC", "ScientificName", "DecayClassCode"),
                   by.y = c("Plot_Name", "Year", "Event_QAQC", "ScientificName", "Decay_Class_ID"),
                   all = TRUE)

cwd_dif <- cwd_merge %>% mutate(cwd_dif = abs(CWD_Vol.x - CWD_Vol.y)) %>% filter(cwd_dif > 0.1 & IsQAQC == FALSE) %>%
  select(Plot_Name:ScientificName, DecayClassCode, CWD_Vol.x, CWD_Vol.y, cwd_dif)
# Differences between old and new are all because of PMs added where missing in old, or b/c slopes were wrong for old CWD vol.

#----- Tree Data
tree_old <- do.call(forestMIDNarch::joinTreeData, c(arglist, list(speciesType = 'all', status = 'all'))) %>%
  mutate(TagCode = as.numeric(Tree_Number_MIDN))
tree_new <- do.call(joinTreeData, c(arglist, list(speciesType = 'all', status = 'all')))

non_tsucan <- tree_new %>% filter(ScientificName != "Tsuga canadensis")
table(non_tsucan$HWACode, non_tsucan$StartYear, useNA = 'always')

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

tree_dbh <- tree_merge %>% mutate(dbh_diff = abs(DBH - DBHcm)) %>%
  filter(dbh_diff > 0) %>%
  select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName, DBHcm, DBH, dbh_diff)
# differences are all OK.

status_check <- check_trees(tree_merge, "TreeStatusCode", "Status_ID")
# none
crown_check <- check_trees(tree_merge, "Crown_Class_ID", "CrownClassCode")
table(tree_merge$CrownClassCode, tree_merge$Crown_Class_ID, useNA = 'always')

# All trees migrated in with PM crown class codes
decay_check <- check_trees(tree_merge, "Decay_Class_ID", "DecayClassCode")
# No issues. Decays that are diff are DF and AS, which shouldn't have a decay class

check_trees(tree_merge, "HWACode", "HWA_Status")
table(tree_merge$HWACode, tree_merge$HWA_Status, useNA = 'always')

check_trees(tree_merge, "BBDCode", "BBD_Status")
table(tree_merge$BBDCode, tree_merge$BBD_Status, useNA = 'always')

check_trees(tree_merge, "IsDBHVerified", "DBH_Verified")
table(tree_merge$IsDBHVerified, tree_merge$DBH_Verified, useNA = 'always')

#check_trees(tree_merge, "Pct_Tot_Foliage_Cond", "Total_Foliage_Condition")
table(tree_merge$Pct_Tot_Foliage_Cond, tree_merge$Total_Foliage_Condition, tree_merge$StartYear, useNA = 'always')
# Pre 2011, 0s go to NA. 2011+ 0s go to 0. Not what we want


#----- Tree Foliage Conditions -----
fol_vw <- VIEWS_MIDN$COMN_TreesFoliageCond
table(fol_vw$PercentLeafAreaLabel, fol_vw$StartYear) # 17 records with Not Applicable before 2016, which is wrong
table(fol_vw$PercentLeafAreaLabel, fol_vw$FoliageConditionCode) # NA correctly applied to L, NO, S, W
table(fol_vw$TotalFoliageConditionLabel, fol_vw$FoliageConditionCode, useNA = 'always')
# <>NO conditions that have Not app. as TFC. These should actually be PM > 2007 and NC for 2007.
table(fol_vw$PercentLeavesLabel, fol_vw$FoliageConditionCode, useNA = 'always')
# 2 H and 2 N that are incorrectly NA- should be PM

# pre 2011 0s are converting to NA correctly. 2011+ are incorrectly 0 for both percent columns.

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
  select(Plot_Name, Year, Event_QAQC, Event_ID, Tree_ID, Tree_Number_MIDN, Total_Foliage_Condition,
         Tree_Data_ID, Status_ID) %>% filter(Status_ID %in% c("1", "AB", "AF", "AL", "AM", "AS",
                                                              "RB", "RF", "RL", "RS"))

fol_old <- left_join(tree_evs_old, fol_old1, by = "Tree_Data_ID") %>%
  select(Plot_Name, Year, Event_QAQC, Tree_Number_MIDN, Cond, Pct_Leaves_Aff, Pct_Leaf_Area) %>%
  arrange(Plot_Name, Year, Event_QAQC, Tree_Number_MIDN) %>%
  pivot_wider(names_from = Cond,
              values_from = c(Pct_Leaves_Aff, Pct_Leaf_Area),
              values_fill = NA_real_) %>%
  mutate(TagCode = as.numeric(Tree_Number_MIDN)) %>%
  select(-Pct_Leaves_Aff_NA, -Pct_Leaf_Area_NA, -Pct_Leaf_Area_W, -Pct_Leaf_Area_O, -Pct_Leaf_Area_S)

nrow(fol_old) #23401
nrow(fol_new) #23401

fol_merge <- merge(fol_new, fol_old,
                   by.x = c("Plot_Name", "StartYear", "IsQAQC", "TagCode"),
                   by.y = c("Plot_Name", "Year", "Event_QAQC", "TagCode"),
                   all = TRUE)

check_conds <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "TagCode", "IsQAQC", col1, col2)]}
  )) %>% bind_rows()
}

names(fol_merge)
lvs_C <- check_conds(fol_merge, "Pct_Leaves_Aff_C.x", "Pct_Leaves_Aff_C.y") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_H <- check_conds(fol_merge, "Pct_Leaves_Aff_H.x", "Pct_Leaves_Aff_H.y") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_L <- check_conds(fol_merge, "Pct_Leaves_Aff_L.x", "Pct_Leaves_Aff_L.y") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_N <- check_conds(fol_merge, "Pct_Leaves_Aff_N.x", "Pct_Leaves_Aff_N.y") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_S <- check_conds(fol_merge, "Pct_Leaves_Aff_S.x", "Pct_Leaves_Aff_S.y") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_W <- check_conds(fol_merge, "Pct_Leaves_Aff_W.x", "Pct_Leaves_Aff_W.y") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_O <- check_conds(fol_merge, "Pct_Leaves_Aff_O.x", "Pct_Leaves_Aff_O.y") # all diffs are NAs converted to 0s. I think they should still be NA.

la_C <- check_conds(fol_merge, "Pct_Leaf_Area_C.x", "Pct_Leaf_Area_C.y") # all diffs are NAs converted to 0s. I think they should still be NA.
la_H <- check_conds(fol_merge, "Pct_Leaf_Area_H.x", "Pct_Leaf_Area_H.y") # all diffs are NAs converted to 0s. I think they should still be NA.
la_N <- check_conds(fol_merge, "Pct_Leaf_Area_N.x", "Pct_Leaf_Area_N.y") # all diffs are NAs converted to 0s. I think they should still be NA.

# +++++ Still a lot of issues with foliage condition for Os getting converted to NA.

#----- Tree Conditions
