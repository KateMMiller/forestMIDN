#------------------------
# Code for testing and retooling forest database
#------------------------
library(forestMIDN)
library(tidyverse)
#importData()
path = "C:/Forest_Health/exports/MIDN"
importCSV(path = path, zip_name = "MIDN_Forest_20210310.zip")
names(VIEWS_MIDN)
# microbenchmark::microbenchmark(importData(),
#                                importCSV("C:/Forest_Health/exports/NETN"),
#                                times = 1) #importCSV is 4+ times faster

# microbenchmark::microbenchmark(importData(),
#                                importData(server = "INPNETN-078644"),
#                                times = 1) #no difference; not surprising


#------------------------
# Plot and Visit data views
#------------------------
# Comparing full plots events join from views with arch db.
plot_events <- joinLocEvent(park = 'all', from = 2007, to = 2019, QAQC = TRUE, panels = 1:4,
                            locType = 'all', eventType = 'all', abandoned = T, output = 'verbose')

head(plot_events)

pe_testing <- joinLocEvent(park = "all")

pe_testing <- joinLocEvent(park = c("APCO"))
pe_testing <- joinLocEvent(park = c("APCO", "BOWA"))
table(pe_testing$ParkUnit)
pe_testing <- joinLocEvent(park = c("GETT", "FRSP"), from = 2015, to = 2021)
table(pe_testing$StartYear)
pe_testing <- joinLocEvent(QAQC = F)
table(pe_testing$IsQAQC)
pe_testing <- joinLocEvent(locType = "all")
table(pe_testing$PlotTypeCode)
pe_testing$Plot_Name[pe_testing$PlotTypeCode == 'Non-VS'] # none in MIDN. good.
pe_testing <- joinLocEvent(eventType = 'complete') #removes ACAD-029. good.
pe_testing <- joinLocEvent(eventType = 'all') #COLO-380-2018 treated properly
nrow(pe_testing)
pe_testing <- joinLocEvent(abandoned = TRUE)
table(pe_testing$IsAbandoned)

plot_events_old <- read.csv("./testing_scripts/plot_events.csv") %>% filter(Plot_Name != "PETE-185")

pe_merge <- merge(plot_events, plot_events_old, by.x = c("EventLegacyID", "Plot_Name"),
                   by.y = c("Event_ID", "Plot_Name"), all.x = T, all.y = T)
names(pe_merge)

# Function to check that the rows in each col. 1 and 2 are identical
# Will return 0 if none, or the values that differ

check_data <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", col1, col2)]}
    )) %>% bind_rows()
}

check_data(pe_merge, "ParkSubUnit", "Unit_ID")
check_data(pe_merge,"xCoordinate", "X_Coord")
check_data(pe_merge,"yCoordinate", "Y_Coord")
check_data(pe_merge,"StartDate", "Start_Date")
pe_merge$Start_Date <- strptime(pe_merge$Start_Date, format = "%Y-%m-%d")
pe_merge$StartDate <- strptime(pe_merge$StartDate, format = "%Y-%m-%d")
check_data(pe_merge,"PanelCode", "Panel")
check_data(pe_merge,"Event_QAQC", "IsQAQC")
test <- check_data(pe_merge,"ZoneCode", "UTM_Zone")
test
table(pe_merge$ZoneCode, pe_merge$UTM_Zone)
check_data(pe_merge,"Orientation.x", "Orientation.y")
check_data(pe_merge,"cycle.x", "cycle.y")
check_data(pe_merge,"PlotTypeCode", "Loc_Type")
check_data(pe_merge,"PlotLegacyID", "Location_ID")
check_data(pe_merge,"Aspect.x", "Aspect.y")
check_data(pe_merge,"PhysiographyCode", "Physiographic_Class")
plot_check <- unique(pe_merge[, c("ParkUnit", "Plot_Name")])
table(plot_check$ParkUnit) # numbers check out

dir_dif <- check_data(pe_merge,"Directions.x", "Directions.y")
write.csv(dir_dif, "./testing_scripts/Directions_check.csv") # Didn't find any issues to report
dir_dif
head(pe_merge)

pnote_diff <- check_data(pe_merge, "PlotNotes", "Loc_Notes")

write.csv(pnote_diff, "./testing_scripts/PNote_check.csv") # checks out
#---------------------------
#  Stand data views
#---------------------------
# Views haven't completely settled for this function, so I'm just going to compare raw
# view data to original function to find migration issues via setdiff and table checks
# ---- StandInfoPhotos
stand_old <- read.csv("./testing_scripts/Stand_Data.csv") %>% filter(Plot_Name != "PETE-185")
stand_new <- get("MIDN_StandInfoPhotos", envir = VIEWS_MIDN)[, -c(39:43, 46)]
stand_new$Plot_Name <- paste(stand_new$ParkUnit, sprintf("%03d", stand_new$PlotCode), sep = "-")
stand_new$Event_QAQC <- ifelse(stand_new$IsQAQC == 0, FALSE, TRUE)
stand_new <- stand_new %>% filter(Plot_Name != "PETE-185")
st_merge <- merge(stand_new, stand_old, by.x = c("Plot_Name", "StartYear", "Event_QAQC"),
                  by.y = c("Plot_Name", "Year", "Event_QAQC"),
                  all.x = T, all.y = T) %>% filter(StartYear < 2020)


table(complete.cases(st_merge$Event_ID)) #VAFO-9999 and COLO-380, OK
table(complete.cases(st_merge$Location_ID)) #VAFO-9999 and COLO-380, OK

check_data(st_merge, "ParkUnit", "Unit_Code")
check_data(st_merge,"Stand_Structure", "StandStructureSummary") # Mostly 2007; correctly labeled
check_data(st_merge,"Microtopography_ID", "MicrotopographyCode") # Mostly 2007; correctly labeled
check_data(st_merge,"Panel", "PanelCode")

table(st_merge$Crown_Closure_ID, st_merge$CrownClosureLabel)
table(st_merge$Event_QAQC, st_merge$IsQAQC)
table(st_merge$Stand_Structure, st_merge$StandStructureSummary)
head(st_merge)

#table(st_merge$Pct_Understory_Low, st_merge$)
#---- StandForestFloor
stand2_new <- get("COMN_StandForestFloor", envir = VIEWS_MIDN)[, -c(19:23)]
stand2_new$Plot_Name <- paste(stand2_new$ParkUnit, sprintf("%03d", stand2_new$PlotCode), sep = "-")
stand2_new <- stand2_new[, c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode",
                             "PlotTypeLabel", "PlotCode", "IsAbandoned", "PanelCode",
                             "PanelLabel", "StartDate", "IsQAQC", "StartYear",
                             "EventID", "PlotID",
                             "ForestFloorCode",
                             "ForestFloorLabel", "CoverClassLabel")]
names(stand2_new)
st_wide <- stand2_new %>% pivot_wider(id_cols = c(Plot_Name:PlotID), names_from = ForestFloorLabel,
                                      values_from = CoverClassLabel) %>%
           mutate(Event_QAQC = ifelse(IsQAQC == 0, FALSE, TRUE))
st_wide <- st_wide %>% rename_all(function(x) gsub(" ", "_", x))
st_wide <- st_wide %>% rename_all(function(x) gsub("-", "_", x))

names(st_wide)

st_merge2 <- merge(st_wide, stand_old, by.x = c("Plot_Name", "StartYear", "Event_QAQC"),
                   by.y = c("Plot_Name", "Year", "Event_QAQC"),
                  all.x = T, all.y = T) %>% filter(StartYear <2020)
table(complete.cases(st_merge2$Event_ID)) #7 all VAFO.9999, PETE-185, COLO-370
table(complete.cases(st_merge2$Location_ID))

table(st_merge2$Pct_Lichen_Cover, st_merge2$Lichen)
table(st_merge2$Pct_Bare_Soil_Cover, st_merge2$Bare_Soil)
table(st_merge2$Pct_Bryophyte_Cover, st_merge2$Non_Vascular)
table(st_merge2$Pct_Rock_Cover, st_merge2$Rock)
table(st_merge2$Pct_Surface_Water_Cover, st_merge2$Water)
table(st_merge2$Pct_Trampled_Cover, st_merge2$Trampled)


#---- StandPlantCoverStrata
names(VIEWS_MIDN$COMN_StandPlantCoverStrata)
stand3_new <- get("COMN_StandPlantCoverStrata", envir = VIEWS_MIDN)[, -c(19:23, 26)]
stand3_new$Plot_Name <- paste(stand3_new$ParkUnit, sprintf("%03d", stand3_new$PlotCode), sep = "-")
stand3_new$Event_QAQC <- ifelse(stand3_new$IsQAQC == 0, FALSE, TRUE)
names(stand3_new)

st_wide3 <- stand3_new %>% select(-StrataCode, -StrataSummary) %>%
  pivot_wider(id_cols = c(Network:StartYear, EventID:Event_QAQC),
              names_from = StrataLabel,
              values_from = CoverClassCode)

st_wide3 <- st_wide3 %>% rename_all(function(x) gsub(" ", "_", x))
st_wide3 <- st_wide3 %>% rename_all(function(x) gsub("-", "_", x))

names(st_wide3)
st_merge3 <- merge(stand_old, st_wide3, by.x = c("Plot_Name", "Year", "Event_QAQC"),
                  by.y = c("Plot_Name", "StartYear", "Event_QAQC"), all.x = T, all.y = T) %>%
  filter(Year < 2020)

table(st_merge3$Pct_Understory_Low, st_merge3$Ground)
table(st_merge3$Pct_Understory_Mid, st_merge3$Mid_understory)
table(st_merge3$Pct_Understory_High, st_merge3$High_understory)

#---- StandDisturbances
stand_dist <- get("COMN_StandDisturbances", envir = VIEWS_MIDN)[,-c(23:29)] %>% filter(StartYear <2020)
stand_dist$Plot_Name <- paste(stand_dist$ParkUnit, sprintf("%03d", stand_dist$PlotCode), sep = "-")
stand_dist$Event_QAQC <- ifelse(stand_dist$IsQAQC == 0, FALSE, TRUE)
head(stand_dist)

# Didn't summarize this in forestNETN yet, so have to do some importing/joining
library(RODBC)
db <-odbcConnect("MIDNFVM") #
disturb<-sqlFetch(db,"tbl_Disturbances")
disttlu<-sqlFetch(db,"tlu_Disturbance_Codes")
disttlutc<-sqlFetch(db,"tlu_Disturbance_Threshhold_Codes")
odbcClose(db)
head(disturb)

intersect(names(disturb), names(plot_events_old))
st_dist_o <- merge(plot_events_old[, c("Plot_Name", "Event_ID", "Event_QAQC", "Year")],
                   disturb[, c(2:6)], by = "Event_ID", all.x = T, all.y = T)
dist_2007_old <- st_dist_o %>% filter(Year == 2007) #75 plots sampled in 2007
ev2007 <- plot_events_old %>% filter(Year == 2007)

dist_2007_new <- stand_dist %>% filter(StartYear == 2007)
table(dist_2007_new$DisturbanceCode)
table(complete.cases(dist_2007_new$DisturbanceCode))
stand_dist <- stand_dist %>% select(Plot_Name, StartYear, everything())
table(complete.cases(stand_dist$DisturbanceCode), stand_dist$StartYear)

dist_miss_PM <- stand_dist[is.na(stand_dist$DisturbanceCode),]
write.csv(dist_miss_PM, "./testing_scripts/MIDN_Disturbances_missing_PM.csv")

dist_merge <- merge(stand_dist, st_dist_o, by.x = c("Plot_Name", "StartYear", "Event_QAQC", "DisturbanceCode"),
                    by.y = c("Plot_Name", "Year", "Event_QAQC", "Disturbance_Code"),
                     all.x = T, all.y = T) %>%
  filter(StartYear<2020)

blanks<-dist_merge[which(!complete.cases(dist_merge$DisturbanceCode)),]
blanks
#check_data(dist_merge, "Disturbance.Code", "Disturbance")
check_data(dist_merge, "Disturbance_Threshold", "ThresholdCode") %>% arrange(Plot_Name)
check_data(dist_merge, "Disturbance_Notes", "DisturbanceNote")
dist_simp <- dist_merge[,c("Plot_Name", "StartYear", "Event_QAQC",
                           "DisturbanceLabel", "DisturbanceCode",
                           "DisturbanceCoverClassCode",
                           "Disturbance_Cover_Class_ID")]
table(dist_merge$DisturbanceCoverClassCode, dist_merge$Disturbance_Cover_Class_ID)
# RICH-238-2009 duplicate stand disturbance

#---- StandSlopes
slopes <- get("COMN_StandSlopes", envir = VIEWS_MIDN) %>% filter(StartDate < 2020)
slopes$Plot_Name <- paste(slopes$ParkUnit, sprintf("%03d", slopes$PlotCode), sep = "-")
slopes$Event_QAQC <- ifelse(slopes$IsQAQC == 0, FALSE, TRUE)

db <-odbcConnect("MIDNFVM") #
stand_d<-sqlFetch(db,"tbl_Stand_Data")
odbcClose(db)

names(stand_d)
names(plot_events_old)
stand_df <- merge(plot_events_old[,c("Event_ID", "Plot_Name", "Year","Event_QAQC")],
                  stand_d[,c("Event_ID", "Derived_Plot_Slope", "Slope_UP",
                             "Slope_BR" ,"Slope_BL")], by = "Event_ID", all.x = T, all.y = F)
head(stand_df)
table(complete.cases(stand_df$Event_ID))
table(complete.cases(stand_df$Plot_Name))

sl_wide <- slopes %>% select(Plot_Name, Event_QAQC, StartYear,
                             PlotSlope, CWDSlope, TransectCode) %>%
  pivot_wider(id_cols = c(Plot_Name:PlotSlope),
              names_from = TransectCode,
              values_from = CWDSlope) %>% select(-"NA")

sl_simp <- slopes[,c(1:13, 17, 23, 26, 27)] %>% unique()

sl_merge1 <- merge(sl_simp, sl_wide,
                   by = intersect(names(sl_simp), names(sl_wide)),
                   all.x = T, all.y = T) %>% select(-`NA`)

sl_merge <- merge(sl_wide, stand_df, by.x = c("Plot_Name", "StartYear", "Event_QAQC"),
                  by.y = c("Plot_Name", "Year", "Event_QAQC"), all.x = T, all.y = T)

options(scipen = 100)
names(sl_merge)

der_slope <- check_data(sl_merge, "Derived_Plot_Slope", "PlotSlope") %>%
  mutate(diff = Derived_Plot_Slope - PlotSlope) %>%
  filter(abs(diff) > 0.5)# diff. in rounding

der_slope # no issues

# otherwise okay
check_data(sl_merge, "UP", "Slope_UP") %>% mutate(diff = UP - Slope_UP) %>% filter(abs(diff)>0.1)
check_data(sl_merge, "BR", "Slope_BR") %>% mutate(diff = BR - Slope_BR) %>% filter(abs(diff)>0.1)
check_data(sl_merge, "BL", "Slope_BL") %>% mutate(diff = BL - Slope_BL) %>% filter(abs(diff)>0.1)

#-----StandTreeHeights
# This is not going to be fun...
tr_ht <- stand_d %>% select(Event_ID, Stand_Structure_ID, Stunted_Woodland,
                            Tree_1_Number_Codom:Height_Tree_3_Inter) %>%
           filter(Stand_Structure_ID != 5) %>% filter(Stunted_Woodland == 0)

tr_ht2 <- merge(plot_events_old[,c("Event_ID", "Plot_Name", "Year", "Event_QAQC")],
                tr_ht, by = "Event_ID", all.x = FALSE, all.y = TRUE) %>% filter(!is.na(Plot_Name))

table(complete.cases(tr_ht2$Plot_Name))

tr_ht_w1 <- tr_ht2 %>% select(Event_ID:Tree_3_Number_Inter) %>%
  pivot_longer(cols = c(Tree_1_Number_Codom:Tree_3_Number_Inter),
               names_to = "Samp",
               values_to = "Tree_Number") %>%
  mutate(Samp_Num = case_when(str_detect(Samp, "_1_") ~ 1L,
                              str_detect(Samp, "_2_") ~ 2L,
                              str_detect(Samp, "_3_") ~ 3L),
         Crown = ifelse(str_detect(Samp, "Codom"), "Codom", "Inter")
)


tr_ht_w2 <- tr_ht2 %>% select(Event_ID:Event_QAQC, Height_Tree_1_Codom:Height_Tree_3_Inter) %>%
  pivot_longer(cols = c(Height_Tree_1_Codom:Height_Tree_3_Inter),
               names_to = "Samp_ht",
               values_to = "Height_m") %>%
  mutate(Samp_Num = case_when(str_detect(Samp_ht, "_1_") ~ 1L,
                              str_detect(Samp_ht, "_2_") ~ 2L,
                              str_detect(Samp_ht, "_3_") ~ 3L),
         Crown = ifelse(str_detect(Samp_ht, "Codom"), "Codom", "Inter")
  )

tr_ht3 <- merge(tr_ht_w1, tr_ht_w2, by = c("Event_ID", "Plot_Name", "Year", "Event_QAQC", "Samp_Num", "Crown"),
                all.x = T, all.y = T) %>% select(-Samp, -Samp_ht) %>% filter(!is.na(Height_m))
head(tr_ht3)
# tr_ht3 is ready to compare with the view
tr_ht_vw <- get("COMN_StandTreeHeights", envir = VIEWS_MIDN)[, -c(18:24, 27)]
tr_ht_vw$Plot_Name <- paste(tr_ht_vw$ParkUnit, sprintf("%03d", tr_ht_vw$PlotCode), sep = "-")
tr_ht_vw$Event_QAQC <- ifelse(tr_ht_vw$IsQAQC == 0, FALSE, TRUE)
names(tr_ht3)
names(tr_ht_vw)
tree_height_comps <- merge(tr_ht_vw,
                           tr_ht3,
                           by.x = c("Plot_Name", "StartYear", "Event_QAQC", "TagCode"),
                           by.y = c("Plot_Name", "Year", "Event_QAQC", "Tree_Number"),
                     all.x = F, all.y = F) %>%
  filter(StartYear < 2020)

table(complete.cases(tree_height_comps$Height),
        tree_height_comps$StartYear) # 2008-2011 are missing stand heights

miss_heights <- tree_height_comps[which(is.na(tree_height_comps$Height)),]
miss_heights
write.csv(miss_heights, "./testing_scripts/MIDN_missing_tree_heights.csv", row.names = F)

trht_comps <- tree_height_comps %>% filter(StartYear > 2010)

check_data(trht_comps, "Height", "Height_m") %>%
  mutate(diff = Height - Height_m) %>% filter(abs(diff)>0.1)

table(trht_comps$Crown, trht_comps$CrownClassLabel) #all good

#---------------------------
#  CWD views
#---------------------------
# library(forestMIDNarch)
# forestMIDNarch::importData()
# cwd_old <- forestMIDNarch::joinCWDData()
# write.csv(cwd_old, "D:/NETN/R_Dev/forestMIDN/testing_scripts/cwd_old.csv", row.names = F)
cwd_old <- read.csv("./testing_scripts/CWD_old.csv")
plot_events_old <- read.csv("./testing_scripts/plot_events.csv")

library(RODBC)
db <-odbcConnect("MIDNFVM") #
cwd <- sqlFetch(db,"tbl_CWD_Transect_Data")
odbcClose(db)
head(cwd)

cwd_old_check <- cwd %>% select(Event_ID, Transect) %>%
  group_by(Event_ID) %>% summarize(num_trans = length(unique(Transect))) %>%
  filter(num_trans < 3) %>%
  left_join(., plot_events_old, by = "Event_ID")
# no visits missing a transect

cwd_vol <- joinCWDData() %>% mutate(Event_QAQC = ifelse(IsQAQC == 1, TRUE, FALSE))

head(cwd_vol)
table(cwd_vol$ParkUnit)
cwd_vol1 <- joinCWDData(park = c("APCO", "GETT"))
cwd_vol2 <- joinCWDData(park = c("SAHI", "RICH"), from = 2007, to = 2011,
                        locType = "all", output = 'verbose')
cwd_vol3 <- joinCWDData(park = 'all', panels = c(1,3), QAQC = TRUE)
cwd_vol4 <- joinCWDData(park = 'all', QAQC = TRUE, units = 'acres')

head(cwd_vol4)

table(cwd_vol2$ParkUnit)
names(cwd_vol)
names(cwd_old)

cwd_new_calc <- cwd_vol %>% group_by(ParkUnit, StartYear, IsQAQC) %>%
  summarize(num_plots = sum(!is.na(Plot_Name)),
            sum_CWD = sum(CWD_Vol),
            mean_CWD = mean(CWD_Vol))

cwd_old_calc <- cwd_old %>%
  group_by(Unit_Code, Year, Event_QAQC) %>%
  summarize(num_plots = sum(!is.na(Plot_Name)),
            sum_CWD = sum(CWD_Vol, na.rm = T),
            mean_CWD = mean(CWD_Vol, na.rm = T)) %>%
  mutate(IsQAQC = ifelse(Event_QAQC == TRUE, 1, 0))

options(scipen = 100)
cwd_merge1 <- merge(cwd_new_calc, cwd_old_calc,
                    by.x = c("ParkUnit", "StartYear", "IsQAQC"),
                    by.y = c("Unit_Code", "Year", "IsQAQC"),
                    all.x = T, all.y = T) %>%
  mutate(diff_plots = num_plots.x - num_plots.y,
         diff_sum = sum_CWD.x - sum_CWD.y,
         diff_mean = mean_CWD.x - mean_CWD.y) %>%
  filter(abs(diff_plots) > 0 | abs(diff_sum) > 0.5 | abs(diff_mean) > 0.5)
# There's 1 plot diff in FRSP in 2007: FRSP-054-2007 BR transect needs no spp added
head(cwd_vol)
head(cwd_old)

cwd_merge <- merge(cwd_old, cwd_vol,
                   by.x = c("Plot_Name", "Year", "Event_QAQC", "Latin_Name", "Decay_Class_ID"),
                   by.y = c("Plot_Name", "StartYear", "Event_QAQC", "ScientificName", "DecayClassCode"),
                   all.x = TRUE, all.y = TRUE) %>% filter(Unit_Code == "FRSP" & Year == 2007)

cwd_check <- cwd_merge %>% mutate(diff = CWD_Vol.x - CWD_Vol.y) %>%
  filter(abs(diff) > 0.1) %>%
  select(Plot_Name, Year, IsQAQC, Latin_Name, Decay_Class_ID, CWD_Vol.x, CWD_Vol.y, diff) %>%
  filter(IsQAQC == FALSE) # Original function didn't pull in cwd slopes from first visit, and
# calculated volume assuming 0 slope, which was wrong. We never used the function for QAQC visits
# so it didn't introduce any issues. Otherwise, the only 2 plots with diff. b/t old and new are
# missing a NP in 1 transect, so denominator is diff. b/t old and new.


