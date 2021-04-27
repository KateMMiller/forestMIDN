#------------------------
# Code for testing and retooling forest database
#------------------------
library(forestMIDN)
library(tidyverse)
#importData()
path = "C:/Forest_Health/exports/MIDN"
#exportCSV(path = "C:/Forest_Health/exports/MIDN", zip = T)
#importCSV(path = path, zip_name = "MIDN_Forest_20210406.zip") # Release from 1.0.21
importCSV(path = path, zip_name = "MIDN_Forest_20210409.zip") # includes the soil views

#importData(instance = 'local', server = "localhost", new_env = T) # release 1.0.21 with IsGerminant added to NETNquadspp

# microbenchmark::microbenchmark(importData(),
#                                importCSV("C:/Forest_Health/exports/midn"),
#                                times = 1) #importCSV is 4+ times faster

# microbenchmark::microbenchmark(importData(),
#                                importData(server = "INPmidn-078644"),
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
pe_merge$Start_Date <- strptime(pe_merge$Start_Date, format = "%Y-%m-%d")
pe_merge$StartDate <- strptime(pe_merge$StartDate, format = "%Y-%m-%d")
check_data(pe_merge,"StartDate", "Start_Date")
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
pnote_diff #UTF issues, but directions seem correct
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

# Didn't summarize this in forestmidn yet, so have to do some importing/joining
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
                   all.x = T, all.y = T)# %>% select(-`NA`)

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
                     all.x = T, all.y = T) %>%
  filter(StartYear < 2020)

table(complete.cases(tree_height_comps$Height),
        tree_height_comps$StartYear) # 2008-2011 are missing stand heights

miss_heights <- tree_height_comps[which(is.na(tree_height_comps$Height)),]
miss_heights
write.csv(miss_heights, "./testing_scripts/MIDN_missing_tree_heights_indiv.csv", row.names = F)

trht_comps <- tree_height_comps %>% filter(StartYear > 2010)

check_data(trht_comps, "Height", "Height_m") %>%
  mutate(diff = Height - Height_m) %>% filter(abs(diff)>0.1)

table(trht_comps$Crown, trht_comps$CrownClassLabel) #all good

#----- joinStandData -----
names(VIEWS_MIDN)
# stand views: midn_StandInfoPhotos, COMN_StandPlantCoverStrata, COMN_StandDisturbances,
# COMN_StandForestFloor, COMN_StandSlopes, COMN_StandTreeHeights
#forestMIDNarch::importData()
stand_old <- forestMIDNarch::joinStandData(from = 2007, to = 2019, QAQC = T)
head(stand_old)
stand_old2 <- merge(stand_old, stand[,c("Event_ID", "Deer_Browse_Line_pre09_ID")],
                    by = "Event_ID", all.x = T, all.y = T)


stand_new <- joinStandData(park = 'all', from = 2007, to = 2019, output = 'short', QAQC = TRUE)
test <- joinStandData(park = c("APCO", "BOWA"), from = 2015, to = 2016, output = 'verbose', QAQC = F)
test <- joinStandData(park = "DFDS")
head(test)

table(complete.cases(stand_new[,c(1:6)])) # all TRUE
table(stand_new$ParkUnit, stand_new$StartYear)

stand_check <- merge(stand_new,
                     stand_old, all.x = T, all.y = T,
                     by.x = c("Plot_Name", "StartYear", "IsQAQC"),
                     by.y = c("Plot_Name", "Year", "Event_QAQC"))
names(stand_check)

table(stand_check$Stand_Structure.x, stand_check$Stand_Structure.y)
check_data(stand_check, "Microtopography", "Microtopography_ID") # no returns
check_data(stand_check, "Deer_Browse_Index", "Deer_Browse_Line_ID") #missing <2009
table(stand_check$Deer_Browse_Index, stand_check$Deer_Browse_Line_ID)
check_data(stand_check, "Pct_Understory_Low.x", "Pct_Understory_Low.y")# no returns
check_data(stand_check, "Pct_Understory_Mid.x", "Pct_Understory_Mid.y")# no returns
check_data(stand_check, "Pct_Understory_High.x", "Pct_Understory_High.y")# no returns
check_data(stand_check, "Pct_Bare_Soil", "Pct_Bare_Soil_Cover")# no returns
check_data(stand_check, "Pct_Bryophyte", "Pct_Bryophyte_Cover")# no returns
check_data(stand_check, "Pct_Rock", "Pct_Rock_Cover")# no returns
check_data(stand_check, "Pct_Water", "Pct_Surface_Water_Cover")# no returns
check_data(stand_check, "Pct_Trampled", "Pct_Trampled_Cover")#no returns
check_data(stand_check, "Pct_Crown_Closure.x", "Pct_Crown_Closure.y")# no returns

head(stand_check)
stand_ht_check <-stand_check %>% mutate(diff_codom = abs(Avg_Codom_HT - Avg_height_Codom),
                       diff_int = abs(Avg_Inter_HT - Avg_height_Inter)) %>%
                filter(diff_codom > 0.1 | diff_int > 0.1) %>%
                select(Plot_Name, StartYear, IsQAQC, Avg_Codom_HT, Avg_height_Codom,
                       Avg_Inter_HT, Avg_height_Inter, diff_codom, diff_int) %>%
  arrange(StartYear, Plot_Name)
nrow(stand_ht_check)
write.csv(stand_ht_check, "./testing_scripts/MIDN_missing_tree_heights.csv")

# there are a lot of records where a tree was probably dropped in the migration
# Most were because of tag changes that Mark has fixed. But will want to
# check after the next migration.

#---------------------------
#  CWD views
#---------------------------
# library(forestMIDNarch)
# forestMIDNarch::importData()
# cwd_old <- forestMIDNarch::joinCWDData()
# write.csv(cwd_old, "D:/midn/R_Dev/forestMIDN/testing_scripts/cwd_old.csv", row.names = F)
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

#---------------------------
# Tree Data
#---------------------------
library(tidyverse)
library(forestMIDN)
forestMIDNarch::importData()

# library(RODBC)
# db <-odbcConnect("midnFVM") #
# plants <- sqlFetch(db,"tlu_Plants")
# odbcClose(db)

midn_tree <- forestMIDNarch::joinTreeData(from = 2006, to = 2019, locType = 'all', QAQC = TRUE) %>%
  mutate(TagCode = as.numeric(Tree_Number_MIDN)) %>% filter(Plot_Name != "PETE-185")

#++++ AFTER MIGRATION RERUN CHECKS WITH THIS tree_new INSTEAD OF THE ONE GENERATED BY THE FUNCTION
# tree_new <- VIEWS_midn$COMN_TreesByEvent %>%
#   select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, TreeLegacyID,
#          TagCode, TaxonID, TSN, ScientificName, Fork, Azimuth, Distance, DBHcm, IsDBHVerified,
#          IsDBHUnusual, TreeStatusCode, TreeStatusLabel, CrownClassCode, CrownClassLabel,
#          DecayClassCode, HWACode, BBDCode, TreeEventNote) %>%
#   filter(StartYear < 2020)

#Subsets and Left joins early makes things a lot faster!
microbenchmark::microbenchmark(
  tree_new <- joinTreeData(from = 2007, to = 2019, locType = 'all', eventType = 'complete', QAQC = TRUE,
                           output = 'verbose'),
  tree_new2 <- joinTreeData(from = 2019, to = 2019, locType = 'all', eventType = 'complete', QAQC = TRUE,
                            output = 'verbose'), times =1)

head(tree_new)

tree_new <- joinTreeData(from = 2007, to = 2019, locType = 'all', eventType = 'complete', QAQC = TRUE,
                         output = 'verbose', status = 'all')

head(joinTreeData(park = c("VAFO", "THST")))
head(joinTreeData(park = c("RICH", "SAHI"), from = 2007, to = 2010))
head(joinTreeData(park = 'all', status = 'active'))

names(tree_new)

tree_newna <- tree_new[is.na(tree_new$DBHcm) & !(tree_new$TreeStatusCode %in% c("DF", "DC")),
                       c(1:3,6,7,16,17,19,21:25,27:45)]

table(tree_new$TreeStatusCode)
table(midn_tree$Status_ID) # codes 2, DS, EX, NL, XP are slightly different. Need to figure out why

# intermediate step in function
# tree_taxa <- merge(tree_vw,
#                    taxa[,c('TSN','ScientificName','CommonName','Family', 'Genus', 'IsExotic')],
#                    by = c("TSN", "ScientificName"), all.x = TRUE, all.y = FALSE) %>% filter(StartYear < 2020)
#
# tree_taxa$Plot_Name <- paste(tree_taxa$ParkUnit, str_pad(tree_taxa$PlotCode, 3, side = 'left', "0"), sep = "-")
# intersect(names(trees), names(treedata))
# names(trees)
# names(treedata)
tree_old <- merge(trees[,1:15], treedata[,1:16], by = "Tree_ID", all.x = T, all.y = T)

plot_events_old <- forestMIDNarch::joinLocEvent(from = 2007, to = 2019, QAQC = T, locType = 'all') %>%
                   filter(Plot_Name != "PETE-185")

#------------
tree_old2 <- merge(plot_events_old, tree_old, by = c("Event_ID", "Location_ID"), all.x = T, all.y = F)
tree_old2$TagCode <- as.numeric(tree_old2$Tree_Number_MIDN)
head(tree_old2)

unique(tree_old2$Event_ID[tree_old2$Plot_Name == "GETT-088"]) #Already removed

tree_merge <- merge(tree_new, tree_old2,
                    by.x = c("Plot_Name", "StartYear", "IsQAQC", "TagCode"),
                    by.y = c("Plot_Name", "Year", "Event_QAQC", "TagCode"),
                    all.x = T, all.y = T)

check_trees <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "TagCode", "IsQAQC", "ScientificName",
              "Status_ID", "TreeStatusCode", "DBHcm", "DBH", col1, col2)]}
  )) %>% bind_rows()
}

options(scipen = 100)
tree_check <- tree_merge %>% select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName,
                                    Status_ID, TreeStatusCode, TSN.x, TSN.y)
# Looks like saplings are in the old tree data, but not in the new tree data?
names(VIEWS_MIDN$COMN_TreesByEvent)
names(VIEWS_MIDN$MIDN_MicroplotSaplings)
tree_tag <- VIEWS_MIDN$COMN_TreesByEvent %>% select(ParkUnit, PlotCode, StartYear, IsQAQC,
                                                    TagCode, TreeStatusCode, ScientificName, DBHcm)
sap_tag <- VIEWS_MIDN$MIDN_MicroplotSaplings %>% select(ParkUnit, PlotCode, StartYear, IsQAQC,
                                                        TagCode, SaplingStatusCode, ScientificName, DBHcm)
head(tree_tag)
head(sap_tag)
head(tree_merge)
summary(tree_merge$DBHcm)

not_matched <- anti_join(tree_tag, sap_tag, by = c("ParkUnit", "PlotCode", "TagCode", "StartYear", "IsQAQC"))
matched <- inner_join(tree_tag, sap_tag, by = c("ParkUnit", "PlotCode" ,"TagCode", "StartYear", "IsQAQC"))

# The old tree join didn't include the fork column. Need to add that for checking
check_tsn <- check_trees(tree_merge, "TSN.x", "TSN.y") %>% filter(Status_ID != "EX") #Lots of mismatches b/c saplings?
write.csv(check_tsn, "./testing_scripts/MIDN_Trees_missing_Dist_Azi_that_got_excluded.csv")
length(unique(check_tsn$TagCode)) # 14 trees that are dropped from migration b/c they weren't mapped in 2007.
# They were DF by the time they were sampled in 2011, so weren't mapped again.

check_trees(tree_merge, "Azimuth.x", "Azimuth.y") #SARA-015-2012
check_trees(tree_merge, "Fork.x", "Fork.y")

names(tree_merge)
#check_data(tree_merge, "Distance.x", "Distance.y")
tree_dist <- tree_merge %>% mutate(dist_diff = abs(Distance.x - Distance.y)) %>%
  filter(dist_diff > 0.1) # 0 records
tree_dist

tree_dbh <- tree_merge %>% mutate(dbh_diff = abs(DBH - DBHcm)) %>%
  filter(dbh_diff > 0) %>%
  select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName, DBHcm, DBH, dbh_diff)
tree_dbh
# Documented DBHs that need to be fixed so migration rounding doesn't round up.

names(tree_merge)
status_check <- check_trees(tree_merge, "TreeStatusCode", "Status_ID")
status_check2 <- tree_merge %>% select(Plot_Name, StartYear, IsQAQC, TagCode,
                                       ScientificName, Status_ID, TreeStatusCode)
# Returns the trees without dist/az that were dropped in the migration
# should return 0 records once that's fixed
status_check2
names(tree_merge)

crown_check <- check_trees(tree_merge, "Crown_Class_ID", "CrownClassCode")
crown_check
# Migration is properly deleted Crown Class from dead trees in MIDN (not in NETN)
# And is missing trees dropped in migration. Should be fine after that's fixed

table(tree_new$TreeStatusCode, tree_new$StartYear)
table(tree_new$CrownClassCode, tree_new$TreeStatusCode)

table(tree_merge$TreeStatusCode, tree_merge$CrownClassLabel, useNA = "always")
# Live trees missing CrownClass should be PM, not NC (26 records)


decay_check<-check_trees(tree_merge, "Decay_Class_ID", "DecayClassCode")
decay_check
# There are 2 trees with live status and decay class entered.
# Migration is correctly dropping the decay class

hwa_check <- check_trees(tree_merge, "HWACode", "HWA_Status")
hwa_check
# The trees dropped b/c dist/az NA are not the only ones being returned. All OK.

table(tree_old2$HWA_Status, tree_old2$Year, useNA = 'always')
# Cycle one not treated correctly
hwa_check <- tree_merge %>% filter(#ScientificName == "Tsuga canadensis" &
  is.na(HWACode))
table(tree_merge$ScientificName, tree_merge$HWALabel, useNA = 'always')
# TSUCAN Not Applicables are all on dead trees
#The TreeEventHWA_Insert.sql script logic isn't quite right.
#The start date in the CASE WHEN should be 1/1/2010 instead of 1/1/2009 (will change 48 PMs in 2009 to NC).
#There are also a 46 records of HWACodes that are NA in 2010 and 2 for 2011 for non-hemlock trees.
#I think these are supposed to be 0 instead of NA, based on the rest of the data in the xref.

tree10 <- tree_merge %>% filter(StartYear == 2010)
table(tree10$ScientificName, tree10$HWACode, useNA = 'always')
tree11 <- tree_merge %>% filter(StartYear == 2011)
table(tree11$ScientificName, tree11$HWACode, useNA = 'always')
#8+37+1+2 #48
#The TreeEventHWA_Insert.sql script logic isn't quite right. The start date in the
#CASE WHEN should be 1/1/2010 instead of 1/1/2009 (will change 48 PMs in 2009 to NC).
#There are also a 46 records of HWACodes that are NA in 2010 and 2 for 2011 for non-hemlock trees.
#I think these are supposed to be 0 instead of NA, based on the rest of the data in the xref.

check_trees(tree_merge, "BBDCode", "BBD_Status")
# BBD is migrating correctly; just showing the dist/az trees that were dropped in migration

names(tree_merge)
dbh_ver <- check_trees(tree_merge, "IsDBHVerified", "DBH_Verified")
dbh_ver
table(tree_merge$IsDBHVerified, tree_merge$TreeStatusCode, tree_merge$StartYear, useNA = 'always')
# differences in whether 0 or NA are for dead or excluded trees. Looks okay.

names(VIEWS_midn$COMN_TreesFoliageCond)

# totfol <- VIEWS_midn$COMN_TreesFoliageCond %>%
#   mutate(Plot_Name = paste(ParkUnit, str_pad(PlotCode, 3, side = 'left', "0"), sep = "-")) %>%
#   select(Plot_Name, StartYear, IsQAQC, TagCode, TotalFoliageCondition.Label) %>% unique()

# head(totfol)
# head(tree_merge)
# tree_merge2 <- merge(tree_merge, totfol, by = c("Plot_Name", "StartYear", "IsQAQC", "TagCode"),
#                      all.x = T, all.y = T)
# head(tree_merge2)
table(tree_merge$Txt_Tot_Foliage_Cond, tree_merge$Total_Foliage_Condition, useNA = 'always')

fol_check <- tree_merge %>% select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode,
                                   Status_ID, Txt_Tot_Foliage_Cond, Total_Foliage_Condition)
# Lots of NC where should be NO or NULL (dead). Camilla captured this in the legacy tracker. Will check
# this again after Stephen makes the changes.

#----------------------
# Done checking records that only have 1/tree/visit. Now to check foliage and tree conditions

#---------------------
# Foliage Conditions
#---------------------

fol_cond <- joinTreeFoliageCond(valueType = 'classes')

fol_test <- joinTreeFoliageCond(park = "GETT", from = 2016, to = 2019, speciesType = 'native',
                                valueType = 'classes')
head(fol_test)

# Will wait to test the actual values until after Stephen's fixes

#-------------------------
# Tree Conditions
#-------------------------

# MIDN correct condition counts
#    H	AD	BBD	CAVL CAVS	 CW	   DBT	EAB	EB	   G	HWA	ID	NO	  OTH	SPB	VIN	  VOB
#13828	968	29	297	 279	2979	2271	9	  3123	172	3	  58	1257	21	8	  1453	189

tree_cond <- joinTreeConditions(park = 'all', from = 2007, to = 2019, QAQC = T,
                                locType = 'all', panels = 1:4, status = 'all')
head(tree_cond)

tree_cond <- joinTreeConditions(park = 'all', from = 2016, to = 2019, QAQC = T,
                                locType = 'all', panels = 1:4, status = 'live')


cond_dead <- joinTreeConditions(status = 'dead')
cond_exo <- joinTreeConditions(speciesType = 'exotic')
table(cond_dead$TreeStatusCode, useNA = "always")

#------------------
# Vines
#------------------

vines <- joinTreeVineSpecies()
vine_test <- joinTreeVineSpecies(park = 'APCO')
vine_test <- joinTreeVineSpecies(from = 2015, to = 2015)
vine_test <- joinTreeVineSpecies(from = 2007, to = 2019, speciesType = 'exotic')


#-----------------
# Quadrat Character
#-----------------
library(forestMIDN)
library(tidyverse)
#importData()
park = 'all'
from = 2007
to = 2019
QAQC = T
locType = 'all'
eventType = 'all'
panels = 1:4
speciesType = 'all'

path = "C:/Forest_Health/exports/MIDN"
importCSV(path = path, zip_name = "MIDN_Forest_20210322.zip") # Release from 1.0.21
forestMIDNarch::importData()

qchar_new <- joinQuadData(park = 'all', from = 2007, to = 2019, locType = 'all', eventType = 'all',
                           valueType = 'all', QAQC = T)

qchar_new %>% filter(is.na(CharacterLabel) | num_quads < 12) %>%
  select(Plot_Name, StartYear, IsQAQC, CharacterLabel, num_quads) %>% arrange(Plot_Name, StartYear)

# locev <- merge(loc, event, by = "Location_ID", all.x = T, all.y = T)
#
# #drops <- anti_join(locev, plotevs_old, by = "Event_ID")

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

qchar_old <- qchar_old %>% mutate(Cover_Type = case_when(Quadrat_ID == 2 ~ "Soil",
                                                         Quadrat_ID == 3 ~ "Rock",
                                                         Quadrat_ID == 4 ~ "Stem",
                                                         Quadrat_ID == 5 ~ "Wood",
                                                         Quadrat_ID == 6 ~ "Sphagnum",
                                                         Quadrat_ID == 7 ~ "NonSphagnum",
                                                         Quadrat_ID == 8 ~ "Lichens",
                                                         Quadrat_ID == 9 ~ "Herbs")) %>% # 9 = Herbs MIDN
  select(Plot_Name, Year, Event_QAQC, Cover_Type, A2, A5, A8, AA, B2, B5, B8, BB, C2, C5, C8, CC)

incomplete_old <- qchar_old[which(!complete.cases(qchar_old)),]
# Plots with at least one quad missing quadrat data:
    # SQs should be NS for: COLO-380-2018 All- needs fixing;
          # RICH-063-2011 AA, B2, B5, B8 (NS correct for data/quads, missing SQ for seedlings);
          # RICH-073-2015 B5 & B8 (NS correct for data/quads, missing SQ for seedlings);
    # PMs should replace blanks for: APCO-184-2009 A8 Herbs; FRSP-106-2008 A2 Herbs.


head(qchar_old)
table(qchar_old$Cover_Type, useNA = 'always') # after dropping SARA.915, matches perfect
table(qchar_new$CharacterLabel, useNA = 'always')
names(qchar_old)
names(qchar_new)

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

check_qchr(quadchr_merge, "Pct_Cov_A2", "A2") # FRSP-106-2008 A2 Herbs should be NA
check_qchr(quadchr_merge, "Pct_Cov_UR", "UR") # all good
check_qchr(quadchr_merge, "Pct_Cov_MR", "MR") # all good
check_qchr(quadchr_merge, "Pct_Cov_BR", "BR") # all good
check_qchr(quadchr_merge, "Pct_Cov_BC", "BC") # all good
check_qchr(quadchr_merge, "Pct_Cov_BL", "BL") # all good
check_qchr(quadchr_merge, "Pct_Cov_UL", "UL") # all good

# Quadrat character data checks out

#------------------------------
# benchmark locevent b/c so many functions use it
#------------------------------

microbenchmark::microbenchmark(
  joinLocEvent(),
  joinLocEvent(park = 'GEWA', from = 2019, to = 2019),
  times = 5
)

#-------------------------
# Quadrat Species
#-------------------------
forestMIDNarch::importData()
park = 'all'
from = 2007
to = 2019
QAQC = T
locType = 'all'
eventType = 'all'
panels = 1:4
#status = 'all'
# status = 'active'
# status = 'dead'
status = 'live'
speciesType = 'all'
dist_m <- NA
microbenchmark::microbenchmark(
  quadspp_old <- forestMIDNarch::joinQuadData(from = 2007, to = 2019, QAQC = T, eventType = "all", locType = "all"),
  quadspp_new <- joinQuadSpecies(from = 2007, to = 2019, QAQC = T, eventType = 'all'),
  times = 5
) # despite more cumbersome dataset, new function is faster.

quadspp_new <- joinQuadSpecies(from = 2007, to = 2019, QAQC = T, eventType = 'all', locType = 'all', valueType = 'midpoint')
nrow(quadspp_new)
nrow(quadspp_old)

quadspp_test <- joinQuadSpecies(park = 'APCO', from = 2007, to = 2019, QAQC = T, eventType = 'all')
quadspp_test <- joinQuadSpecies(park = 'BOWA', from = 2007, to = 2019, speciesType = 'invasive')
quadspp_test <- joinQuadSpecies(park = 'FRSP', speciesType= 'native')
quadspp_test <- joinQuadSpecies(park = 'FRSP', speciesType= 'invasive')
quadspp_test <- joinQuadSpecies(park = 'VAFO', speciesType= 'native')
nrow(quadspp_test)
head(quadspp_old)
head(quadspp_new)

# I changed how I'm handling germinants, so have to drop for now
quadspp_old2 <- quadspp_old %>% #filter(!(avg.cover == 0 & germ.cover > 0)) %>%
  filter(Latin_Name != "No species")
str(quadspp_new)
str(quadspp_old)
quadspp_old2$Year_fix <- as.numeric(quadspp_old2$Year)

quadspp_merge <- full_join(quadspp_new ,
                           quadspp_old2, by = c("Plot_Name" = "Plot_Name",
                                                "StartYear" = "Year_fix",
                                                "IsQAQC" = "Event_QAQC",
                                                "TSN" = "TSN")) %>%
  select(Plot_Name, PlotID, EventID, StartYear, cycle.x, cycle.y, IsQAQC,
          Confidence, ScientificName,TSN, Latin_Name,
         num_quads, quad_avg_cov,
         quad_pct_freq, avg.cover, avg.freq, Pct_Cov_A2:Pct_Cov_CC, A2:CC)

head(quadspp_merge)
names(quadspp_merge)

check_qspp <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "StartYear", "IsQAQC", "ScientificName", "Latin_Name",
              col1, col2)]}
  )) %>% bind_rows()
}

quadspp_cov <- quadspp_merge %>% mutate(cov_diff = quad_avg_cov - avg.cover) %>%
  filter(cov_diff > 0.01) %>%
  select(PlotID, EventID, Plot_Name, StartYear, IsQAQC, TSN, ScientificName, num_quads,
         quad_avg_cov, avg.cover, quad_pct_freq, avg.freq, cov_diff)

quadfreq_cov <- quadspp_merge %>% mutate(freq_diff = quad_pct_freq - 100*(avg.freq)) %>%
  filter(freq_diff > 0.01) %>%
  select(PlotID, EventID, Plot_Name, StartYear, IsQAQC, TSN, ScientificName, num_quads,
         quad_avg_cov, avg.cover, quad_pct_freq, avg.freq, freq_diff)

# Unknown Spp ##s got swapped occassionally in the migration
# see MIDN_quad_spp_mismatch.xlsx for list of affected plots
new_quads <- c("Pct_Cov_A2", "Pct_Cov_A5", "Pct_Cov_A8", "Pct_Cov_AA",
               "Pct_Cov_B2", "Pct_Cov_B5", "Pct_Cov_B8", "Pct_Cov_BB",
               "Pct_Cov_C2", "Pct_Cov_C5", "Pct_Cov_C8", "Pct_Cov_CC")

quadspp_merge[new_quads][quadspp_merge[new_quads] > 0] <- 1

check_qspp(quadspp_merge, "Pct_Cov_A2", "A2") # something is weird with RICH-063-2011 cyperaceae A2

check_qspp(quadspp_merge, "Pct_Cov_A5", "A5") %>% filter(ScientificName != "None present") %>%
  filter(Latin_Name != "no species recorded") # RICH-073 since not turned off in old

check_qspp(quadspp_merge, "Pct_Cov_A8", "A8") # RICH-073 & 63 since not turned off in old
check_qspp(quadspp_merge, "Pct_Cov_AA", "AA") # RICH-073 & 63 since not turned off in old
check_qspp(quadspp_merge, "Pct_Cov_B2", "B2") # RICH-073 & 63 since not turned off in old
check_qspp(quadspp_merge, "Pct_Cov_B5", "B5") # RICH-073 & 63 since not turned off in old
check_qspp(quadspp_merge, "Pct_Cov_B8", "B8") # RICH-073 & 63 since not turned off in old
check_qspp(quadspp_merge, "Pct_Cov_BB", "BB") # VAFO-161-2009 b/c no species
check_qspp(quadspp_merge, "Pct_Cov_C2", "C2") # VAFO-161-2009 b/c no species
check_qspp(quadspp_merge, "Pct_Cov_C5", "C5") # VAFO-161-2009 b/c no species
check_qspp(quadspp_merge, "Pct_Cov_C8", "C8") # VAFO-161-2009 b/c no species
check_qspp(quadspp_merge, "Pct_Cov_CC", "CC") # VAFO-161-2009 b/c no species

check_qspp(quadspp_merge, "ScientificName", "Latin_Name")

#----------------------
# Quad Notes
#----------------------

quad_notes <- joinQuadNotes()

#------------------------
# Microplot- Shrubs
#------------------------
forestMIDNarch::importData()
shrub_old <- forestNETNarch::joinMicroShrubData(from = 2007, to = 2019, locType = 'all', eventType = 'all', QAQC = T)
shrub_old$Latin_Name[shrub_old$Latin_Name == "No species recorded"] <- "None present"
shrub_new <- joinMicroShrubData(from = 2007, to = 2019, locType = 'all', eventType = 'all', QAQC = T)

length(unique(shrub_new$Plot_Name)) #375
shrub_exo <- joinMicroShrubData(speciesType = 'exotic')
length(unique(shrub_exo$Plot_Name)) #375


sort(unique(shrub_new$ParkUnit))
shrub_merge <- full_join(shrub_new, shrub_old, by = c("Plot_Name" = "Plot_Name",
                                                      "StartYear" = "Year",
                                                      "IsQAQC" = "Event_QAQC",
                                                      "ScientificName" = "Latin_Name") )

table(complete.cases(shrub_merge$Event_ID)) # 4 NAs COLO-380-2018; FRSO-002/054/106-2008 QAQC; have a better way to handle now


check_shrbs <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "StartYear", "IsQAQC",
              "ScientificName", col1, col2)]}
  )) %>% bind_rows()
}

names(shrub_merge)
shrub_dif <- shrub_merge %>% select(Plot_Name, StartYear, IsQAQC, ScientificName, shrub_avg_cov, cover) %>%
  mutate(cov_diff = abs(shrub_avg_cov - cover)) #%>% filter(cov_diff > 0.1)
# Shouldn't have any cover data for records < 2009; Not sure why sometimes we have that in the database
# The only record >2009 is SARA-023-2012 b/c duplicate Vitis in UR. Second doesn't migrate, which is fine.

#

#--------------------------
# Quadrat seedlings
#--------------------------
# Sigh...MIDN protocol differences are really sucking the joy out of this...
library(forestMIDN)
library(tidyverse)
path = "C:/Forest_Health/exports/MIDN/"
#exportCSV(path = "C:/Forest_Health/exports/MIDN", zip = T)
importCSV(path = path, zip_name = "MIDN_Forest_20210406.zip") # Release from 1.0.21
# This zip file has taxa and quad seedling SQs manually fixed.

head(seeds_vw)
#seed_na <- seed_comb[is.na(seed_comb$tot_seeds),]

table(seeds_vw$SQSeedlingCode) # the numbers below are close to what it should be
# with a few more NS b/c of COLO-380-2018
#ND     NP     NS     SS
#31   3027     37 107948

#892; 512 benchmark
microbenchmark::microbenchmark(
seeds_new <- joinQuadSeedlings(locType = 'all', eventType = 'all', QAQC = T, valueType = 'all', canopyForm = 'all')
,times = 5)

nrow(seeds_new)
seeds_pm <- seeds_new[seeds_new$Txt_Cov %in% "Permanently Missing",]
nrow(seeds_pm)
# There are 76 rows with a blank % Cover, which also matches the number in the database.

test1 <- joinQuadSeedlings(park = 'ASIS', canopyForm = 'all')
test2 <- joinQuadSeedlings(speciesType = 'invasive', canopyForm = 'all', QAQC = T, locType = 'all', eventType = 'all')
length(unique(test2$EventID)) #1185

#------------------------
# Microplot Saplings
#------------------------

park = 'all'
park = "ASIS"
from = 2007
to = 2019
QAQC = T
locType = 'all'
eventType = 'all'
panels = 1:4
#status = 'all'
# status = 'active'
# status = 'dead'
status = 'live'
speciesType = 'all'
canopyForm = 'all'
status = 'all'
canopyForm = 'canopy'


table(sap_tax$SQSaplingCode, sap_tax$Count, useNA = 'always') # 5 NAs b/c ND SQ.

dbhcheck<-as.data.frame(table(sap_comb$SaplingStatusCode, sap_comb$DBHcm, useNA = 'always'))
                        # There are 7 AS with NA DBH. These aren't migration issues

dbhcheck2 <- sap_comb %>% filter(SaplingStatusCode == "AS" & is.na(DBHcm)) %>%
             select(Plot_Name, StartYear, IsQAQC, SQSaplingCode, MicroplotCode,
                    ScientificName, TagCode, SaplingStatusCode, DBHcm, Count)

sap_new <- joinMicroSaplings(locType = 'all', eventType = 'all', QAQC = T)
length(unique(sap_new$EventID)) #1185
sap_exo <- joinMicroSaplings(locType = 'all', eventType = 'all', QAQC = T, speciesType = 'exotic')
sap_exo_live <- joinMicroSaplings(locType = 'all', eventType = 'all', QAQC = T, speciesType = 'exotic', status = 'live')
length(unique(sap_exo_live$EventID)) #1185


#---------------------------
# joinRegen checking
#---------------------------
library(forestMIDN)
library(tidyverse)
path = "C:/Forest_Health/exports/MIDN/"
#exportCSV(path = "C:/Forest_Health/exports/MIDN", zip = T)
importCSV(path = path, zip_name = "MIDN_Forest_20210406.zip") # Release from 1.0.21
# This zip file has taxa and quad seedling SQs manually fixed.

forestMIDNarch::importData()

microbenchmark::microbenchmark(
  reg_new <- joinRegenData(eventType = 'all', locType = 'all', QAQC = T, canopyForm = 'all'),
  reg_old <- forestMIDNarch::joinRegenData(eventType = 'all', locType = 'all', QAQC = T,
                                           from = 2007, to = 2019, canopyForm = 'all'),
  times = 5) # new one is faster, even though it's clunky too

reg_new <- joinRegenData(eventType = 'all', locType = 'all', QAQC = T, canopyForm = 'all')

length(unique(reg_new$EventID)) #1185

reg_old$Latin_Name[reg_old$Latin_Name == "No species recorded"] <- "None present"
head(reg_new)
head(reg_old)
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
check_reg(reg_merge, "seed_15_30cm", "seed15.30") # 14 records that should mostly be fixed by migration
check_reg(reg_merge, "seed_30_100cm", "seed30.100") # 13 records that should mostly be fixed by migration
check_reg(reg_merge, "seed_100_150cm", "seed100.150") # 13 records that should mostly be fixed by migration
check_reg(reg_merge, "seed_p150cm", "seed150p")# 13 records that should mostly be fixed by migration
check_reg(reg_merge, "sap_den", "sap.den") #47 records that differ. Most should be fixed by migration

reg_merge2 <- reg_merge %>% select(Plot_Name, StartYear, IsQAQC, ScientificName, num_micros:regen_den,
                                   seed15.30:stock.y) %>% mutate(stock_diff = stock.x - stock.y)
names(reg_merge2)
# Stocking is going to change because dropping saplings >2.5 cm per FIA and EFWG
check_stock <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "ScientificName",
              "sap_den", "sap.den", col1, col2)]}
  )) %>% bind_rows()
}

stock_diff <- check_stock(reg_merge2, "stock.x", 'stock.y')
View(stock_diff) # all of the differences are because of fixes in new data and dropping saplings >2.5" from SI
check_reg(reg_merge2, "sap_den", "sap.den")
# when all saps < 2.5" same stocking index between old and new

#---------------------------
# Additional Species
#----------------------------
addspp_vw <- get("COMN_AdditionalSpecies", envir = VIEWS_MIDN)
addspp_vw$Plot_Name <- paste(addspp_vw$ParkUnit, sprintf("%03d", addspp_vw$PlotCode), sep = "-")
addspp_vw <- addspp_vw %>%
  select(Plot_Name, PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, TSN, ScientificName,
         ConfidenceClassCode, IsCollected, SQAddSppCode, Note,  SQAddSppNotes)

addspp_nd <- addspp_vw %>% filter(SQAddSppCode == "ND" & StartYear > 2008)

table(addspp_vw$SQAddSppCode, useNA = 'always')

head(addspp_vw)
head(addspp)

addspp2 <- forestMIDNarch::joinLocEvent(locType = 'all', eventType = 'all', from = 2007, to = 2019, QAQC = T) %>%
  left_join(., addspp) %>% left_join(., plants[, c("TSN", "Latin_Name")]) %>%
  select(Plot_Name, Year, Event_QAQC, TSN, Latin_Name, Confidence_ID, Collected, Notes)
names(addspp_vw)
names(addspp2)
addspp_merge <- merge(addspp_vw, addspp2,
                      by.x = c("Plot_Name", "StartYear", "IsQAQC", "TSN"),
                      by.y = c("Plot_Name", "Year", "Event_QAQC", "TSN"),
                      all.x = T, all.y = T)


check_spp <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", col1, col2)]}
  )) %>% bind_rows()
}

addspp_na <- addspp_merge[is.na(addspp_merge$ScientificName),]

check_spp(addspp_merge, "ScientificName", "Latin_Name")
# RICH-226-2009 is missing Polystichum acrostichoides in new database. Hopefully it will show up in the next migration.
# Check back to be sure.

addspp_test <- joinAdditionalSpecies()

#-----------------------------------
# Server connection
connect_serv <- "Driver={SQL Server};server=INP2300VTSQL16\\IRMADEV1;database=MIDN_Forest;trusted_connection=TRUE;ReadOnly=True"
# Local connection
connect <- "Driver={SQL Server};server=localhost\\SQLEXPRESS;database=MIDN_Forest;trusted_connection=TRUE;ReadOnly=True"

con <- RODBC::odbcDriverConnect(connection = connect, readOnlyOptimize = TRUE, rows_at_time = 1)

path_db <- "C:/Forest_Health/schema/"

# Import Soil header
soilheader_qry <- readr::read_lines(paste0(path_db, "COMN_SoilHeader.sql")) %>%
  glue::glue_collapse(sep = "\n") %>%
  glue::glue_sql(.con = conn)

COMN_SoilHeader <- RODBC::sqlQuery(con, soilheader_qry)

# Import soil lab data
soillab_qry <- readr::read_lines(paste0(path_db, "COMN_SoilLab.sql")) %>%
  glue::glue_collapse(sep = "\n") %>%
  glue::glue_sql(.con = conn)

COMN_SoilLab <- RODBC::sqlQuery(con, soillab_qry)

#Import Soil sample data
soilsample_qry <- readr::read_lines(paste0(path_db, "COMN_SoilSample.sql")) %>%
  glue::glue_collapse(sep = "\n") %>%
  glue::glue_sql(.con = conn)

COMN_SoilSample <- RODBC::sqlQuery(con, soilsample_qry)
RODBC::odbcClose(con)

assign("COMN_SoilHeader", COMN_SoilHeader, envir = VIEWS_MIDN)
assign("COMN_SoilLab", COMN_SoilLab, envir = VIEWS_MIDN)
assign("COMN_SoilSample", COMN_SoilSample, envir = VIEWS_MIDN)

names(VIEWS_MIDN)
names(VIEWS_MIDN$COMN_SoilHeader)

exportCSV(path = "C:/Forest_Health/exports/MIDN", zip = T) #20210413
importCSV(path = "C:/Forest_Health/exports/MIDN", zip_name = "MIDN_Forest_20210413.zip")
list.files(path)
# migration checking

env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

# Prepare the soil data
tryCatch(soilhd_vw <- get("COMN_SoilHeader", envir = VIEWS_MIDN) %>%
           select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                  SampleType.Label, SoilPosition.Code, HorizonQualifier.Code,
                  SoilEvent.Note, SoilEvent.IsArchived),
         error = function(e){stop("COMN_SoilHeader view not found. Please import view.")})

tryCatch(soillab_vw <- get("COMN_SoilLab", envir = VIEWS_MIDN) %>%
           select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                  SampleType.Code, Position.Code, Position.Code, Horizon.Code, Horizon.Label,
                  SoilEvent.Note, SoilEvent.IsArchived, SoilEvent.LegacyID, Lab.Layer,
                  Lab.DateSoilCollected, DateDry:ECEC, Lab.Notes, Lab.LegacyID, EventID, PlotID),
         error = function(e){stop("COMN_SoilLab view not found. Please import view.")})

tryCatch(soilsamp_vw <- get("COMN_SoilSample", envir = VIEWS_MIDN) %>%
           select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                  SQSoil.Code, SampleSequence.Code, SoilLayer.Label,
                  Depth_cm, Note),
         error = function(e){stop("COMN_SoilSample view not found. Please import view.")})


# Pivot the soilsamp_vw layers wide
soilsamp_vw$SoilLayer = gsub(" ", "_", soilsamp_vw$SoilLayer.Label)
soilsamp_vw$SoilLayer = gsub("Unconsolidated_Litter", "Litter", soilsamp_vw$SoilLayer)

# Change this step after migration switches FF to O
soilsamp_wide <- soilsamp_vw %>% select(-SoilLayer.Label) %>%
  filter(SoilLayer %in% c("Litter", "Forest_Floor", "A_Horizon", "Total_Depth")) %>%
  pivot_wider(names_from = SoilLayer,
              values_from = Depth_cm) %>%
  rename(O_Horizon = Forest_Floor) %>%
  mutate(O_Horizon = ifelse(is.na(O_Horizon), 0, O_Horizon),
         A_Horizon = ifelse(is.na(A_Horizon), 0, A_Horizon),
         Total_Depth = ifelse(is.na(Total_Depth) | Total_Depth == 0,
                              O_Horizon + A_Horizon,
                              Total_Depth))


soilsamp_wide$Plot_Name <-  paste(soilsamp_wide$ParkUnit, sprintf("%03d", soilsamp_wide$PlotCode), sep = "-")

#tbl_ssd <- read.csv("C:/Forest_Health/exports/tbl_Soil_Sample_Data.csv")
forestMIDNarch::importData(type = 'file',
  path = 'D:/NETN/Monitoring_Projects/Forest_Health/Database/MIDN/2021_Forest_Database/MIDN_FVM_BE_MASTER_20210408_Migration.accdb')

names(soildata)
names(soilsamp)
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

soilsamp_merge <- merge(soilsamp_wide, soil_old2,
                        by.x = c("Plot_Name", "StartYear", "IsQAQC", "SampleSequence.Code"),
                        by.y = c("Plot_Name", "Year", "Event_QAQC", "Sample_Number"), all = T)
names(soilsamp_merge)

soil_miss <- soilsamp_merge %>% filter(is.na(SQSoil.Code)) # I think these are all okay and will get cleaned up in next migration

check_soils <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "SQSoil.Code", "Sampling_Position", "SampleSequence.Code", col1, col2)]}
  )) %>% bind_rows()
}


p5 <- soilsamp_merge %>% filter(Litter_Depth == 0.5 |A_Horizon_Depth == 0.5 | FF_Depth == 0.5)

soilsamp_merge <- soilsamp_merge %>% mutate(A_diff = abs(A_Horizon_Depth - A_Horizon),  #%>% filter(A_diff > 0.5) # 2 records due to rounding errors
                                            O_diff = abs(O_Horizon - FF_Depth),
                                            tot_diff = abs(Total_Excavation_Depth - Total_Depth))

check_soils(soilsamp_merge, "Note", "Comments") # no issues

table(soilsamp_merge$SQSoil.Code) # All SS.

#------------------------------
# joinSoilLabData
#-----------------------------
soillab_test <- joinSoilLabData(from = 2010, to = 2014)

soilsamp_test <- joinSoilSampleData()


# ------------------------------
# Joining notes functions

test <- joinVisitNotes() %>% filter(StartDate > "2019-06-01")

head(test)
t2 <- joinVisitNotes(park = "APCO", from = 2016, to = 2019, QAQC = TRUE)
head(t2)
table(test$IsQAQC)
table(t2$IsQAQC)

t3 <- t2 %>% filter(StartDate > "2019-06-06")
table(t3$StartDate)
table(t2$StartDate)

#---------------------
# sumSpeciesList

test <- sumSpeciesList(park = "ASIS")



#--------------------
# Tree map testing

plotTreeMap(park = "ASIS", from = 2016, to = 2019, output_to = 'view', plotName = "ASIS-386", canopyPosition = "canopy")
plotTreeMap(park = "ASIS", from = 2016, to = 2019, output_to = 'view', plotName = "ASIS-386", status = 'live')
plotTreeMap(park = "ASIS", from = 2016, to = 2019, output_to = 'view', plotName = "ASIS-387")
plotTreeMap(park = "ASIS", from = 2016, to = 2019, output_to = 'view', plotName = "ASIS-386")

panel23 <- plotTreeMap(park = "FRSP", from = 2016, to = 2019, panels = c(2,3), output_to = "file",
                       path = "D:/NETN/Monitoring_Projects/Forest_Health/2021_Data/Maps&Data/MIDN_Tree_Maps")

plotTreeMap(park = "RICH", from = 2016, to = 2019, panels = c(2,3), output_to = "file",
            path = "D:/NETN/Monitoring_Projects/Forest_Health/2021_Data/Maps&Data/MIDN_Tree_Maps")
plotTreeMap(park = "PETE", from = 2016, to = 2019, panels = c(2,3), output_to = "file",
            path = "D:/NETN/Monitoring_Projects/Forest_Health/2021_Data/Maps&Data/MIDN_Tree_Maps")
plotTreeMap(park = "GEWA", from = 2016, to = 2019, panels = c(2,3), output_to = "file",
            path = "D:/NETN/Monitoring_Projects/Forest_Health/2021_Data/Maps&Data/MIDN_Tree_Maps")
plotTreeMap(park = "THST", from = 2016, to = 2019, panels = c(2,3), output_to = "file",
            path = "D:/NETN/Monitoring_Projects/Forest_Health/2021_Data/Maps&Data/MIDN_Tree_Maps")
plotTreeMap(park = "SAHI", from = 2016, to = 2019, panels = c(2,3), output_to = "file",
            path = "D:/NETN/Monitoring_Projects/Forest_Health/2021_Data/Maps&Data/MIDN_Tree_Maps")

#-------------------
head(sumTreeDBHDist(status = 'live'))
head(sumTreeDBHDist(units = "BA"))
head(sumTreeDBHDist(units = "dens"))
head(sumTreeDBHDist(units = "both", stauts = 'live'))
head(sumTreeDBHDist(park = "ROVA", units = 'both', canopyPosition = 'canopy'))
head(sumTreeDBHDist(park = "ROVA", units = 'both', speciesType = 'exotic'))


#--------------------
# sumQuadGuilds
test <- sumQuadGuilds()
head(test)
test2 <- sumQuadGuilds(park = "ACAD", speciesType = "invasive")
head(test2)
table(test2$Group, test2$StartYear)
head(sumQuadGuilds(park = "APCO", from = 2016, to = 2019, speciesType = 'native', splitHerb = TRUE))
head(sumQuadGuilds(park = "APCO", from = 2016, to = 2019, speciesType = 'exotic', splitHerb = TRUE))
head(sumQuadGuilds(park = "APCO", from = 2016, to = 2019, speciesType = 'invasive', splitHerb = TRUE))

#-------------------
# sumSapDBH

test <- sumSapDBHDist()
head(sumSapDBHDist(park = "GETT"))
head(sumSapDBHDist(park = "GETT", canopyForm = "canopy", speciesType = 'exotic'))
head(sumSapDBHDist(park = "GETT", canopyForm = "canopy", speciesType = 'native'))
head(sumSapDBHDist(park = "GETT", canopyForm = "canopy", speciesType = 'native', from = 2016, to = 2019))


