# Reinstall forestMIDN from github
library(devtools)
install_github("KateMMiller/forestMIDN", ref = "sql_overhaul")

# Load library
library(forestMIDN)
exportCSV(zip = T) # Should fail with useful error message

#----------------------
# Flavors of importData() and error handling explained
#---------------------
importData() # Import with defaults. Should work if you have a local instance installed
head(VIEWS_MIDN$COMN_CWD) # should see the CWD data
importData(instance = "local2") # will fail quickly if you specify anything but local or server
importData(instance = "local", server = "localhsot") # misspelled server- will fail with useful
# error message, but has to try the odbc driver connection before it fails, so takes longer to fail.
importData(new_env = FALSE) # imports views to global environment using default local instance
importData(instance = "server", server = "INP2300VTSQL16\\IRMADEV1") # Connect to remote server
# This can be quite slow to complete.
# NOTE: if not connected to VPN, will fail with informative error message, but has to try the
# odbc driver connection before it fails, so takes a few seconds.

#----------------------
# Flavors of exportCSV() and error handling explained
#---------------------
exportCSV() # Default settings will export individual CSVs to working directory if no path specified.
# Output in console will also state this. This function also checks that all of the views exist
# in your workspace (either VIEWS_MIDN or global environment). If all are missing, you'll get a
# message to import the data. If some are missing, the message will tell you which views are missing.
path = "C:/Forest_Health/exports/MIDN"
exportCSV(path = path) # individual csvs exported to path
exportCSV(path = "C:/Forest_Health/epxorts/MIDN") # misspelled path- should fail quickly
exportCSV(path = path, zip = TRUE) # outputs a zip with csvs with date at the end of filename
# NOTE: if you don't have the zip package installed, it will fail and tell you to install it
# but only if you specify zip = TRUE.
#install.packages("zip") # run if you need the zip package

#----------------------
# Flavors of importCSV() and error handling explained
#---------------------
importCSV() # Will fail quickly, because requires a path to import csvs or zip
importCSV(path = ptah) # Will fail quickly, because path misspelled/doesn't exist
importCSV(path = path) # Default import puts views into VIEWS_MIDN
importCSV(path = path, new_env = FALSE) # import individual csvs and put in global environment
# Testing that if a view is missing from the path folder, importCSV will throw an error
file.remove(paste0(path, "/", "COMN_AddtionalSpecies.csv"))
importCSV(path = path) # Default import puts views into VIEWS_MIDN
importCSV(path = path, zip_name =
            paste0("MIDN_Forest_",  format(Sys.Date(), "%Y%m%d"), ".zip")) # If you exported zip on a day,
# change file name to the YYYYMMDD of your zip file.
importCSV(path = path, zip_name = "MIDN_Froest_20210220.zip") # Will fail quickly b/c zip_name misspelled


