
#######################################################
#       Data io: Fetch data from STReaMS backend      #
#######################################################

# Fetch data from MS SQLserver

# ----- Attach Packages -----
{
  library(tidyverse)
  library(dbplyr)
  library(lubridate)
  library(DBI)
  library(UCRBtools)
  library(ggplot2)
}

theme_set(theme_bw())
#----------------------------------------
# Enter user defined variables
#----------------------------------------
# Define hydro area for initial tag deployment
HYDRO_AREA <- c("PR")
YEAR <- c(2016:2022)
TARGET <- "BT"


CONFIG <-  "live_site"  # Comment out to access the test-site
#MACHINE <- "FWS"
MACHINE <- "CNHP"

#----------------------------------------
# Fetch configuration
#----------------------------------------

if (MACHINE == "FWS") {
  CONFIG_PATH <-  "C:/Users/cmichaud/Projects_git/etc/config_streams.yml"
} else if (MACHINE == "CNHP") {
  CONFIG_PATH <- "/Users/jstahli/Documents/etc/config_streams.yml"
}

if(exists("CONFIG")) {

  Sys.setenv(R_CONFIG_ACTIVE = CONFIG)
  config <- config::get(file = CONFIG_PATH)

} else {

  config <- config::get(file = CONFIG_PATH)

}


#----------------------------------------
# Establish STReaMS connection
#----------------------------------------

con <- dbConnect(odbc::odbc(),
                 Driver = config$driver,
                 Server = config$server,
                 UID    = config$uid,
                 PWD    = config$pwd,
                 Port   = config$port,
                 Database = config$database
)

# dbDisconnect(con)

#-------------------------------------------
# Create pointers to requisite tables
#-------------------------------------------

# Encounter Type table
p_enc_typ <- tbl(con, "D_EncounterType") %>%
  rename(EncounterTypeID = ID)

# Species table
p_spp <- tbl(con, "LKU_Species") %>%
  select(SpeciesID = ID,
         SpeciesCode = Code,
         SpeciesName = CommonName) %>%
  filter(SpeciesCode %in% TARGET)

# Hydro-areas table
p_hydro <- tbl(con, "LKU_HydroArea") %>%
  select(HydroAreaID = ID,
         HydroAreaCode = Code,
         HydroAreaName = Name) %>%
  filter(HydroAreaCode %in% HYDRO_AREA)

# PIA table
# p_loc_typ <- tbl(con, "LKU_PIALocationType") %>%
#   select(PIALocationTypeID = ID,
#          HabitatType = Name)

p_loc <- tbl(con, "TBL_PIALocation") %>%
#  left_join(p_loc_typ, by = "PIALocationTypeID") %>% collect
  select(PIALocationID = ID,
         RiverID) %>% collect

p_ant <- tbl(con, "TBL_PIAAntenna") %>%
  select(PIAAntennaID = ID,
         PIAArrayID) %>% collect

p_array <- tbl(con, "TBL_PIAArray") %>%
  rename(PIAArrayID = ID) %>% #collect
  left_join(p_ant, by = "PIAArrayID") %>%
  left_join(p_loc, by = "PIALocationID") %>%
  select(PIAArrayID,
         PIAAntennaID,
         PIALocationID,
         PIAName = Name,
         PIARiverID = RiverID,
         PIARiverMile = RiverMile,
         PIAUTMX = UTMX,
         PIAUTMY = UTMY,
         PIADBANotes = DBANotes)


# Gear table
p_gear_typ <- tbl(con, "LKU_GearType") %>%
  rename(GearTypeID = ID,
         GearTypeCode = Code,
         GearTypeName = Name,
         GearDefID = GearType) #%>% collect

p_gear_def <- tbl(con, "D_GearType") %>%
  rename(GearDefID = ID,
         GearDefName = GearType)

p_gear <- p_gear_typ %>%
  left_join(p_gear_def, by = "GearDefID")


# Individual table
p_indiv <- tbl(con, "TBL_Individual") %>%
  select(IndividualID = ID,
         SpeciesID) %>%
  inner_join(p_spp, by = "SpeciesID")


# Fetch IDs of all individual
ind_ids <- tbl(con, "TBL_Encounter") %>%
  rename(EncounterTypeID = EncounterType,
         HydroAreaID = RiverID) %>%
  inner_join(p_hydro, by = c("HydroAreaID")) %>%
  inner_join(p_indiv, by = "IndividualID") %>%
  mutate(Year = year(EncounterDateTime)) %>%
  filter(Year %in% YEAR) %>%
  distinct(IndividualID)


tmp_trib <- tbl(con, "TBL_Encounter") %>%
  rename(EncounterTypeID = EncounterType,
         HydroAreaID = RiverID) %>%
  inner_join(p_indiv, by = "IndividualID") %>%
#  filter(IndividualID %in% ind_ids) %>%
  left_join(p_hydro, by = c("HydroAreaID")) %>%
  inner_join(p_enc_typ, by = "EncounterTypeID") %>%
  inner_join(p_indiv, by = "IndividualID") %>%
  left_join(p_array, by = c("AntennaID" = "PIAAntennaID")) %>%
  left_join(p_gear, by = "GearTypeID") %>%
  mutate(Year = year(EncounterDateTime)) %>%
  head() %>% collect
  select(EncounterID = ID,
         IndividualID,
         Year,
         EncounterDateTime,
         HydroAreaCode,
         RiverMile,
         EncounterType,
#         FirstCapture,
         Length,
         Weight,
         GearTypeCode,
         SpeciesCode,
         SpeciesName,
         PIAName) %>%
  collect()

