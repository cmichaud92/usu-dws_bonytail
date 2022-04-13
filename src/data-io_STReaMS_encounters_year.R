
#######################################################
#          Data io: Fetch data from STReaMS           #
#######################################################

# Fetch data from STReaMS

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

CONFIG <-  "live_site"  # Comment out to access the test-site
#MACHINE <- "FWS"
MACHINE <- "CNHP"

#----------------------------------------
# Fetch configuration
#----------------------------------------

if (MACHINE == "FWS") {
  CONFIG_PATH <-  "C:/Users/cmichaud/OneDrive - DOI/Documents/Projects/etc/config_streams.yml"
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
         SpeciesName = CommonName)

# PIA table
p_loc_typ <- tbl(con, "LKU_PIALocationType") %>%
  select(PIALocationTypeID = ID,
         HabitatType = Name)

p_loc <- tbl(con, "TBL_PIALocation") %>%
  left_join(p_loc_typ, by = "PIALocationTypeID") %>%
  select(PIALocationID = ID,
         PIAStartDate = StartDate,
         PIAEndDate = EndDate)

p_ant <- tbl(con, "TBL_PIAAntenna") %>%
  rename(AntennaID = ID)

p_array <- tbl(con, "TBL_PIAArray") %>%
  rename(ArrayID = ID) %>%
  left_join(p_ant, by = c("ArrayID" = "PIAArrayID"))%>%
  left_join(p_loc, by = "PIALocationID") %>%
  select(ArrayID, AntennaID, PIALocationID,
         PIAName = Name)


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


# Encounter table
enc_p <- tbl(con, "TBL_Encounter") %>%
  rename(EncounterTypeID = EncounterType) %>%
  inner_join(p_enc_typ, by = "EncounterTypeID") %>%
  inner_join(p_indiv, by = "IndividualID") %>%
  left_join(p_array, by = "AntennaID") %>%
  left_join(p_gear, by = "GearTypeID") %>%
  mutate(Year = year(EncounterDateTime)) %>%
  select(EncounterID = ID,
         Year,
         EncounterDateTime,
         RiverMile,
         EncounterType,
         GearTypeCode,
         SpeciesCode,
         SpeciesName,
         PIAName)


h <- head(enc_p) %>%
  collect()

enc1 <- enc_p %>%
  group_by(EncounterType, Year) %>%
  summarise(n = n()) %>%
  filter(!is.na(Year) &
           EncounterType != "Transfer") %>%
  collect() %>%
  ungroup() %>%
  mutate(era = case_when(Year < 1992 ~ "Paper",
                         between(Year, 1992, 2004) ~ "Excel",
                         between(Year, 2005, 2014) ~ "Access",
                         Year >= 2015 ~ "STReaMS"),
         across(EncounterType, ~factor(.,
                                       levels = c("Detection", "Capture", "Stocking"))))
enc1 %>%
  ggplot(aes(x = Year, y = n, fill = EncounterType)) +
  geom_col(color = "black") +
  scale_y_continuous(label = scales::comma) +
  geom_vline(xintercept = 1992, color = "green") +
  geom_vline(xintercept = 2005, color = "darkred") +
  geom_vline(xintercept = 2015, color = "darkblue") +
  labs(title = "Raw encounters added to STReaMS by year,\n1979 - 2022",
       y = "Number of encounters",
       caption = "NOTE: Not all pre-2006 data has been migrated to STReaMS") +
  theme_bw()

enc2 <- enc_p %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(n = n()) %>%
  collect() %>%
  arrange(Year) %>%
  mutate(c_sum = cumsum(n))

enc2 %>%
  filter(Year >= 1992) %>%
  ggplot(aes(x = Year,
             y = c_sum)) +
  geom_col() +
  scale_y_continuous(label = scales::comma) +
  geom_vline(xintercept = 1992, color = "green") +
  geom_vline(xintercept = 2005, color = "darkred") +
  geom_vline(xintercept = 2015, color = "darkblue") +
  labs(title = "Cumulative sum of total encounters in STReaMS by year,\n1979 - 2022",
       y = "Number of encounters",
       caption = "NOTE: Not all pre-2006 data has been migrated to STReaMS") +
  theme_bw()


enc_p %>%
  distinct(IndividualID) %>%
  summarise(n = n())

enc3 <- enc_p %>%
  group_by(IndividualID, EncounterType) %>%
  mutate(n_indiv = n()) %>%
  group_by(EncounterType, Year) %>%
  summarise(n_indiv = sum(n_indiv)) %>%
  collect()


enc_p %>%
  group_by(EncounterType) %>%
  summarise(n = n())

ck <- enc_p %>%
  filter(EncounterType == "Detection" &
           !is.na(GearTypeCode) &
           SpeciesCode %in% c("CS", "BT", "HB", "RZ")) %>%
  group_by(SpeciesCode, GearTypeCode) %>%
  summarise(n = n()) %>%
  collect
ck %>% filter(GearTypeCode != "SR") %>%
  group_by(SpeciesCode) %>%
  summarise(n = sum(n)) %>%
  ggplot()+
  geom_col(aes(x = SpeciesCode, y = n))

ck

# ----- Nontagged fish bin -----
p_ntf <- p_site %>%
  select(id_sample) %>%
  inner_join(tbl(con, "BIN_NonTaggedFish"),
             by = c("id_sample" = "SampleNumber"))%>%
  rename(SpeciesCode = Species) %>%
  left_join(p_spp, by = "SpeciesCode") %>%

  select(id_sample = SampleNumber,
         tm_enc = DateTime,
         rmi_enc = RiverMile,
         cd_spp = SpeciesCode,
         n_fish = FishCount,
         val_totlen = Length,
         val_weight = Weight,
         cat_sex = Sex,
         ind_ripe = Ripe,
         nm_sci = ScientificName,
         nm_com = CommonName,
         val_utmz = UTMZone,
         val_utmx = UTMX,
         val_utmy = UTMY,
         id_upload = SampleUploadID,
         id_uploadntf = UploadID,
         id_uploadrf = RareFishUploadID)

#--------------------------------------------
# Encounter tbl:
#   Preprocess as much as possible
#   BEFORE collecting!!!
#--------------------------------------------

# Raw datatable

# ----- Terminate DBI connection -----

dbDisconnect(con)


# ----- Write queried data to disk -----

write_rds(enc, "./data/2021-02-19_streams-cs-encounter.rds")


## END
