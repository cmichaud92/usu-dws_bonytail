#######################################################
#       Data io: Fetch data from STReaMS download     #
#######################################################



# ----- Attach Packages -----
{
  library(tidyverse)
  library(lubridate)
  library(waterData)
  library(UCRBtools)
  library(ggplot2)
}

theme_set(theme_bw())

`%!in%` <- Negate(`%in%`)

# User defined variables
TRIBS <- c("PR")
GUAGES <- c("Woodside","Confluence")
WRITE_TO_DISK = FALSE

#----------------------------------------
# Data i/o
#----------------------------------------

# Query the STReaMS GUI: "View and edit data" -> "Encounters"

# FILTER: Species == "Gila elegans",
#         Encounter Type == "Stocking"
#         River == "Price River"
#         Date Range == "2016" to "2022"

# Download (this takes time! sometimes an hour or more)

# When u recieve a link via email, paste it into your browser
# Right click on the "STReaMS_Individuals_YYYY_MM_DD_HH_MM.txt link and select
# "Save link as", select a location and save ...

# Ignore the Encounters file (we will fetch a more comprehensive file soon)

# ----- STReaMS ENcounter data -----

# Read in individual data acquired from STReaMS
tmp_ind <- read_tsv("./data/PR_stocked_individuals_20220414.txt")

table(tmp_ind$Mortality)
tmp_ind %>%
  filter(Mortality == "Yes")
# Dataset dimensions
dim(tmp_ind)

# Fetch the individualIDs for all fish stocked into the Price River
# These are the individuals of interest.
stk_ind <- unique(tmp_ind$IndividualID)

length(stk_ind)

# Create a text file(s) containing the IndividualIDS
# STReaMS accepts max of 10,000 ids at a time so break
# into smaller files if necessary
ids <- split(stk_ind, ceiling(seq_along(stk_ind)/10000))
names(ids) %>%
  walk(~write_lines(ids[[.]], paste0("./data/PR_stock_ind_ids_",
                                    .,
                                    ".txt")))

# Return to the STReaMS GUI: "View and edit data" -> "Fish" -> open your
# "ids.txt" file, copy and paste contents into into the "ID" box -> Filter
# -> Download. BE PATIENT!!! THis one takes longer!

# When u recieve a link via email, paste it into your browser
# Right click on the "STReaMS_Encounters_YYYY_MM_DD_HH_MM.txt link and select
# "Save link as", select a location and save ...
# The Individuals file should be == to the one you previously downloaded so you
# can ignore it...

tmp_enc <- read_tsv("./data/all-rvrs_encounters_20220414.txt")


# Assess dataset dimensions
dim(tmp_enc)
length(unique(tmp_enc$IndividualID))
length(unique(tmp_enc$EncounterID))

# Assess flags and notes, superficial QA
table(tmp_enc$IndividualDBAFlag)
unique(tmp_enc$EncounterDBAFlagNotes)
unique(tmp_enc$IndividualDBAFlagNotes)
unique(tmp_enc$CommonName)
unique(tmp_enc$DispositionType)
range(tmp_enc$EncounterDate)
sum(is.na(tmp_enc$EncounterDate))
sum(is.na(tmp_enc$EncounterTime))

# Encounter wrangling and feature development
enc <- tmp_enc %>%
  mutate(across(EncounterTime, ~ifelse(is.na(.), "00:00:00", as.character(.))),
         EncounterDateTime = ymd_hms(paste(EncounterDate, EncounterTime))) %>%
  select(IndividualID,
         EncounterID,
         MostRecentTag,
         TagDeployDate,
         CommonName,
         EncounterDate,
         EncounterDateTime,
         EncounterType,
         RiverName,
         RiverMile,
         TotalLength,
         Weight,
         DispositionType,
         ArrayName,
         AntennaCode,
         AntennaSubArray,
         SourceHatchery,
         HarvestType,
         ReleaseType,
         YearClass,
         Mortality,
         SampleorStockingNumber,
         ProjectBiologist,
         Org,
         Study,
         Longitude,
         Latitude,
         UTMX,
         UTMY,
         UTMZone,
         matches("Notes|Flag"))


# Stocking locations
unique(enc$RiverMile[enc$EncounterType == "Stocking"])
unique(enc$EncounterDate[enc$RiverMile == 27.3])
unique(enc$EncounterDate[enc$RiverMile == 19.4])


summary_ind <- enc %>%
  group_by(IndividualID) %>%
  summarise(days_at_lrg = difftime(max(EncounterDate), min(EncounterDate), units = "days"),
            total_encounters = n(),
            total_rivers = n_distinct(RiverName),
            total_arrays = n_distinct(ArrayName, na.rm = TRUE),
            total_enc_typ = n_distinct(EncounterType),
            .groups = "drop")

tmp_mv <- enc %>%
  select(IndividualID, EncounterID, EncounterType, EncounterDate, EncounterDateTime, RiverName, RiverMile, ArrayName) %>%
  group_by(IndividualID) %>%
  arrange(IndividualID, EncounterDateTime) %>%
  mutate(trans_ant = ifelse(lag(RiverName) == RiverName &
                               lag(RiverMile) == RiverMile, 0, 1) %>%
           coalesce(0),
         trans_rvr = ifelse(lag(RiverName) != RiverName, 1, 0) %>%
           coalesce(0)) %>%
  group_by(IndividualID) %>%
  mutate(trans_order_ant = cumsum(trans_ant) + 1,
         trans_order_rvr = cumsum(trans_rvr) + 1) %>%
  ungroup()

mv_ant <- tmp_mv %>%
  group_by(IndividualID, EncounterType, RiverName, RiverMile, ArrayName, trans_order_ant) %>%
  summarise(across(EncounterDate, list(min = min,
                                       max = max)),
            n_encounters = n(),
            .groups = "drop") %>%
  group_by(IndividualID) %>%
  arrange(IndividualID, trans_order_ant) %>%
  mutate(res_days = difftime(EncounterDate_max, EncounterDate_min, units = "days"),
         trans_days = difftime(EncounterDate_min, lag(EncounterDate_max), units = "days")) %>%
  group_by(IndividualID, RiverName) %>%
  mutate(dist = RiverMile - lag(RiverMile)) %>%
  ungroup() %>%
  left_join(summary_ind, by = "IndividualID")


mv_rvr <- tmp_mv %>%
  group_by(IndividualID, RiverName, trans_order_rvr) %>%
  summarise(across(EncounterDate, list(min = min,
                                       max = max)),
            n_encounters = n(),
            .groups = "drop") %>%
  group_by(IndividualID) %>%
  arrange(IndividualID, trans_order_rvr) %>%
  mutate(res_days = difftime(EncounterDate_max, EncounterDate_min, units = "days"),
         trans_days = difftime(EncounterDate_min, lag(EncounterDate_max), units = "days")) %>%
  ungroup() %>%
  left_join(summary_ind, by = "IndividualID")





# Stocking years
min_year <- year(min(tmp_enc$EncounterDate[tmp_enc$EncounterType == "Stocking"]))
max_year <- year(max(tmp_enc$EncounterDate[tmp_enc$EncounterType == "Stocking"]))


# ----- USGS Hydrology data -----

guage <- tribble(
  ~cd_rvr, ~nm_guage, ~staid,
  "GR", "Green River, UT", "09315000",
  "GR", "Ouray, UT", "09272400",
  "GR", "Jensen, UT", "09261000",
  "WH", "Watson, UT", "09306500",
  "PR", "Woodside, UT", "09314500",
  "SR", "Confluence with the Green River", "09328910",
  "SR", "Near Green River, UT", "09328500",
  "CO", "Near Cicso (Dewey), UT", "09180500",
  "CO", "UT/CO State Line", "09163500",
  "DO", "Near Cisco (Dewey), UT", "09180000",
)

# USGS Temperature and discharge data

tgt <- guage %>%
  filter(cd_rvr %in% TRIBS &
           grepl(paste(GUAGES, collapse = "|"),
                 nm_guage,
                 ignore.case = TRUE)) %>%
  pull(staid)


w <- map_df(tgt, ~importDVs(staid = .x,
                            code = "00060",
                            stat = "00003",
                            sdate = paste0(min_year, "-01-01"),
                            edate = paste0(max_year, "-12-31"))) %>%
  rename(discharge = val,
         date = dates,
         discharge_qualcode = qualcode)

t <- map_df(tgt, ~importDVs(staid = .x,
                            code = "00010",
                            stat = "00003",
                            sdate = paste0(min_year, "-01-01"),
                            edate = paste0(max_year, "-12-31"))) %>%
  rename(temperature = val,
         date = dates,
         temperature_qualcode = qualcode)

water <- left_join(w, t, by = c("staid", "date")) %>%
  left_join(guage, by = "staid") %>%
  select(cd_rvr,
         nm_guage,
         date,
         discharge,
         temperature,
         staid,
         matches("qualcode"))

# Stocking dates
tmp_stock_date <- enc %>%
  filter(EncounterType == "Stocking") %>%
  distinct(RiverName, EncounterDate)

stock_dates <- water %>%
  filter(date %in% tmp_stock_date$EncounterDate) %>%
  select(date,
         cd_rvr,
         discharge_pt = discharge,
         temp_pt = temperature)

fnl_water <-  water %>%
  left_join(stock_dates, by = c("date", "cd_rvr"))



#---------------------------------------
# Finalize data and write to disk
#---------------------------------------

#Build list
fnl_dat <- list(encounters = enc,
                antenna_transitions = mv_ant,
                river_transitions = mv_rvr,
                water_data = fnl_water)

# Write data to disk if "TRUE"
if (WRITE_TO_DISK == TRUE) {
  # Write list to csv
  names(fnl_dat) %>%
    walk(~write_csv(fnl_dat[[.]], paste0("./data/Bonytail_",
                                  .,
                                  ".csv"),
                    na = ""))

  # Write list to Rds
  write_rds(fnl_dat, "./data/Bonytail-research-data.Rds")
}
## END



