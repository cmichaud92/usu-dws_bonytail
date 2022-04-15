#######################################################
#       Data io: Fetch data from STReaMS download     #
#######################################################

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


# ----- Attach Packages -----
{
  library(tidyverse)
  library(lubridate)
  library(UCRBtools)
  library(ggplot2)
}

theme_set(theme_bw())

`%!in%` <- Negate(`%in%`)

#----------------------------------------
# Data i/o
#----------------------------------------

# Read in individual data acquired from STReaMS
tmp_ind <- read_tsv("./data/PR_stocked_individuals_20220414.txt")

table(tmp_ind$Mortality)
ck <- tmp_ind %>%
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

# The dataset is really wide, select relevant cols if helpful
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


# General information
table(enc$EncounterType)
table(enc$ArrayName)
table(enc$RiverName)


#--------------------------------
# Feature engineering
#--------------------------------
summary_ind <- enc %>%
  group_by(IndividualID) %>%
  summarise(days_at_lrg = difftime(max(EncounterDate), min(EncounterDate), units = "days"),
            n_rivers = n_distinct(RiverName),
            n_arrays = n_distinct(ArrayName),
            n_det_typ = n_distinct(EncounterType),
            .groups = "drop")

mvmt <- enc %>%
  select(IndividualID, EncounterID, EncounterType, EncounterDate, EncounterDateTime, RiverName, RiverMile, ArrayName) %>%
  group_by(IndividualID) %>%
  arrange(IndividualID, EncounterDateTime) %>%
  mutate(trans = ifelse(lag(RiverName) == RiverName &
                          lag(RiverMile) == RiverMile, 0, 1) %>%
           coalesce(0)) %>%
  group_by(IndividualID) %>%
  mutate(trans_order = cumsum(trans) + 1) %>%
  ungroup()

mvmt1 <- mvmt %>%
  group_by(IndividualID, EncounterType, RiverName, RiverMile, ArrayName, trans_order) %>%
  summarise(across(EncounterDate, list(min = min,
                                       max = max)),
            n_encounters = n(),
            .groups = "drop") %>%
  group_by(IndividualID) %>%
  arrange(IndividualID, trans_order) %>%
  mutate(res_days = difftime(EncounterDate_max, EncounterDate_min, units = "days"),
         trans_days = difftime(EncounterDate_min, lag(EncounterDate_max), units = "days")) %>%
  group_by(IndividualID, RiverName) %>%
  mutate(dist = RiverMile - lag(RiverMile)) %>%
  ungroup() %>%
  left_join(summary_ind, by = "IndividualID")



ck <- mvmt1$IndividualID[mvmt1$dist == 0]
ck <- mvmt1 %>%
  filter(dist > 0) %>%
  distinct(IndividualID) %>%
  pull(IndividualID)

ck1 <- mvmt1 %>%
  filter(IndividualID %in% ck)

ck <- summary_ind$IndividualID[summary_ind$n_det_typ == 3]
ck1 <- mvmt1 %>%
  filter(IndividualID %in% ck)

mvmt1 %>%
  ggplot() +
  geom_bar(aes(x = trans_order, fill = ArrayName))


mvmt %>%
  filter(trans_order == 2) %>%
  count(ArrayName)

enc %>%
  group_by(IndividualID, ArrayName, EncounterType) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ArrayName, EncounterType) %>%
  summarise(unq_indiv = n(),
            total_enc = sum(n))

nrow(enc)

agg_day <- enc %>%
  count(IndividualID, EncounterDate, RiverName, RiverMile)
nrow(agg_day)

nrow(filter(agg_day,n >1))

agg_ind <- enc %>%
  group_by(IndividualID, EncounterDate, RiverName, RiverMile) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(IndividualID) %>%
  summarise(across(EncounterDate, list(min,
                                       max)),
            n_days_enc = n()) %>%
  mutate(days_at_large = difftime(EncounterDate_2, EncounterDate_1, units = "days"))

range(agg_ind$days_at_large)

agg_ind_mv <-  enc %>%
  group_by(IndividualID, RiverName, RiverMile) %>%
  summarise(n = n(),
            min_date = min(EncounterDate),
            max_date = max(EncounterDate),
            .groups = "drop") %>%
  arrange(IndividualID, desc(max_date)) %>%
  group_by(IndividualID) %>%
  mutate(days_res = difftime(max_date, min_date, units = "days"),
         days_mv = difftime(max_date, lead(min_date), units = "days"))

range(agg_ind_mv$days_res)
range(agg_ind_mv$days_mv, na.rm = TRUE)

enc %>%
  count(IndividualID, EncounterDate) %>%
  ggplot() +
  geom_histogram(aes(x = n)) +
  scale_y_log10()

enc %>%
  count(IndividualID, EncounterDate) %>%
  filter(n < 10) %>%
  sum(.$n == 1)
  ggplot() +
  geom_histogram(aes(x = n))# +
  scale_y_log10()



