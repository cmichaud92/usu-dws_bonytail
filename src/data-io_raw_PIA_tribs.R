

####################################################
#                 Raw Detection Files              #
####################################################

{
  library(tidyverse)
  library(lubridate)
}

# Define env variables
ARRAY <- "Woodside"
WRITE_TO_DISK = FALSE


# Data io
tmp_dat <- read_csv(paste0("./data/PIA-dat-file_",
                           ARRAY,
                           ".txt"),
                    skip = 4,
                    col_names = c("tm_upload", "idx", "tag_info")) %>%
  mutate(ArrayName = ARRAY)


dat <- tmp_dat %>%
  separate(tag_info,
           into = c("delete1", "delete2", "AntennaCode", "dt_scan", "tm_scan", "MostRecentTag"),
           sep = " ") %>%
  mutate(EncounterDateTime = mdy_hms(paste(dt_scan,
                                           tm_scan)),
         across(MostRecentTag, ~sub("[.]", "", .)),
         .keep = "unused") %>%
  select(!matches("delete"))


fnl_dat <- read_rds("./data/Bonytail-research-data.Rds")

tags <- fnl_dat$encounters %>%
  distinct(IndividualID, MostRecentTag)

encounters_fine <- inner_join(tags, dat, by = "MostRecentTag")

fnl_dat$pia_dat_file_woodside <- encounters_fine


if (WRITE_TO_DISK == TRUE) {

  write_csv(encounters_fine, "./data/Bonytail_PIA-dat-file_Woodside.csv")
  write_rds(fnl_dat, "./data/Bonytail-research-data.Rds")
}
