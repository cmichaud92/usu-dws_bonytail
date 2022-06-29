

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

m_enc <- read_tsv("./data/Mounds_encounters.txt")
table(m_enc$CommonName)
ck <- unique(m_enc$IndividualID[m_enc$CommonName == "unidentified"])
m_enc %>%
  summarise(n_ind = n_distinct(IndividualID),
            n_spp = n_distinct(CommonName),
            n = n())
unique(m_enc$CommonName)
unique(m_enc$Org)

ind <- m_enc %>%
  count(MostRecentTag) %>%
  filter(n > 100)
m_enc %>%
  distinct(IndividualID, CommonName) %>%
  count(CommonName)
clues <- m_enc %>%
  filter(CommonName %!in% "unidentified") %>%
  distinct(IndividualID) %>%
  pull()

ids <- unique(clues$IndividualID)

write_lines(clues, "./data/mounds.txt")

encs <- read_tsv("./data/all-rvrs_Mounds-indiv_encounters.txt")

c1 <- encs %>%
  filter(EncounterType == "Capture")

table(c1$CommonName)
table(c1$ProjectBiologist)
range(c1$EncounterDate)
