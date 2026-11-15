# Crosswalk from 2010 census tracts to CDC's current counties (#30).
#
# CDC AtlasPlus reports every year on its current county map. ACS 5-year
# releases from 2006-2010 to 2015-2019 use 2010 tracts, which belong to the
# old map in two places:
#   - Connecticut: the eight old counties (09001-09015) were replaced by nine
#     planning regions (09110-09190) as county-equivalents in 2022.
#   - Alaska: Valdez-Cordova Census Area (02261) was split into Chugach
#     (02063) and Copper River (02066) in 2019.
# This script gives each 2010 tract there a share in each current county,
# by land area, and writes inst/extdata/tract-2010-crosswalk.csv, which
# extrapolate_from_county() reads.
#
# Method:
#   - Census's 2020-to-2010 tract relationship files give the land area each
#     2010 tract shares with each 2020 tract.
#   - Connecticut's 2020 tracts kept their tract codes when they were
#     recoded to planning regions in 2022, so each is matched to its 2022
#     tract (and so its planning region) by tract code. Four water-only
#     tract codes (99xxxx) repeat across counties; they have no population
#     and are left out.
#   - Alaska's 2020 tracts already carry the new county codes.
#   - Overlaps under 1% of a tract's land are boundary slivers, not real
#     splits; they are dropped and the rest rescaled to sum to 1.
#
# Run from the package root with internet access:
#   source("data-raw/tract-crosswalk.R")
# Reads three small text files from www2.census.gov.

library(dplyr)

read_census <- function(url) {
  utils::read.delim(url, sep = "|", colClasses = "character")
}

rel <- "https://www2.census.gov/geo/docs/maps-data/data"
ct <- read_census(file.path(
  rel,
  "rel2020/tract/tab20_tract20_tract10_st09.txt"
))
ct22 <- read_census(file.path(rel, "rel2022/acs22_cousub22_tract22_st09.txt"))
ak <- read_census(file.path(
  rel,
  "rel2020/tract/tab20_tract20_tract10_st02.txt"
))

# Each Connecticut 2020 tract's 2022 tract: same tract code, and the closest
# land area where a code repeats.
tracts_22 <- distinct(ct22, GEOID_TRACT_22, AREALAND_TRACT_22) |>
  mutate(code = substr(GEOID_TRACT_22, 6, 11))
tracts_20 <- distinct(ct, GEOID_TRACT_20, AREALAND_TRACT_20) |>
  mutate(code = substr(GEOID_TRACT_20, 6, 11))
to_22 <- inner_join(
  tracts_20,
  tracts_22,
  by = "code",
  relationship = "many-to-many"
) |>
  mutate(
    gap = abs(as.numeric(AREALAND_TRACT_20) - as.numeric(AREALAND_TRACT_22))
  ) |>
  slice_min(gap, n = 1, with_ties = FALSE, by = GEOID_TRACT_20) |>
  select(GEOID_TRACT_20, GEOID_TRACT_22)
stopifnot(nrow(to_22) == nrow(tracts_20))

parts <- bind_rows(
  ct |>
    filter(substr(GEOID_TRACT_10, 1, 2) == "09") |>
    left_join(to_22, by = "GEOID_TRACT_20") |>
    transmute(
      tract_2010 = GEOID_TRACT_10,
      county_fips = substr(GEOID_TRACT_22, 1, 5),
      land = as.numeric(AREALAND_PART)
    ),
  ak |>
    filter(substr(GEOID_TRACT_10, 1, 5) == "02261") |>
    transmute(
      tract_2010 = GEOID_TRACT_10,
      county_fips = substr(GEOID_TRACT_20, 1, 5),
      land = as.numeric(AREALAND_PART)
    ) |>
    filter(county_fips %in% c("02063", "02066"))
) |>
  filter(!startsWith(substr(tract_2010, 6, 11), "99"))
stopifnot(!anyNA(parts$county_fips))

crosswalk <- parts |>
  summarize(land = sum(land), .by = c(tract_2010, county_fips)) |>
  mutate(share = land / sum(land), .by = tract_2010) |>
  filter(share >= 0.01) |>
  mutate(share = share / sum(share), .by = tract_2010) |>
  arrange(tract_2010, county_fips)

stopifnot(
  !anyNA(crosswalk$share),
  all(abs(tapply(crosswalk$share, crosswalk$tract_2010, sum) - 1) < 1e-9),
  all(crosswalk$county_fips %in% c(sprintf("091%d0", 1:9), "02063", "02066"))
)

lines <- paste(
  crosswalk$tract_2010,
  crosswalk$county_fips,
  ifelse(crosswalk$share == 1, "1", sprintf("%.6f", crosswalk$share)),
  sep = ","
)
writeLines(
  c("tract_2010,county_fips,share", lines),
  "inst/extdata/tract-2010-crosswalk.csv"
)
