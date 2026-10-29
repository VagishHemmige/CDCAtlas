# Made-up AtlasPlus and Census data for the offline pipeline tests
# (impute_suppressed(), no_county_cases(), extrapolate_from_county()). Everything
# is made up, so the right answers are known exactly.
#
# State 01 has two counties: 01001 with three tracts and 01003 with one (a
# very small county, like Loving County, TX).

fake_tracts <- data.frame(
  geoid = c("01001000100", "01001000200", "01001000300", "01003000100"),
  county_fips = c("01001", "01001", "01001", "01003")
)

census_variables <- c(
  sprintf("B01001_%03d", 1:49),
  sprintf("B03002_%03d", 1:21),
  sprintf(
    "B01001%s_%03d",
    rep(c("B", "C", "D", "E", "G", "H", "I"), each = 31),
    1:31
  )
)

# A fixed made-up population for every tract and Census variable. Each table
# total (B01001_001, B03002_001, B01001B_001 and so on) is the sum of its
# sex-by-age bands, as in real ACS data.
fake_acs <- function() {
  grid <- expand.grid(
    t = seq_len(nrow(fake_tracts)),
    v = seq_along(census_variables)
  )
  acs <- data.frame(
    GEOID = fake_tracts$geoid[grid$t],
    NAME = paste("Census Tract", fake_tracts$geoid[grid$t]),
    variable = census_variables[grid$v],
    estimate = (37 * grid$t + 13 * grid$v) %% 400 + 25,
    moe = 0
  )
  for (tract in fake_tracts$geoid) {
    in_tract <- acs$GEOID == tract
    total <- function(codes) {
      sum(acs$estimate[in_tract & acs$variable %in% codes])
    }
    all_bands <- total(sprintf("B01001_%03d", c(3:25, 27:49)))
    acs$estimate[in_tract & acs$variable == "B01001_001"] <- all_bands
    acs$estimate[in_tract & acs$variable == "B03002_001"] <- all_bands
    for (letter in c("B", "C", "D", "E", "G", "H", "I")) {
      code <- sprintf("B01001%s_001", letter)
      acs$estimate[in_tract & acs$variable == code] <-
        total(sprintf("B01001%s_%03d", letter, c(3:16, 18:31)))
    }
  }
  acs
}

# Each made-up tract split into two block groups (GEOID + "1" and + "2"), the
# second with 7 more people per variable than the first.
fake_block_groups <- function(acs = fake_acs()) {
  first <- acs
  first$GEOID <- paste0(acs$GEOID, "1")
  second <- acs
  second$GEOID <- paste0(acs$GEOID, "2")
  second$estimate <- second$estimate + 7
  out <- rbind(first, second)
  out$NAME <- paste("Block Group", out$GEOID)
  out
}

# One tract's made-up populations, named by variable.
fake_acs_tract <- function(geoid, acs = fake_acs()) {
  one <- acs[acs$GEOID == geoid, ]
  rlang::set_names(one$estimate, one$variable)
}

strata_for <- function(stratify_by, all_ages = "Ages 13 years and older") {
  all <- data.frame(
    race_ethnicity = "All races/ethnicities",
    sex = "Both sexes",
    age = all_ages
  )
  if (is.null(stratify_by)) {
    return(all)
  }
  values <- switch(
    stratify_by,
    sex = list(sex = c("Male", "Female")),
    race = list(race_ethnicity = c(
      "American Indian/Alaska Native", "Asian", "Black/African American",
      "Hispanic/Latino", "Multiracial",
      "Native Hawaiian/Other Pacific Islander", "White"
    )),
    age = list(age = c("13-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  )
  out <- all[rep(1, length(values[[1]])), ]
  out[[names(values)]] <- values[[1]]
  rownames(out) <- NULL
  out
}

# County rows as get_atlas() returns them (factors included). `suppressed`
# lists county FIPS codes whose counts CDC suppressed.
fake_county_atlas <- function(
  stratify_by = NULL,
  suppressed = character(0),
  indicators = c("HIV diagnoses", "HIV prevalence"),
  years = 2022
) {
  all_ages <- if (any(grepl("HIV", indicators))) {
    "Ages 13 years and older"
  } else {
    "All age groups"
  }
  strata <- strata_for(stratify_by, all_ages)
  grid <- expand.grid(
    county = c("01001", "01003"),
    stratum = seq_len(nrow(strata)),
    indicator = indicators,
    year = years,
    stringsAsFactors = FALSE
  )
  cases <- 10 * match(grid$county, c("01001", "01003")) + grid$stratum +
    100 * (match(grid$indicator, indicators) - 1) + (grid$year - min(years))
  is_suppressed <- grid$county %in% suppressed
  out <- data.frame(
    indicator = grid$indicator,
    year = grid$year,
    geography = paste("County", grid$county),
    data_status = ifelse(is_suppressed, "Data suppressed", "Not Suppressed"),
    strata[grid$stratum, ],
    transmission = "All transmission categories",
    rate100000 = NA_real_,
    cases = ifelse(is_suppressed, NA_real_, cases),
    population = 50000,
    fips = grid$county,
    row.names = NULL
  )
  factors <- c("indicator", "geography", "data_status", "race_ethnicity",
               "sex", "age", "transmission")
  out[factors] <- lapply(out[factors], factor)
  out$data_status <- factor(
    out$data_status,
    levels = c("Not Suppressed", "Data suppressed", "Data not available")
  )
  out
}

# State rows: the counties' true counts plus `no_county` extra cases, with a
# population of 1,000,000 (so the state rate is cases / 1e6).
fake_state_atlas <- function(no_county = 0, ...) {
  full <- fake_county_atlas(...)
  full[c("indicator", "race_ethnicity", "sex", "age")] <- lapply(
    full[c("indicator", "race_ethnicity", "sex", "age")],
    as.character
  )
  state <- stats::aggregate(
    cases ~ indicator + year + race_ethnicity + sex + age,
    data = full,
    FUN = sum
  )
  state$cases <- state$cases + no_county
  state$population <- 1e6
  state$geography <- "Alabama"
  state$data_status <- "Not Suppressed"
  state$fips <- "01"
  state
}

# Replace CDC and the Census with the made-up data for one test.
local_fake_sources <- function(
  state = fake_state_atlas(),
  acs = fake_acs(),
  env = parent.frame()
) {
  local_mocked_bindings(
    .request_atlas = function(payload) state,
    .check_census_ready = function(call) invisible(),
    .get_acs_areas = function(variables, year, states, to) {
      if (to == "block group") {
        acs <- fake_block_groups(acs)
      }
      tibble::as_tibble(acs[acs$variable %in% variables, ])
    },
    .env = env
  )
}

# County count and area sum for each county stratum, with the areas' GEOIDs.
county_vs_areas <- function(areas) {
  areas |>
    dplyr::summarize(
      county_values = dplyr::n_distinct(county_cases_allocated),
      county_cases = dplyr::first(county_cases_allocated),
      area_cases_sum = sum(area_cases),
      area_ids = list(sort(geoid)),
      .by = c(county_fips, year, indicator, race_ethnicity, sex, age)
    )
}

# Two states: state 02 is a copy of state 01 (counties 02001 and 02003, the
# same tracts) with twice the state population and 300 more cases with no
# county. County 01003 and 02003 are suppressed.
two_state_atlas <- function() {
  county_01 <- fake_county_atlas(suppressed = "01003")
  county_01$fips <- as.character(county_01$fips)
  county_02 <- county_01
  county_02$fips <- sub("^01", "02", county_02$fips)
  county_02$geography <- factor(paste("County", county_02$fips))

  state_01 <- fake_state_atlas(no_county = 0)
  state_02 <- fake_state_atlas(no_county = 300)
  state_02$fips <- "02"
  state_02$geography <- "Alaska"
  state_02$population <- 2e6

  acs_01 <- fake_acs()
  acs_02 <- acs_01
  acs_02$GEOID <- sub("^01", "02", acs_02$GEOID)

  list(
    county = rbind(county_01, county_02),
    state = rbind(state_01, state_02),
    acs = rbind(acs_01, acs_02)
  )
}

# County race data with 5 cases of race "Unknown" in each county and
# indicator.
county_with_unknown_race <- function() {
  county <- fake_county_atlas("race")
  unknown <- county[county$race_ethnicity == "White", ]
  unknown$race_ethnicity <- factor("Unknown")
  unknown$cases <- 5
  rbind(county, unknown)
}

# State rows for county_with_unknown_race(): 5 + 5 unknown per indicator.
state_with_unknown_race <- function() {
  state <- fake_state_atlas(stratify_by = "race")
  unknown <- state[state$race_ethnicity == "White", ]
  unknown$race_ethnicity <- "Unknown"
  unknown$cases <- 10
  rbind(state, unknown)
}
