.fill_in_censored_count <- function(df_to_fill_in, payload_state) {

  df_state <- .request_atlas(payload_state) %>%
    dplyr::select(indicator, year, geography, race_ethnicity, sex, age, cases, fips) %>%
    dplyr::rename(
      state_fips = fips,
      state_name = geography,
      state_cases = cases
    )

  results_df <- df_to_fill_in %>%
    dplyr::mutate(
      state_fips = substr(county_fips, 1, 2),
      is_censored = is.na(county_cases)
    ) %>%
    dplyr::left_join(
      df_state,
      by = dplyr::join_by(indicator, year, race_ethnicity, sex, age, state_fips)
    ) %>%
    dplyr::group_by(state_fips, year, indicator, race_ethnicity, sex, age) %>%
    dplyr::mutate(
      state_cases_attributed = sum(county_cases, na.rm = TRUE),
      state_cases_missing = state_cases - state_cases_attributed,
      censored_pop_total = sum(dplyr::if_else(is_censored, county_population_atlas, 0), na.rm = TRUE),
      county_cases_filled = dplyr::case_when(
        !is_censored ~ county_cases,
        is_censored & censored_pop_total > 0 ~
          state_cases_missing * county_population_atlas / censored_pop_total,
        is_censored & censored_pop_total == 0 ~ 0
      )
    ) %>%
    dplyr::ungroup()

  results_df %>%
    dplyr::select(
      -censored_pop_total,
      -state_cases_missing,
      -state_cases_attributed,
      -is_censored,
      -county_cases
    ) %>%
    dplyr::rename(county_cases = county_cases_filled)
}


.census_variables_finder <- function(stratify_by, variables_available) {

  return_value_filter <- variable_lookup_atlas

  if (is.null(stratify_by)) {
    return_value_filter <- return_value_filter %>%
      dplyr::filter(sex == "Both sexes")%>%
      dplyr::filter(race_ethnicity == "All races/ethnicities")%>%
      dplyr::filter(age == "Ages 13 years and older")
  }

  if (!is.null(stratify_by) && "sex" %in% stratify_by) {
    return_value_filter <- return_value_filter %>%
      dplyr::filter(sex != "Both sexes")
  }

  if (!is.null(stratify_by) && "race" %in% stratify_by) {
    return_value_filter <- return_value_filter %>%
      dplyr::filter(race_ethnicity != "All races/ethnicities")
  }

  if (!is.null(stratify_by) && "age" %in% stratify_by) {
    return_value_filter <- return_value_filter %>%
      dplyr::filter(age != "Ages 13 years and older")
  }

  variables_available %>%
    dplyr::inner_join(return_value_filter) %>%
    dplyr::rename(variable = name)
}


.extrapolate_to_tract <- function(df, payload_state, stratify_by) {

  # Initialize lists
  Merged_counties <- list()
  Merged_tracts <- list()
  variables_list <- list()
  us_tracts_population <- list()
  us_counties_population <- list()

  county_atlas_data <- df %>%
    dplyr::rename(
      county_cases = cases,
      county_fips = fips,
      county_name = geography,
      county_population_atlas = population
    ) %>%
    dplyr::mutate(state_fips = substr(county_fips, 1, 2)) %>%
    .fill_in_censored_count(payload_state) %>%
    dplyr::mutate(year = as.character(year)) %>%
    tidyr::nest(data = -year) %>%
    tibble::deframe()

  # Define years and states for analysis
  year_list <- as.character(unique(df$year))
  state_vector <- unique(substr(df$fips, 1, 2))

  for (year_loop in year_list) {

    variables_available <- tidycensus::load_variables(as.numeric(year_loop), "acs5")

    variables_list[[year_loop]] <- .census_variables_finder(stratify_by, variables_available)

    us_tracts_population[[year_loop]] <- tidycensus::get_acs(
      geography = "tract",
      variables = variables_list[[year_loop]]$variable,
      year = as.numeric(year_loop),
      survey = "acs5",
      state = state_vector
    ) %>%
      dplyr::mutate(county_fips = substr(GEOID, 1, 5)) %>%
      dplyr::inner_join(variables_list[[year_loop]]) %>%
      dplyr::select(-variable, -moe, -geography, -label, -concept) %>%
      dplyr::rename(
        tract_fips = GEOID,
        tract_name = NAME
      )%>%
      group_by(tract_fips, tract_name,county_fips,race_ethnicity,sex,age )%>%
      summarize(tract_population_acs=sum(estimate, na.rm = TRUE))%>%
      ungroup()

    us_counties_population[[year_loop]] <- tidycensus::get_acs(
      geography = "county",
      variables = variables_list[[year_loop]]$variable,
      year = as.numeric(year_loop),
      survey = "acs5",
      state = state_vector
    ) %>%
      dplyr::inner_join(variables_list[[year_loop]]) %>%
      dplyr::select(-variable, -moe, -geography, -label, -concept) %>%
      dplyr::rename(
        county_fips = GEOID
      ) %>%
      dplyr::select(-NAME)%>%
      group_by(county_fips, race_ethnicity,sex,age )%>%
      summarize(county_population_acs=sum(estimate, na.rm = TRUE))%>%
      ungroup()

    Merged_counties[[year_loop]] <- dplyr::left_join(
      us_counties_population[[year_loop]],
      county_atlas_data[[year_loop]],
      by = dplyr::join_by(county_fips, race_ethnicity, sex, age)
    )

    Merged_tracts[[year_loop]] <- dplyr::left_join(
      us_tracts_population[[year_loop]],
      Merged_counties[[year_loop]],
      by = dplyr::join_by(county_fips, race_ethnicity, sex, age)
    ) %>%

      # Estimate census tract HIV population
      dplyr::mutate(
        tract_cases = dplyr::case_when(
          is.na(county_cases) | is.na(tract_population_acs) | is.na(county_population_acs) ~ NA_real_,
          county_population_acs == 0 & county_cases == 0 ~ 0,
          county_population_acs == 0 & county_cases > 0 ~ NA_real_,
          TRUE ~ county_cases * tract_population_acs / county_population_acs
        ))%>%

      # Estimate HIV-negative population in census tracts
      dplyr::mutate(tract_noncases = tract_population_acs - tract_cases)
  }

  dplyr::bind_rows(Merged_tracts, .id = "year") %>%
    dplyr::mutate(year = as.numeric(year))
}
