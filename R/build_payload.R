#' Internal: temporary dummy payload builder
#' @keywords internal
.build_payload <- function(
  disease,
  geography,
  year,
  stratify_by = NULL,
  geo_ids = NULL
) {
  #Initial payload
  id <- NULL

  #Add each disease to payload
  if ("chlamydia" %in% disease) {
    id <- c(id, 206)
  }
  if ("gonorrhea" %in% disease) {
    id <- c(id, 207)
  }
  if ("adult syphilis" %in% disease) {
    id <- c(id, 208, 213, 290)
  }
  if ("congenital syphilis" %in% disease) {
    id <- c(id, 214)
  }
  if ("tuberculosis" %in% disease) {
    id <- c(id, 209)
  }
  if ("hiv" %in% disease) {
    id <- c(id, 203, 205)
  }
  if ("estimate" %in% disease) {
    id <- c(id, 285:287)
  }
  if ("hepatitis" %in% disease) {
    id <- c(id, 210:212)
  }

  #Add geography to payload.  Only allows one geography
  id <- switch(
    geography,
    "national" = c(id, 5000), #5000 is the United States; state codes start at 5001
    #CDC's region level (geoLevel 1005): the four Census regions (8453-8456) and
    #U.S. territories and freely associated states (8457)
    "region" = c(id, varvals$id[varvals$geoLevel %in% 1005]),
    "state" = c(id, lookup[["state"]]$geo_id),
    "county" = c(id, geo_ids %||% lookup[["counties"]]$geo_id),
    "msa" = c(id, lookup[["msa"]]$geo_id)
  )

  #Add year to payload.  The year id is the current year, subtracting 1478
  id <- c(id, year - 1478)

  #Add stratification variables to id
  #Add age: each disease's own age groups, or its age total
  id <- c(id, .atlas_age_codes(disease, "age" %in% stratify_by))

  #Add race stratification variables
  if ("race" %in% stratify_by) {
    id <- c(id, 552, 553, 554, 555, 562, 556, 559, 557)
  }
  if (!("race" %in% stratify_by)) {
    id <- c(id, 551)
  }

  #Add sex stratification variables
  if ("sex" %in% stratify_by) {
    id <- c(id, 602, 603)
  }
  if (!("sex" %in% stratify_by)) {
    id <- c(id, 601)
  }

  #Transmission category--defaults to all for now
  id <- c(id, 801)

  #Create payload
  list(
    VariableIDs = paste(id, collapse = ",")
  )
}


# Requests are split into one per disease and year, and county requests also
# by state, so each saved response covers one disease, one year and (for
# counties) one state. Then get_atlas(refresh = TRUE) and the pipeline's own
# requests always name the same saved pieces, whatever years or diseases they
# were asked for together (#77). A single request for every county can also
# exceed CDC's database timeout (seen for 2017 in October 2026), while one
# state's counties returns in a few seconds.
#
# `states`: for counties, the 2-digit state FIPS codes to request (all if
# NULL). County payloads are named by state code.
.build_payloads <- function(
  disease,
  geography,
  year,
  stratify_by = NULL,
  states = NULL
) {
  years <- sort(unique(as.numeric(year)))
  pieces <- lapply(unique(disease), function(one_disease) {
    lapply(years, function(one_year) {
      .build_year_payloads(
        one_disease,
        geography,
        one_year,
        stratify_by,
        states
      )
    })
  })
  unlist(unlist(pieces, recursive = FALSE), recursive = FALSE)
}

# The payloads for one disease and year: one, or one per state for counties.
.build_year_payloads <- function(
  disease,
  geography,
  year,
  stratify_by,
  states
) {
  if (geography != "county") {
    return(list(.build_payload(disease, geography, year, stratify_by)))
  }

  counties <- lookup[["counties"]]
  by_state <- split(counties$geo_id, substr(counties$fips, 1, 2))
  if (!is.null(states)) {
    by_state <- by_state[intersect(names(by_state), states)]
  }

  lapply(by_state, function(geo_ids) {
    .build_payload(disease, geography, year, stratify_by, geo_ids = geo_ids)
  })
}


# AtlasPlus age codes for one disease: its age groups when stratified by age,
# otherwise its total. From CDC's set-up data (`combo`, which lists the codes
# each indicator offers), checked by a live test. Each set of groups covers
# the disease's ages once: HIV groups start at 13; the others cover all ages,
# and chlamydia, gonorrhea and syphilis also have "Unknown" (673) (#10, #11).
# Congenital syphilis has no age groups.
.atlas_age_codes <- function(disease, by_age) {
  hiv <- c(656, 662, 665, 668, 669, 672)
  all_ages <- c(654, 657, 660, 661, 663, 664, 666, 668, 669, 672)
  if (!by_age) {
    return(if (disease %in% c("hiv", "estimate")) 650 else 651)
  }
  switch(
    disease,
    "hiv" = ,
    "estimate" = hiv,
    "chlamydia" = ,
    "gonorrhea" = ,
    "adult syphilis" = c(all_ages, 673),
    "tuberculosis" = ,
    "hepatitis" = all_ages,
    "congenital syphilis" = cli::cli_abort(
      "CDC has no age groups for congenital syphilis.",
      .internal = TRUE
    )
  )
}
