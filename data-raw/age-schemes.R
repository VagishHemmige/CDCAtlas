# Which age groups does AtlasPlus offer for each disease?
#
# For each disease and a few years, asks AtlasPlus for national data with every
# age-group code in the package's lookup (`varvals`), and records which codes
# come back with data. The result shows each disease's age scheme, which the
# package needs to request the right age groups and to map them to Census age
# bands (issues #10 and #11).
#
# Run from the package root, with internet access:
#   source("data-raw/age-schemes.R")
#
# Reads from CDC only: about 25 small national requests, or up to ~35 more
# per disease and year if CDC rejects a request with every age code. Writes
# data-raw/age-schemes.csv for review; nothing in the package changes.

devtools::load_all()
library(dplyr)

diseases <- c(
  "hiv", "estimate", "chlamydia", "gonorrhea", "adult syphilis",
  "congenital syphilis", "tuberculosis", "hepatitis"
)
years <- c(2010, 2017, 2022)

age_codes <- varvals |>
  filter(vtid == 7) |>
  select(age_id = id, age = name)

# Send one national request with the given age codes in place of the usual
# "all ages" code. Returns the parsed response, or the error.
request_ages <- function(disease, year, codes) {
  ids <- strsplit(.build_payload(disease, "national", year)$VariableIDs, ",")[[1]]
  position <- which(ids %in% c("650", "651"))
  ids <- append(ids[-position], as.character(codes), after = position - 1)
  result <- tryCatch(
    .request_atlas_internal(list(VariableIDs = paste(ids, collapse = ","))),
    error = function(e) e
  )
  Sys.sleep(0.5)
  result
}

# Which age codes return data for one disease and year. Asks for every code at
# once; if CDC rejects that, asks for each code separately.
probe <- function(disease, year) {
  result <- request_ages(disease, year, age_codes$age_id)
  problem <- NA_character_

  if (inherits(result, "error")) {
    problem <- paste("All codes at once failed:", conditionMessage(result))
    parts <- lapply(age_codes$age_id, function(code) request_ages(disease, year, code))
    ok <- !vapply(parts, inherits, logical(1), what = "error")
    if (!any(ok)) {
      return(tibble(disease = disease, year = year, problem = paste(problem, "Each code alone also failed.")))
    }
    result <- bind_rows(lapply(parts[ok], function(p) mutate(p, across(where(is.factor), as.character))))
  }

  result |>
    mutate(across(c(age, indicator, data_status), as.character)) |>
    summarize(
      rows = n(),
      rows_with_cases = sum(!is.na(cases)),
      cases = sum(cases, na.rm = TRUE),
      data_status = paste(sort(unique(data_status)), collapse = "; "),
      .by = c(indicator, age)
    ) |>
    left_join(age_codes, by = "age") |>
    mutate(disease = disease, year = year, problem = problem, .before = 1)
}

schemes <- tidyr::expand_grid(disease = diseases, year = years) |>
  purrr::pmap(probe, .progress = TRUE) |>
  bind_rows() |>
  as_tibble() |>
  arrange(disease, indicator, year, age_id)

readr::write_csv(schemes, "data-raw/age-schemes.csv", na = "")

# Summary: for each disease and indicator, the age groups with data. Years
# with the same groups are listed together.
summary_lines <- schemes |>
  filter(rows_with_cases > 0) |>
  summarize(age_groups = paste(age, collapse = ", "), .by = c(disease, indicator, year)) |>
  summarize(years = paste(year, collapse = ", "), .by = c(disease, indicator, age_groups))
cat(sprintf("%s | %s | %s\n    %s\n", summary_lines$disease, summary_lines$indicator,
            summary_lines$years, summary_lines$age_groups), sep = "")

# Requests that failed, if any.
problems <- schemes |> filter(!is.na(problem)) |> distinct(disease, year, problem)
if (nrow(problems) > 0) print(problems, n = Inf, width = Inf)
