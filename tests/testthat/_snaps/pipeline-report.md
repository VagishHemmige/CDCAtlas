# the report refuses limits that the imputation didn't use

    Code
      .pipeline_report(run$county, run$imputed, run$areas, cap = 4, floor = 0)
    Condition
      Error in `.pipeline_report()`:
      ! `cap`, `floor` and `population` don't reproduce 1 imputed count in `imputed`.
      i Give the values that were given to `impute_suppressed()`.

---

    Code
      .pipeline_report(run$imputed, run$imputed, run$areas)
    Condition
      Error in `.pipeline_report()`:
      ! `county` must be county data before `impute_suppressed()`.
      x It has a cases_source column.

---

    Code
      .pipeline_report(run$county, run$county, run$areas)
    Condition
      Error in `.pipeline_report()`:
      ! `imputed` has not been through `impute_suppressed()`.
      i Run `impute_suppressed(imputed)` first.

# the report refuses areas allocated from a different imputation

    Code
      .pipeline_report(county, default, uncapped_areas)
    Condition
      Error in `.pipeline_report()`:
      ! `areas` was not allocated from `imputed`.
      x imputed_cases in its no_county_cases attribute differs in 1 state stratum.
      i Give the `extrapolate_from_county()` result for these `imputed` rows.

---

    Code
      .pipeline_report(county, uncapped, default_areas, cap = Inf)
    Condition
      Error in `.pipeline_report()`:
      ! `areas` was not allocated from `imputed`.
      x imputed_cases in its no_county_cases attribute differs in 1 state stratum.
      i Give the `extrapolate_from_county()` result for these `imputed` rows.

---

    Code
      .pipeline_report(county, default, suppressMessages(suppressWarnings(
        extrapolate_from_county(default[default$indicator == "HIV prevalence", ]))))
    Condition
      Error in `.pipeline_report()`:
      ! `areas` was not allocated from `imputed`.
      x Its no_county_cases attribute covers different state strata (state, indicator, year and stratum).
      i Give the `extrapolate_from_county()` result for these `imputed` rows.

# the report refuses areas kept from before a population changed

    Code
      .pipeline_report(changed, changed_imputed, areas)
    Condition
      Error in `.pipeline_report()`:
      ! `areas` was not allocated from `imputed`.
      x county_population_atlas differs in 1 county row.
      i Give the `extrapolate_from_county()` result for these `imputed` rows.

# the report refuses allocations it can't account for yet

    Code
      .pipeline_report(county, imputed, redistributed)
    Condition
      Error in `.pipeline_report()`:
      ! The report can't account for cases of unknown race or age shared across the known groups yet.
      x `areas` was made with `unknown_race = "redistribute" or unknown_age = "redistribute"`.
      i Use the `extrapolate_from_county()` result with the default for that argument.

---

    Code
      .pipeline_report(data$county, imputed, spread)
    Condition
      Error in `.pipeline_report()`:
      ! The report can't account for a remainder spread across counties yet.
      x `areas` was made with `no_county = "spread"`.
      i Use the `extrapolate_from_county()` result with the default for that argument.

