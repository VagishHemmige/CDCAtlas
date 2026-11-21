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

