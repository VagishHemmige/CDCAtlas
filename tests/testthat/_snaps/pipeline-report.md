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

