# the negative-residual warning gives counts only

    Code
      out <- .fit_county_total(x, seed, totals, "age")
    Condition
      Warning:
      In 2 groups of county, indicator and year, the published strata add up to more than the county's published total, so they were not fitted.
      i The stratified and unstratified county data may come from different CDC releases.

# the incomplete-strata warning gives counts only

    Code
      out <- .fit_county_total(x, seed, totals, "age")
    Condition
      Warning:
      In 2 groups of county, indicator and year, the strata are incomplete or inconsistent, so they were not fitted.
      i A requested stratum is missing, or a cell has a count but is marked "Data suppressed", or a cell has no count and isn't marked "Data suppressed".

