# the no-county total is announced

    Code
      invisible(.extrapolate_to_tract(fake_county_atlas(NULL), list(), NULL))
    Message
      i Cases in state totals with no county are not allocated to tracts:
      * HIV diagnoses 2022: 500
      * HIV prevalence 2022: 500
        See `attr(x, "no_county_cases")` for each state.

# get_atlas() rejects unknown fill_suppressed values

    Code
      get_atlas("hiv", "county", 2022, fill_suppressed = "mean")
    Condition
      Error in `get_atlas()`:
      ! `fill_suppressed` must be one of "state_rate" or "zero", not "mean".

