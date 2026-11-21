# a county download that fails in one state is not read as an unreleased year (#157)

    Code
      live_county_if_released(2025, "sex")
    Condition
      Error in `live_county_if_released()`:
      ! The county download for hiv 2025, by sex failed.
      x Failing request: Alaska (02).
      i CDC has state rows for HIV diagnoses and HIV prevalence in 2025, so the year is released and an earlier year is not used instead.
      Caused by error:
      ! CDC AtlasPlus returned no data for this request.

# live_county_if_released() stops when counties lack an indicator states have

    Code
      live_county_if_released(2025)
    Condition
      Error in `live_county_if_released()`:
      ! The county download for hiv 2025, unstratified has no rows for HIV prevalence.
      i CDC has state rows for it in 2025, so an earlier year is not used instead.

