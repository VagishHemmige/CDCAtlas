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

# a state response with no data is not read as an unreleased year (#157)

    Code
      live_county_if_released(2025, "sex")
    Condition
      Error in `live_released_indicators()`:
      ! Couldn't establish whether CDC has released hiv 2025, by sex: the state request failed.
      i A failed request is not evidence that the year is unreleased, so an earlier year is not used instead.
      Caused by error:
      ! CDC AtlasPlus returned no data for this request.
      i CDC may not have released this year for this disease yet, or may not report this combination of geography and strata.
      i Check what AtlasPlus offers at <https://gis.cdc.gov/grasp/nchhstpatlas/main.html>.

---

    Code
      live_released_indicators(2025, "sex")
    Condition
      Error in `live_released_indicators()`:
      ! Couldn't establish whether CDC has released hiv 2025, by sex: the state request failed.
      i A failed request is not evidence that the year is unreleased, so an earlier year is not used instead.
      Caused by error:
      ! CDC AtlasPlus returned no data for this request.
      i CDC may not have released this year for this disease yet, or may not report this combination of geography and strata.
      i Check what AtlasPlus offers at <https://gis.cdc.gov/grasp/nchhstpatlas/main.html>.

# live_county_if_released() stops when counties lack an indicator states have

    Code
      live_county_if_released(2025)
    Condition
      Error in `live_county_if_released()`:
      ! The county download for hiv 2025, unstratified has no rows for HIV prevalence.
      i CDC has state rows for it in 2025, so an earlier year is not used instead.

# live_county_if_released() stops on any other failure of the state request

    Code
      live_county_if_released(2025)
    Condition
      Error in `live_released_indicators()`:
      ! Couldn't establish whether CDC has released hiv 2025, unstratified: the state request failed.
      i A failed request is not evidence that the year is unreleased, so an earlier year is not used instead.
      Caused by error:
      ! CDC is down

