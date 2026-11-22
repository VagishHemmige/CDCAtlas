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

# a state response for another disease is not read as an unreleased year (#157)

    Code
      live_county_if_released(2025)
    Condition
      Error in `live_released_indicators()`:
      ! Couldn't establish whether CDC has released hiv 2025, unstratified: the state response has indicators that weren't asked for.
      x Returned: Chlamydia.
      x Asked for: HIV diagnoses, HIV prevalence.
      i The response doesn't match the request, so nothing is inferred from it and an earlier year is not used instead.

# a state response with another disease beside the one asked for stops too (#157)

    Code
      live_county_if_released(2025)
    Condition
      Error in `live_released_indicators()`:
      ! Couldn't establish whether CDC has released hiv 2025, unstratified: the state response has indicators that weren't asked for.
      x Returned: Chlamydia, HIV diagnoses.
      x Asked for: HIV diagnoses, HIV prevalence.
      i The response doesn't match the request, so nothing is inferred from it and an earlier year is not used instead.

# a state response with no rows is not read as an unreleased year (#157)

    Code
      live_county_if_released(2025)
    Condition
      Error in `live_released_indicators()`:
      ! Couldn't establish whether CDC has released hiv 2025, unstratified: the state response has none of the indicators asked for.
      x Asked for: HIV diagnoses, HIV prevalence.
      i The response has no rows for the request, so nothing is inferred from it and an earlier year is not used instead.

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

# describe_suppressed_strata() reports every count, one line per indicator

    Code
      writeLines(describe_suppressed_strata(counts, "made-up"))
    Output
      made-up, HIV diagnoses 2022 by age (3 strata). Counties in both responses: 2 (left out: 0 only stratified, 0 only unstratified); unstratified count published in 2, suppressed in 0, neither in 0. Of those with it published, suppressed stratum cells: none 1, exactly one 0, two or more 1; missing a stratum row 0; with a cell neither published nor suppressed 0. Every stratum published: 1 (strata sum equal to the unstratified count 1, less 0, more 0). Published strata alone exceed the unstratified count: 0.
      made-up, HIV prevalence 2022 by age (3 strata). Counties in both responses: 10 (left out: 1 only stratified, 1 only unstratified); unstratified count published in 8, suppressed in 1, neither in 1. Of those with it published, suppressed stratum cells: none 5, exactly one 1, two or more 2; missing a stratum row 1; with a cell neither published nor suppressed 1. Every stratum published: 3 (strata sum equal to the unstratified count 1, less 1, more 1). Published strata alone exceed the unstratified count: 2.

# count_suppressed_strata() stops rather than count a county twice

    Code
      count_suppressed_strata(fake$stratified, twice)
    Condition
      Error in `count_suppressed_strata()`:
      ! `unstratified` has 2 repeated county rows (same fips, indicator and year), so a county would be counted twice.
    Code
      count_suppressed_strata(cell_twice, fake$unstratified)
    Condition
      Error in `count_suppressed_strata()`:
      ! `stratified` has 3 repeated stratum rows (same fips, indicator, year and stratum), so a county would be counted twice.

# count_suppressed_strata() stops unless given one stratified and one unstratified response

    Code
      count_suppressed_strata(fake$unstratified, fake$unstratified)
    Condition
      Error in `count_suppressed_strata()`:
      ! `stratified` must be stratified by one of race, sex and age; it is not stratified.
    Code
      count_suppressed_strata(fake$stratified, fake$stratified)
    Condition
      Error in `count_suppressed_strata()`:
      ! `unstratified` must not be stratified; it is by age.
    Code
      count_suppressed_strata(by_sex_too, fake$unstratified)
    Condition
      Error in `count_suppressed_strata()`:
      ! `stratified` must be stratified by one of race, sex and age; it is by sex, age.
    Code
      count_suppressed_strata(fake$stratified[c("fips", "cases")], fake$unstratified)
    Condition
      Error in `count_suppressed_strata()`:
      ! `stratified` has no column indicator, year, data_status, race_ethnicity, sex, age.
    Code
      count_suppressed_strata(fake$stratified, fake$unstratified[c("fips", "age")])
    Condition
      Error in `count_suppressed_strata()`:
      ! `unstratified` has no column indicator, year, data_status, cases, race_ethnicity, sex.

