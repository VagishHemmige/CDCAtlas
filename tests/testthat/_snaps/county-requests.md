# a failed request passes on CDC's message without the server trace

    Code
      get_atlas("hiv", "state", 2017)
    Message
      ! CDC AtlasPlus timed out. Trying again in 10 seconds (retry 1 of 2).
      ! CDC AtlasPlus timed out. Trying again in 30 seconds (retry 2 of 2).
    Condition
      Error:
      ! CDC AtlasPlus request failed (HTTP status 400).
      i CDC says: Execution Timeout Expired. The timeout period elapsed prior to completion of the operation or the server is not responding.
      i CDC's database timed out. This is usually temporary: try again in a few minutes.

# a failed request without a readable message still explains itself

    Code
      get_atlas("hiv", "state", 2017)
    Condition
      Error:
      ! CDC AtlasPlus request failed (HTTP status 404).
      i If this keeps happening, CDC may have changed AtlasPlus. Please report it at <https://github.com/VagishHemmige/CDCAtlas/issues>.

# a request that can't reach CDC says so

    Code
      get_atlas("hiv", "state", 2017)
    Condition
      Error:
      ! Couldn't reach CDC AtlasPlus.
      x Could not resolve host: gis.cdc.gov
      i Check your internet connection, or try again later; CDC's site is sometimes slow or down.

# an empty response says what may be wrong

    Code
      get_atlas("hiv", "state", 2017)
    Condition
      Error:
      ! CDC AtlasPlus returned no data for this request.
      i CDC may not have released this year for this disease yet, or may not report this combination of geography and strata.
      i Check what AtlasPlus offers at <https://gis.cdc.gov/grasp/nchhstpatlas/main.html>.

# a CDC timeout is retried twice, then fails

    Code
      get_atlas("hiv", "state", 2017)
    Message
      ! CDC AtlasPlus timed out. Trying again in 10 seconds (retry 1 of 2).
      ! CDC AtlasPlus timed out. Trying again in 30 seconds (retry 2 of 2).
    Condition
      Error:
      ! CDC AtlasPlus request failed (HTTP status 400).
      i CDC says: Execution Timeout Expired. The timeout period elapsed prior to completion of the operation or the server is not responding.
      i CDC's database timed out. This is usually temporary: try again in a few minutes.

